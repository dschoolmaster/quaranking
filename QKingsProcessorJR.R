####QKINGS ####

#clear out old stuff
rm(list=ls())
#setpath
setwd("~/Documents/RaceStuff/QKings/")
#getapp info
app_info<-jsonlite::read_json("app_info.json")
app_name <- app_info$app_name
app_client_id  <- app_info$app_client_id
app_secret <- app_info$app_secret


#get old scores from file
Q.df<-read.csv("Qdata.csv",stringsAsFactors = F)


#create empty athlete dictionary
ath.list<-list()
#read in the athlete data file
ath<-readLines(con<-file("~/Documents/RaceStuff/QKings/AthleteJsons.txt"))
close.connection(con)

for(j in 1:length(ath)){
#grab athlete
ans<-jsonlite::parse_json(ath[j])
#renew token
tokcall<-paste0("curl -X POST https://www.strava.com/api/v3/oauth/token"," -d client_id=",app_client_id,
                " -d client_secret=",app_secret," -d grant_type=refresh_token -d refresh_token=",ans$refresh_token," -o temp.json")
system(tokcall,ignore.stdout = T)
#update athlete file
tmp<-jsonlite::read_json("~/Documents/RaceStuff/QKings/temp.json")
ans$expires_at<-tmp$expires_at
ans$expires_in<-tmp$expires_in
ans$refresh_token<-tmp$refresh_token
ans$access_token<-tmp$access_token
#replace old info in athlete database
ath[j]<-jsonlite::toJSON(ans,auto_unbox=T)
#keep the info in athlete dictionary for further usage
ath.list[[j]]<-ans

#pull segment data
seg<-c(23304682,23304743,23253789,23276213)

  for(i in 1:length(seg)){
    datfile=paste0("Data/",unlist(ans$athlete$username),"_",seg[i],".json")
    getcall<-paste0('curl -X GET https://www.strava.com/api/v3/segments/',seg[i],'/all_efforts -d page=1 -d per_page=200 -H "Authorization: Bearer ', ans$access_token,'" -o ',datfile)
    system(getcall,ignore.stderr = T,wait = T)
    Sys.sleep(.1)
 }
}

#update Athlete JSONs in saved file
sink("AthleteJsons.txt",append = F)
cat(ath,sep ="\n")
sink()

####start processing ####
#grab ids from each athelete in dictionary
ids<-NULL
for(i in 1:length(ath.list))ids[i]<-unlist(ath.list[[i]]$athlete$id[[1]])

#update Q with new athletes 
if(length(ids[!ids%in%Q.df$Id])>0){
  Qlen<-dim(Q.df)[2]-1
  newQ<-data.frame(ids[!ids%in%Q.df$Id],matrix(0,nrow=sum(!ids%in%Q.df$Id),ncol = Qlen))
  colnames(newQ)<-colnames(Q.df)
  Q.df<-rbind(Q.df,newQ)
 }

#compile data
#get activity files
act.files<-dir("Data")
dat.list<-list()
#set current date range
start.date<-as.Date("2020-04-08")
end.date<-as.Date("2020-04-12")

for(i in 1:length(act.files)){
dat<-jsonlite::read_json(paste0("~/Documents/RaceStuff/QKings/Data/",act.files[i]))
if(length(dat)!=0){
len<-length(dat);len
dts<-as.Date(sapply(dat,"[[","start_date"))
pick<-which(dts>=start.date&dts<=end.date)
if(length(pick)>0)dat.list[[i]]<-data.frame(id=as.character(dat[[1]]$athlete$id),segment=dat[[1]]$name,count=length(pick),min.time=min(sapply(dat,"[[","moving_time")[pick]))
  }
}

#combine all efforts into single dataframe
df<-do.call("rbind",dat.list)

#convert effort counts and best times from long to wide form for easier processing
df.count <-reshape2::dcast(df, id ~ segment, value.var="count",fill=0)
rownames(df.count)<-df.count$id
df.count<-df.count[,-1]
df.time<-reshape2::dcast(df, id ~ segment, value.var="min.time")
rownames(df.time)<-df.time$id
df.time<-df.time[,-1]

#create functions to calculate Q
calc.H<-function(L)exp(-sum(L/sum(L)*log((L+1)/sum(L+1))))/length(seg)
t.min<-apply(df.time,2,min,na.rm=T)
#these are for sniff tests
#Z_part<-(1-sweep(df.time,MARGIN = 2,STATS = t.min,FUN = "-")/df.time)*df.count
#Entropy_adj<-apply(df.count,1,calc.H)

#get Q
Q<-apply((1-sweep(df.time,MARGIN = 2,STATS = t.min,FUN = "-")/df.time)*df.count,1,sum,na.rm=T)*apply(df.count,1,calc.H)

#update Q_dat
Q.current<-data.frame(Id=names(Q),Q=Q)
colnames(Q.current)[2]<-paste0("Q_","current")

#on Sunday night change this to F to add column instead of overwrite it
replace.current<-F

if(replace.current){
  Q.df<-Q.df[,-(dim(Q.df)[2])]
Q.df<-merge(Q.df,Q.current,by = "Id",all.x = T)
Q.df[is.na(Q.df)]<-0
} else {
  colnames(Q.df)[dim(Q.df)[2]]<-paste0('Q_',Sys.Date())
  Q.df<-merge(Q.df,Q.current,by = "Id",all.x = T)
  Q.df[is.na(Q.df)]<-0
  Q.df$Q_current<-rep(0,dim(Q.df)[1])
}
Q.df
#save the Q_file
write.csv(Q.df,"Qdata.csv",row.names = F)
  
#get it in form for further processing for webpage
rownames(Q.df)<-Q.df[,"Id"]  
Q.df<-Q.df[,-1]
Q.now<-Q.df[,dim(Q.df)[2]]
Q.tot<-apply(Q.df,1,sum)
Q.lastweek<-Q.df[,(dim(Q.df)[2]-1)]

#fill in any segment not run at all
seg.names<-c("Girard Park stroller meetup route","QK - Freetown Tour", "QK - Reds HorseFarm Loop","QK - Moore Field Perimeter")
seg.names%in%colnames(df.count)

#collect the current counts and times for each athlete for webpage
if(!all(seg.names%in%colnames(df.count))){
  mt.count<-matrix(0,nrow=dim(df.count)[1],ncol=sum(!seg.names%in%colnames(df.count)),
           dimnames = list(rownames(df.count),seg.names[!seg.names%in%colnames(df.count)]))
  mt.time<-matrix(NA,nrow=dim(df.count)[1],ncol=sum(!seg.names%in%colnames(df.count)),
                 dimnames = list(rownames(df.count),seg.names[!seg.names%in%colnames(df.count)]))
    df.count<-data.frame(df.count,mt.count)
    df.time<-data.frame(df.time,mt.time)
}
#put segment in correct order
df.count<-df.count[,c(2,3,1,4)]
df.time<-df.time[,c(2,3,1,4)]
#get order of athlist

ord<-order(Q.tot,decreasing = T)

####make webpage####
#load silly stuff#
clr<-readLines(con<-file("~/Documents/RaceStuff/QKings/Colors.txt"))
close.connection(con)
ani<-readLines(con<-file("~/Documents/RaceStuff/QKings/Animals.txt"))
close.connection(con)

#write the results.html file
sink("results.html")

cat("<style>
    table {
    font-family: arial, sans-serif;
    border-collapse: collapse;
    width: 100%;
    }
    
    td, th {
    border: 1px solid #dddddd;
    text-align: left;
    padding: 8px;
    }
    
    tr:nth-child(even) {
    background-color: #dddddd;
    }
    </style>",sep="\n")
cat("<html>", sep = "\n")
cat("<body>", sep = "\n")
cat("<a name='top'></a>") 
cat("<title>QuaranKing Results</title>")
cat("<h1>Overall Results</h1>", sep="\n")
cat("<p>click on athlete photo to go to profile</p>")
#create main table
cat("<table>", sep = "\n")
cat(" <tr>", sep = "\n")
cat("<th>Place</th>", sep = "\n")
cat("<th></th>", sep = "\n")
cat("<th>Name</th>", sep = "\n")
cat("<th>Q This Week</th>", sep = "\n")
cat("<th>Q Last Week</th>", sep = "\n")
cat("<th>Overall Q</th>", sep = "\n")
cat("</tr>", sep = "\n")
for(i in 1:length(ord)){
  cat("<tr>", sep = "\n")
  cat("<td>",i,"</td>")
  #add anchor with username here
  cat("<td><a href='#",paste(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$username)),"'>",sep='')
  cat("<img src='",ifelse(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$profile_medium)=="avatar/athlete/medium.png",
                          "https://www.sciencekids.co.nz/images/pictures/animals/lemur.jpg",unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$profile_medium)),"' height='120' width='120'>")
  cat("</td></a>")
  cat("<td>",paste(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$firstname),
                   unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$lastname),sep = "<br>"),"</td>")
  cat("<td>",round(Q.now[ord[i]],2),"</td>")
  cat("<td>",round(Q.lastweek[ord[i]],2),"</td>")
  cat("<td>",round(Q.tot[ord[i]],2),"</td>")
  cat("\n")
  cat("</tr>", sep = "\n")
}
cat("</table>")
#create queens table
cat("\n")
cat("<h1>QuaranQueens</h1>", sep="\n")
cat("<table>", sep = "\n")
cat(" <tr>", sep = "\n")
cat("<th>Place</th>", sep = "\n")
cat("<th></th>", sep = "\n")
cat("<th>Name</th>", sep = "\n")
cat("<th>Q This Week</th>", sep = "\n")
cat("<th>Q Last Week</th>", sep = "\n")
cat("<th>Overall Q</th>", sep = "\n")
cat("</tr>", sep = "\n")
k=1;
for(i in 1:length(ord)){
  if(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$sex)=="F"){
    cat("<tr>", sep = "\n")
    cat("<td>",k,"</td>")
    cat("<td><a href='#",paste(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$username)),"'>",sep='')
    cat("<img src='",ifelse(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$profile_medium)=="avatar/athlete/medium.png","https://www.sciencekids.co.nz/images/pictures/animals/lemur.jpg",unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$profile_medium)),"' height='120' width='120'>")
    cat("</td></a>")
    cat("<td>",paste(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$firstname),
                     unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$lastname),sep = "<br>"),"</td>")
    cat("<td>",round(Q.now[ord[i]],2),"</td>")
    cat("<td>",round(Q.lastweek[ord[i]],2),"</td>")
    cat("<td>",round(Q.tot[ord[i]],2),"</td>")
    cat("\n")
    cat("</tr>", sep = "\n")
    k=k+1}
}
cat("</table>")
#create kings table
cat("\n")
cat("<h1>QuaranKings</h1>", sep="\n")
cat("<table>", sep = "\n")
cat(" <tr>", sep = "\n")
cat("<th>Place</th>", sep = "\n")
cat("<th></th>", sep = "\n")
cat("<th>Name</th>", sep = "\n")
cat("<th>Q This Week</th>", sep = "\n")
cat("<th>Q Last Week</th>", sep = "\n")
cat("<th>Overall Q</th>", sep = "\n")
cat("</tr>", sep = "\n")
k=1;
for(i in 1:length(ord)){
  if(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$sex)=="M"){
    cat("<tr>", sep = "\n")
    cat("<td>",k,"</td>")
    cat("<td><a href='#",paste(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$username)),"'>",sep='')
    cat("<img src='",ifelse(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$profile_medium)=="avatar/athlete/medium.png",
                            "https://www.sciencekids.co.nz/images/pictures/animals/lemur.jpg",unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$profile_medium)),"' height='120' width='120'>")
    cat("</td></a>")
    cat("<td>",paste(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$firstname),
                     unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$lastname),sep = "<br>"),"</td>")
    cat("<td>",round(Q.now[ord[i]],2),"</td>")
    cat("<td>",round(Q.lastweek[ord[i]],2),"</td>")
    cat("<td>",round(Q.tot[ord[i]],2),"</td>")
    cat("\n")
    cat("</tr>", sep = "\n")
    k=k+1}
}
cat("</table>")
#create atlete profiles
cat("\n")
cat("<h1>This Week's Stats</h1>")
for(i in 1:length(ord)){
  cat("<a href='#top'>Back to top of page</a>")
  cat("<a name='",paste(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$username)),"'></a>",sep='')
  cat("<h2>",paste(unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$firstname),
                   unlist(ath.list[[which(ids==names(Q.tot)[ord[i]])]]$athlete$lastname),sep = "<br>"),"</h2>")
  cat("<a>",paste("Favorite Crayola Color:",sample(clr,1)),"</a>")
  cat("<br>")
  cat("<a>",paste("Spirit Animal:",sample(ani,1)),"</a>")
  cat("<table>", sep = "\n")
  cat(" <tr>", sep = "\n")
  cat("<th></th>", sep = "\n")
  cat("<th>Segement #1</th>", sep = "\n")
  cat("<th>Segement #2</th>", sep = "\n")
  cat("<th>Segement #3</th>", sep = "\n")
  cat("<th>Segement #4</th>", sep = "\n")
  cat("</tr>", sep = "\n")
  cat("<tr>", sep = "\n")
  cat("<td>Effort Count</td>")
  for(j in 1:4)cat("<td align='center'>",df.count[names(Q.tot)[ord[i]],j],"</td>")
  cat("</tr>", sep = "\n")
  cat("<tr>", sep = "\n")
  cat("<td>Best Time (min)</td>")
  for(j in 1:4)cat("<td align='center'>",round(df.time[names(Q.tot)[ord[i]],j]/60,2),"</td>")
  cat("</tr>", sep = "\n")
  cat("</table>")
}
cat("</body></html>")
sink() 
#update github repository so google sites can pull it
system("git add results.html")
system(paste0("git commit -m 'results update ",Sys.time(),"'"))
system("git push origin master")
