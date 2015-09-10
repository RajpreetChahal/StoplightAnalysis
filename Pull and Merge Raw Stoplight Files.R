#Pull Stoplight raw (unformatted data) and merge
library(plyr)
library(stringr)

#Set file path and glob files
stoplight.path<-file.path('C:/Users/rchahal/Desktop/PGS Resting State/Stoplight/Unformatted/')
stop.data<-file.path(stoplight.path, 'sid-*.csv')
stop.files<-Sys.glob(stop.data)

#To identify reg expr of ID
##b<-c("sid-95127-20150825T111111")
##sapply(strsplit(stop.files, '-', fixed=T),'[', 2)

#To identify reg expr of Date
##sample<-sapply(strsplit(stop.files, '-', fixed=T),'[', 3)
##sample2<-sapply(strsplit(sample, "T", fixed=T), '[', 1)

#Combined is:
##sample2<-sapply(strsplit(sapply(strsplit(stop.files,'-', fixed=T), '[',3), "T", fixed=T), '[', 1)


#read files from file names and extract subject ID
stop.datalist<-lapply(stop.files, function(x) {
  df<-read.table(x, header=FALSE, sep=",", 
                 col.names=paste0("V", seq_len(30)), colClasses="factor",fill=TRUE)
  df$Subject<-sapply(strsplit(x,'-',fixed=T), '[',2);
  df$DateofVisit<-sapply(strsplit(sapply(strsplit(x,'-',fixed=T), '[',3), "T", fixed=T), '[', 1);
  return(df);
  })
#Added colClasses="factor" to coerce all to factors. Before was leaving
#as logic and disrupting how the events showed up.


#bind all file data
stoplight.alldata<-rbind.fill(stop.datalist)

#exclude lines of dashes or subject ID 
stoplight.alldata<-subset(stoplight.alldata, V2=="1")

#convert wonky date format to proper one
stoplight.alldata$DateofVisit<-as.Date(as.character(stoplight.alldata$DateofVisit), format="%Y%m%d")

#Rename variables
stoplight.alldata<-rename(stoplight.alldata,c('V1'='MS',
                                              'V2'='N',
                                              'V3'='Round',
                                              'V4'='Time0',
                                              'V5'='Frame0',
                                              'V6'='Event0',
                                              'V7'='Time1',
                                              'V8'='Frame1',
                                              'V9'='Event1',
                                              'V10'='Time2',
                                              'V11'='Frame2',
                                              'V12'='Event2',
                                              'V13'='Time3',
                                              'V14'='Frame3',
                                              'V15'='Event3',
                                              'V16'='Time4',
                                              'V17'='Frame4',
                                              'V18'='Event4',
                                              'V19'='Time5',
                                              'V20'='Frame5',
                                              'V21'='Event5',
                                              'V22'='Time6',
                                              'V23'='Frame6',
                                              'V24'='Event6'))

#Set multiple variables to factors

events<-which(names(stoplight.alldata)%in%c("Event0", "Event1",
                                          "Event2", "Event3",
                                          "Event4", "Event5",
                                          "Event6"))
stoplight.alldata[,events]<-lapply(stoplight.alldata[,events],as.factor)

str(stoplight.alldata[,events])

#Create column indicating that this is wave 1 (7)

stoplight.alldata$TimePoint<-1

#Create new column with ID and visit so we know when they came in
##but really this should be done when we already have index scores

library(tidyr)
#first copy subject col and time point col since they delete when
#merged

stoplight.alldata$ID<-stoplight.alldata$Subject
stoplight.alldata<-stoplight.alldata %>% unite(SubTP, ID, TimePoint, sep=".")

#Write table
write.csv(stoplight.alldata,file='stoplightraw_TP1.csv')


#now redo all above steps for second time point.
#extract summary scores and merge dataframes 
#redo glm to see differences between scores
#use the difference between scores to create an index of change




##WOOT WOOT We have all subj info raw merged with ID and date



#Now let's merge subject date of birth to get age at time point
###NEED DOB
df <- merge(stoplight.alldata,dob,by.x='Subject',by.y='ID',all.x = TRUE)
df$Age <- as.numeric((as.Date(df$DateOfBirth) - df$DateofVisit)/365.25)
df$DateOfBirth <- as.character(df$DateOfBirth)
