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
                 col.names=paste0("V", seq_len(30)), fill=TRUE)
  df$Subject<-sapply(strsplit(x,'-',fixed=T), '[',2);
  df$DateofVisit<-sapply(strsplit(sapply(strsplit(x,'-',fixed=T), '[',3), "T", fixed=T), '[', 1);
  return(df);
  })

#bind all file data
stoplight.alldata<-rbind.fill(stop.datalist)

#exclude lines of dashes or subject ID 
stoplight.alldata<-subset(stoplight.alldata, V2=="1")

#Write table
write.table(stoplight.alldata, file="stoplight.csv", sep=",")

##WOOT WOOT We have all subj info raw merged with ID and date

#Now lets make summary variables per subject per time point
