#################################
##### import_expom_RF       #####
##### By: Marloes Eeftens   #####
##### Last edit: 14/08/2017 #####
#################################

#Function "import_expom_RF":
import_expom_RF=function(filename,prefix="",suffix=""){

  #0) Set defaults & parameters:
  filetype<-NA
  if(substr(filename,nchar(filename)-3,nchar(filename))==".csv"){filetype<-"csv"}
  if(substr(filename,nchar(filename)-3,nchar(filename))==".xls"){filetype<-"xls"}
  if(substr(filename,nchar(filename)-4,nchar(filename))==".xlsx"){filetype<-"xlsx"}
  if(is.na(filetype)){stop(cat("File extension not recognized. Please provide the extension as part of the filename as follows: filename=",'"',"U:/my_folder/my_expom_file.csv",'"',".",sep=""))}
  old_names_xlsx<-c("Date and Time","Sequence number","FM Radio","TV","Mobile 800 MHz Downlink",
               "Mobile 800 MHz Uplink","Mobile 900 MHz Uplink","Mobile 900 MHz Downlink","Mobile 1.8 GHz Uplink",
               "Mobile 1.8 GHz Downlink","DECT","Mobile 2.1 GHz Uplink","Mobile 2.1 GHz Downlink",
               "ISM 2.4 GHz","Mobile 2.6 GHz Uplink","Mobile 2.6 GHz Downlink","Mobile 3.5 GHz","WiFi 5 GHz",
               "Total","Overload","GPS Fix","GPS Lat","GPS Lon","GPS Altitude","GPS HDOP","GPS# Satellites",
               "GPS Speed","Marker","Battery charge %","USB cable")
  old_band_names<-old_names_xlsx[3:19]
  new_band_names<-paste0(prefix,c("FM","TV","DL800","UL800","UL900","DL900","UL1800","DL1800","DECT",
               "UL2100","DL2100","WIFI2G","UL2600","DL2600","WIMAX","WIFI5G","TOTAL"),suffix)
  new_other_names<-c("id","latitude","longitude","speed","nr_sats","marker","charge","charging","overload")
  separators<-c(",","\t",";")
  timeformats<-c("%m/%d/%Y %H:%M:%S","%Y-%m-%d %H:%M:%S","%d.%m.%Y %H:%M:%S","%Y/%m/%d %H:%M:%S","%d/%m/%Y %H:%M:%S","%d.%m.%Y %H:%M:%S")

  #1) Read the file depending on filetype:
  if(filetype %in% c("xls","xlsx")){
    dat<-as.data.frame(read_excel(filename,sheet="ExpoM RF Measurement",skip=1,col_names=TRUE))
    #Add Expom id to the dataset as column, not text:
    dat$id<-as.numeric(colnames(read_excel(filename,sheet="ExpoM RF Measurement",range="B1",skip=0)))
    }
  if(filetype=="csv"){
    try(dat<-read.csv(file=filename,header=FALSE,skip=2,sep=separators[1]),silent=TRUE)
    try(dat$id<-as.numeric(gsub("\\D","",read.csv(file=filename,header=FALSE,nrows=1,sep=separators[1])[1,1])))
    if(dim(dat)[2]<3){
      try(dat<-read.csv(file=filename,header=FALSE,skip=2,sep=separators[2]),silent=TRUE)
      try(dat$id<-as.numeric(gsub("\\D","",read.csv(file=filename,header=FALSE,nrows=1,sep=separators[2])[1,1])))
      }
    if(dim(dat)[2]<3){
      try(dat<-read.csv(file=filename,header=FALSE,skip=2,sep=separators[3]),silent=TRUE)
      try(dat$id<-as.numeric(gsub("\\D","",read.csv(file=filename,header=FALSE,nrows=1,sep=separators[3])[1,1])))
      }
    #Match the variable names to those of xls and xlsx filetype:
    names(dat)<-c(old_names_xlsx,"id")
    }

  #2) Format coordinates (x,y)
  dat[["GPS Lat"]]<-as.character(dat[["GPS Lat"]])
  dat[["GPS Lon"]]<-as.character(dat[["GPS Lon"]])
  degrees_X<-as.numeric(substr(sapply(X=dat[["GPS Lat"]],FUN=function(s)strsplit(s,split="[.]")[[1]][1]),1,2))
  degrees_Y<-as.numeric(substr(sapply(X=dat[["GPS Lon"]],FUN=function(s)strsplit(s,split="[.]")[[1]][1]),1,3))
  minutes_X<-as.numeric(substr(sapply(X=dat[["GPS Lat"]],FUN=function(s)strsplit(s,split="[.]")[[1]][1]),3,4))/60
  minutes_Y<-as.numeric(substr(sapply(X=dat[["GPS Lon"]],FUN=function(s)strsplit(s,split="[.]")[[1]][1]),4,5))/60
  seconds_X<-as.numeric(paste0("0.",gsub("\\D","",sapply(X=dat[["GPS Lat"]],FUN=function(s)strsplit(s,split="[.]")[[1]][2])),sep=""))/60
  seconds_Y<-as.numeric(paste0("0.",gsub("\\D","",sapply(X=dat[["GPS Lon"]],FUN=function(s)strsplit(s,split="[.]")[[1]][2])),sep=""))/60
  dat$latitude<-degrees_X+minutes_X+seconds_X
  dat$longitude<-degrees_Y+minutes_Y+seconds_Y
  dat$speed<-as.numeric(dat[["GPS Speed"]])
  #If the coordinate's latitude is "S" or longitude is "W" then *-1:
  dat$latitude[grepl("S",dat$latitude)==TRUE]<-dat$latitude*-1
  dat$longitude[grepl("W",dat$longitude)==TRUE]<-dat$longitude*-1
  #If the coordinate is "0000.0000X" and "00000.0000Y", it is missing:
  dat$latitude[dat[["GPS Lat"]]=="0000.0000X"&dat[["GPS Lon"]]=="00000.0000Y"]<-NA
  dat$longitude[dat[["GPS Lat"]]=="0000.0000X"&dat[["GPS Lon"]]=="00000.0000Y"]<-NA
  dat$speed[dat[["GPS Lat"]]=="0000.0000X"&dat[["GPS Lon"]]=="00000.0000Y"]<-NA
  dat[["GPS Lat"]]<-NULL
  dat[["GPS Lon"]]<-NULL
  dat[["GPS Speed"]]<-NULL

  #3) Convert to numeric: number of satellites that GPS signal is based on, marker, battery charge, USB cable:
  dat$nr_sats<-as.numeric(dat[["GPS# Satellites"]])
  dat$marker<-as.numeric(dat[["Marker"]])
  dat$charge<-as.numeric(gsub("%","",as.character(dat[["Battery charge %"]])))
  dat$charge<-ifelse(all(dat$charge<=1),dat$charge<-dat$charge*100,dat$charge<-dat$charge) #In case the "%" makes everything <1.
  dat$charging<-ifelse(dat[["USB cable"]]=="yes",TRUE,FALSE)
  dat$overload<-ifelse(dat[["Overload"]]%in%c(NA,0,"","No"),FALSE,TRUE)
  dat[["GPS# Satellites"]]<-NULL
  dat[["Marker"]]<-NULL
  dat[["Battery charge %"]]<-NULL
  dat[["USB cable"]]<-NULL
  dat[["Overload"]]<-NULL

  #4) Change names of the frequency bands and other variables to easier ones:
  names(dat)[names(dat) %in% old_band_names]<-new_band_names
  if(!prefix==""){names(dat)[names(dat) %in% new_other_names]<-paste0(prefix,new_other_names)}

  #5) If the original was an .xlsx file:
  if(filetype %in% c("xls","xlsx")){
    dat[,new_band_names]<-lapply(dat[,new_band_names],function(x) round(x,digits=4))
    }

  #6) Set some unused variables to null:
  dat[["GPS Fix"]]<-NULL
  dat[["GPS HDOP"]]<-NULL
  dat[["GPS Altitude"]]<-NULL
  dat[["Sequence number"]]<-NULL

  #7) Format timestamp:
  PosixTime<-as.POSIXct(strptime(dat[["Date and Time"]],format=timeformats[1])) #e.g. 2016-10-13 12:55:28
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(strptime(dat[["Date and Time"]],format=timeformats[2]))}
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(strptime(dat[["Date and Time"]],format=timeformats[3]))}
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(strptime(dat[["Date and Time"]],format=timeformats[4]))}
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(strptime(dat[["Date and Time"]],format=timeformats[5]))}
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(strptime(dat[["Date and Time"]],format=timeformats[6]))}
  if(any(is.na(PosixTime))){stop("Time format not recognized...")}
  dat[["Date and Time"]]<-NULL
  dat<-cbind(PosixTime,dat)

  #8) Return the resulting R dataframe:
  return(dat)
}
