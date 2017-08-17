#################################
##### import_vest           #####
##### By: Marloes Eeftens   #####
##### Last edit: 17/08/2017 #####
#################################

#Function "import_vest":
import_vest=function(filename,prefix,suffix){

  #0) Set defaults & parameters:
  if(missing(prefix)){prefix<-""}
  if(missing(suffix)){suffix<-""}
  old_band_names<-c("X800MHz","X900MHz_UL","X900MHz_DL","X1800MHz_UL","X1800MHz_DL","DECT",
                    "X2100MHz_UL","X2100MHz_DL","WIFI_2G","X2600MHz","WIFI_5G")
  new_band_names<-paste0(prefix,c("DL800","UL900","DL900","UL1800","DL1800",
                                  "DECT","UL2100","DL2100","WIFI2G","DL2600",
                                  "WIFI5G"),suffix)
  separators<-c(",","\t",";")
  timeformats<-c("%B-%d-%Y %H:%M:%S","%m/%d/%Y %H:%M:%S","%Y-%m-%d %H:%M:%S","%d.%m.%Y %H:%M:%S")

  #1) Read the file depending on filetype:
  try(dat<-read.csv(file=filename,header=FALSE,skip=1,sep=separators[1]),silent=TRUE)
  if(dim(dat)[2]<2){
    try(dat<-read.csv(file=filename,header=FALSE,skip=1,sep=separators[2]),silent=TRUE)
    }
  if(dim(dat)[2]<2){
    try(dat<-read.csv(file=filename,header=FALSE,skip=1,sep=separators[3]),silent=TRUE)
    }

  #2) Change names of the frequency bands and other variables to easier ones:
  names(dat)<-c("timestamp",new_band_names)

  #3) Format timestamp:
  #Sys.setlocale(category="LC_TIME",locale="en_GB.UTF-8")
  PosixTime<-as.POSIXct(strptime(dat[["timestamp"]],format=timeformats[1])) #e.g. October-13-2016 12:54:23
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(strptime(dat[["Date and Time"]],format=timeformats[2]))}
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(strptime(dat[["Date and Time"]],format=timeformats[3]))}
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(strptime(dat[["Date and Time"]],format=timeformats[4]))}
  if(any(is.na(PosixTime))){stop("Time format not recognized...")}
  dat[["timestamp"]]<-NULL
  dat<-cbind(PosixTime,dat)

  #4) Return the resulting R dataframe:
  return(dat)
}
