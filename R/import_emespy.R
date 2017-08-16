#################################
##### import_emespy         #####
##### By: Marloes Eeftens   #####
##### Last edit: 15/08/2017 #####
#################################

#Function "import_emespy":
import_emespy=function(filename,prefix,suffix){

  #0) Set defaults & parameters:
  if(substr(filename,nchar(filename)-3,nchar(filename))!=".xls"){stop(cat("File extension not recognized, this function was built for .xls files. Please provide the extension as part of the filename as follows: filename=",'"',"U:/my_folder/my_expom_file.csv",'"',"."))}
  if(missing(prefix)){prefix<-""}
  if(missing(suffix)){suffix<-""}
  old_names<-c("Sample","Date","Time","Battery","FM","TV3","TETRA I","TETRA II","TETRA III",
               "TV4&5","LTE 800 (DL)","LTE 800 (UL)","GSM + UMTS 900(UL)","GSM + UMTS 900(DL)",                    "GSM 1800 (UL)",
               "GSM 1800 (DL)","DECT","UMTS 2100 (UL)","UMTS 2100 (DL)","WIFI 2G","LTE 2600 (UL)",
               "LTE 2600 (DL)","WIMax","WIFI 5G","TOTAL","Marks")
  old_band_names<-old_names[5:25]
  new_band_names<-paste0(prefix,c("FM","TV3","TETRA1","TETRA2","TETRA3","TV","DL800","UL800",
                                  "UL900","DL900","UL1800","DL1800","DECT","UL2100","DL2100","WIFI2G",
                                  "UL2600","DL2600","WIMAX","WIFI5G","TOTAL"),suffix)
  new_other_names<-c("id","marker","charge")
  timeformats<-c("%Y-%m-%d %H:%M:%S","%Y/%m/%d %H:%M:%S","%d.%m.%Y %H:%M:%S","%Y/%m/%d %H:%M:%S","%d/%m/%Y %H:%M:%S","%d.%m.%Y %H:%M:%S")

  #1) Read the file:
  firstline<-read_excel(filename,sheet="V.m-1",range="B1:C1",col_names=FALSE)
  dat<-read_excel(filename,sheet="V.m-1",skip=4,col_names=TRUE)
  dat<-as.data.frame(dat[-1,])
  dat$id<-as.character(firstline[1,2])

  #2) Convert to numeric: number of satellites that GPS signal is based on, marker, battery charge, USB cable:
  dat$marker<-as.numeric(dat[["Marks"]])
  dat$charge<-as.numeric(dat[["Battery"]])
  dat[["Marks"]]<-NULL
  dat[["Battery"]]<-NULL

  #3) Change names of the frequency bands and other variables to easier ones:
  names(dat)[names(dat) %in% old_band_names]<-new_band_names
  if(!prefix==""){names(dat)[names(dat) %in% new_other_names]<-paste0(prefix,new_other_names)}

  #4) Set some unused variables to null:
  dat[["Sample"]]<-NULL

  #5) Format timestamp:
  PosixTime<-as.POSIXct(paste(dat$Date,chron::times(as.numeric(dat$Time)),sep=" "),format=timeformats[1]) #e.g. 2016-10-13 12:55:28
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(paste(dat$Date,chron::times(as.numeric(dat$Time)),sep=" "),format=timeformats[2])}
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(paste(dat$Date,chron::times(as.numeric(dat$Time)),sep=" "),format=timeformats[3])}
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(paste(dat$Date,chron::times(as.numeric(dat$Time)),sep=" "),format=timeformats[4])}
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(paste(dat$Date,chron::times(as.numeric(dat$Time)),sep=" "),format=timeformats[5])}
  if(any(is.na(PosixTime))){PosixTime<-as.POSIXct(paste(dat$Date,chron::times(as.numeric(dat$Time)),sep=" "),format=timeformats[6])}
  if(any(is.na(PosixTime))){stop("Time format not recognized...")}
  dat[["Date"]]<-NULL
  dat[["Time"]]<-NULL
  dat<-cbind(PosixTime,dat)

  #6) If band names are imported as characters, convert to numbers:
  dat[,new_band_names]<-as.numeric(as.character(unlist(dat[,new_band_names])))

  #7) Return the resulting R dataframe:
  return(dat)
}
