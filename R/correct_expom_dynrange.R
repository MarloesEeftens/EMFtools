##################################
##### correct_expom_dynrange #####
##### By: Marloes Eeftens    #####
##### Last edit: 09/04/2019  #####
##################################

#Function correct_dynamic_range:
correct_expom_dynrange=function(dataset,bandnames,prefix="",old_suffix="",new_suffix="",new_total=FALSE){

  #0) Define bandnames if not supplied:
  dat<-dataset
  if(missing(bandnames)){
    bandnames<-c("FM","TV","DL800","UL800","UL900","DL900","UL1800","DL1800","DECT",
                                      "UL2100","DL2100","WIFI2G","UL2600","DL2600","WIMAX","WIFI5G")
  }
  old_bandnames<-paste0(prefix,gsub(old_suffix,"",bandnames),old_suffix)
  new_bandnames<-gsub(old_suffix,new_suffix,bandnames)
  if(!all(old_bandnames %in% names(dat))){
    stop("Not all bandnames found in dataset... Define all variables containing the ExpoM-RF frequency bands.")}

  #1) Define dynamic range:
  low_replimit_vec<-c(0.02,rep(0.005,8),0.003,0.003,0.005,rep(0.003,3),0.05)/2
  low_replimit_vec2<-low_replimit_vec^2/0.3767
  upp_replimit_vec<-c(rep(5,14),3,5)
  upp_replimit_vec2<-upp_replimit_vec^2/0.3767

  #2) Create a copy of the uncorrected bandname unless the old ones are the same as the new ones (and thus should be overwritten)
  if(any(old_bandnames!=new_bandnames)){
    for(j in seq(along=old_bandnames)){
    eval(parse(text=paste0("dat$",new_bandnames[j],"<-","dat$",old_bandnames[j])))}
  }

  #3) Correct values below low_replimit_vec2
  for(j in seq(along=new_bandnames)){
    eval(parse(text=paste0("dat$",new_bandnames[j],"[dat$",new_bandnames[j],"<",low_replimit_vec2[j],"]<-",low_replimit_vec2[j])))
  }

  #4) Correct values above upp_replimit_vec2
  for(j in seq(along=new_bandnames)){
    eval(parse(text=paste0("dat$",new_bandnames[j],"[dat$",new_bandnames[j],">",upp_replimit_vec2[j],"]<-",upp_replimit_vec2[j])))
  }

  #5) Recalculate total:
  if(new_total==TRUE){
    new_total<-rowSums(subset(dat,select=new_bandnames))
    eval(parse(text=paste0("dat$",prefix,"TOTAL",new_suffix,"<-new_total")))}

  #6) Return result:
  return(dat)
}
