#################################
##### convert_Vm_to_mWm2    #####
##### By: Marloes Eeftens   #####
##### Last edit: 11/08/2017 #####
#################################

convert_Vm_to_mWm2=function(x,vars,old_suffix,new_suffix){

  #0) Set defaults and parameters:
  if(missing(x)){stop("Please specify a dataframe, numeric vector or value for x...")}
  if(missing(old_suffix)){old_suffix<-""}
  if(missing(new_suffix)){new_suffix<-""}

  #1) Convert variables indicated for conversion:
  if(is.data.frame(x)){
    new_vars<-gsub(old_suffix,new_suffix,vars)
    new_x<-x
    new_x[,new_vars]<-lapply(x[,vars],function(xx) xx^2*1000/377)
    }
  if(is.vector(x)|is.numeric(x)){new_x<-x^2*1000/377}

  #2) Return converted data:
  return(new_x)
  }
