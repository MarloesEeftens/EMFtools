#################################
##### correct_crosstalk     #####
##### By: Marloes Eeftens   #####
##### Last edit: 11/08/2017 #####
#################################

#Function correct_crosstalk:
correct_crosstalk=function(dataset,timevar,signal1,signal2,activityvar,no_ct_sig1,no_ct_sig2,stats=FALSE,plot_folder,plot_nr,window_width=4,change_threshold=10,correlation_threshold=0.20,suppressMessages){

  #0) Check settings, generate messages, set defaults:
  if(missing(dataset)){stop("Please specify a dataset...")}
  if(missing(timevar)){stop("Please specify the name of the time variables: timevar")}
  if(missing(signal1)){stop("Please specify the first variable considered for crosstalk: signal1")}
  if(missing(signal2)){stop("Please specify the first variable considered for crosstalk: signal2")}
  if(missing(suppressMessages)){suppressMessages<-FALSE}
  if(suppressMessages==FALSE){
    if(missing(activityvar)&missing(no_ct_sig1)&missing(no_ct_sig2)){message("activityvar not set, default correction to reporting limit (minimum value)")}
    if(!missing(activityvar)){message("Corrections based on median during same activity")}
    if(missing(stats)){message("stats not set, default is: FALSE")}
    if(stats==TRUE){message("Output will be a list with [[1]]: corrected dataset and [[2]]: stats about the corrections")}
    if(missing(window_width)){message("window_width not set, default is: 4")}
    if(missing(change_threshold)){message("change_threshold not set, default is: 10")}
    if(missing(correlation_threshold)){message("correlation_threshold not set, default is: 0.20")}
    if(missing(plot_folder)){message("No plot_folder specified. Time series plots will not be generated.")}}
  # You need the suggested package for this function
  if(missing(plot_folder)==FALSE&!requireNamespace("ggplot2",quietly=TRUE)){
    stop("package ggplot2 is needed for this function to work. Please install it.")
  }
  if(missing(plot_folder)==FALSE&!requireNamespace("gridExtra",quietly=TRUE)){
    stop("package gridExtra is needed for this function to work. Please install it.")
  }
  if(missing(plot_folder)==FALSE&!requireNamespace("reshape2",quietly=TRUE)){
    stop("package reshape2 is needed for this function to work. Please install it.")
  }
  if(missing(plot_folder)==FALSE&!requireNamespace("scales",quietly=TRUE)){
    stop("package scales is needed for this function to work. Please install it.")
  }

  #1) Extract the relevant variables from the dataset:
  if(!missing(activityvar)){dat<-subset(dataset,select=c(timevar,signal1,signal2,activityvar))}
  if(missing(activityvar)){dat<-subset(dataset,select=c(timevar,signal1,signal2))}
  dat$sig1_uncor<-dat[[signal1]]
  dat$sig2_uncor<-dat[[signal2]]

  #2) Calculate means for both signals during window before and after each observation
  dat$avg_s1_left<-rollapply(lag(dat$sig1_uncor,k=1),width=window_width,FUN=mean,na.rm=TRUE,align="left",fill=NA)
  dat$avg_s1_right<-rollapply(lag(dat$sig1_uncor,k=-1),width=window_width,FUN=mean,na.rm=TRUE,align="right",fill=NA)
  dat$avg_s2_left<-rollapply(lag(dat$sig2_uncor,k=1),width=window_width,FUN=mean,na.rm=TRUE,align="left",fill=NA)
  dat$avg_s2_right<-rollapply(lag(dat$sig2_uncor,k=-1),width=window_width,FUN=mean,na.rm=TRUE,align="right",fill=NA)
  #Calculate the rate of change (ROC) for both signals individually
  dat$ROCslope1<-abs((dat$avg_s1_left-dat$avg_s1_right)/dat$sig1_uncor)
  dat$ROCslope2<-abs((dat$avg_s2_left-dat$avg_s2_right)/dat$sig2_uncor)

  #3) Calculate ratios between signal1 and signal2 and the other way around, if they change quickly -> new cluster
  dat$rat_sig1_sig2<-dat$sig1_uncor/dat$sig2_uncor
  dat$rat_abs<-ifelse(dat$rat_sig1_sig2<1,dat$rat_sig1_sig2,1/dat$rat_sig1_sig2)
  dat$avg_rat_abs_left<-rollapply(lag(dat$rat_abs,k=1),width=window_width,FUN=mean,na.rm=TRUE,align="left",fill=NA)
  dat$avg_rat_abs_right<-rollapply(lag(dat$rat_abs,k=-1),width=window_width,FUN=mean,na.rm=TRUE,align="right",fill=NA)
  #Calculate the rate of change (ROC) for the ratio between the signals
  dat$ROCratio<-abs((dat$avg_rat_abs_left-dat$avg_rat_abs_right)/dat$rat_abs)

  #4) If one of the ROCs is above the threshold, define indicators a new potential cluster
  dat$change_ind_temp<-0
  dat$change_ind_temp[dat$ROCslope1>change_threshold|dat$ROCslope2>change_threshold|dat$ROCratio>change_threshold]<-1
  dat$change_ind_temp[is.na(dat$sig1_uncor)|is.na(dat$sig2_uncor)]<-0

  #5) Define the temporary clusters based on the change indicators:
  dat$cluster_temp<-cumsum(dat$change_ind_temp)+1

  #6) Many clusters are defined 4/8/12 seconds apart. We need at least 3 observations in each cluster to calculate a correlation. Therefore, we look for any double / triple / multiple-identified cluster starts. If k+1 k+2 or k+3 is also identified as a cluster, this is undesirable and should be resolved
  dat$change_ind_def<-dat$change_ind_temp
  dat$cluster_def<-dat$cluster_temp
  length_k<-dim(dat)[1]
  #Resolve closely spaced potential clusters
  #With consecutive cluster indicators
  for (k in 1:length_k){
    if(dat$change_ind_def[k]==1&dat$change_ind_def[k+1]==1){
      #If ROCratio is higher for [k+1] than for [k], we run cluster [k-1] 1 instance longer
      if(dat$ROCratio[k]<=dat$ROCratio[k+1]){
        dat$cluster_def[k]<-dat$cluster_def[k-1]
        dat$change_ind_def[k]<-0}
      #If ROCratio is higher for [k] than for [k+1], we start cluster [k+1] 1 instance earlier
      if(dat$ROCratio[k]>dat$ROCratio[k+1]){
        dat$cluster_def[k]<-dat$cluster_def[k+1]
        dat$change_ind_def[k+1]<-0}}}
  #With one observation in between
  for (k in 1:length_k){
    if(dat$change_ind_def[k]==1&dat$change_ind_def[k+2]==1){
      if(dat$ROCratio[k]<=dat$ROCratio[k+2]){
        dat$cluster_def[k]<-dat$cluster_def[k-1]
        dat$cluster_def[k+1]<-dat$cluster_def[k-1]
        dat$change_ind_def[k]<-0}
      if(dat$ROCratio[k]>dat$ROCratio[k+2]){
        dat$cluster_def[k]<-dat$cluster_def[k+2]
        dat$cluster_def[k+1]<-dat$cluster_def[k+2]
        dat$change_ind_def[k+2]<-0}}}
  #With two observations in between
  for (k in 1:length_k){
    if(dat$change_ind_def[k]==1&dat$change_ind_def[k+3]==1){
      if(dat$ROCratio[k]<=dat$ROCratio[k+3]){
        dat$cluster_def[k]<-dat$cluster_def[k-1]
        dat$cluster_def[k+1]<-dat$cluster_def[k-1]
        dat$cluster_def[k+2]<-dat$cluster_def[k-1]
        dat$change_ind_def[k]<-0}
      if(dat$ROCratio[k]>dat$ROCratio[k+3]){
        dat$cluster_def[k]<-dat$cluster_def[k+3]
        dat$cluster_def[k+1]<-dat$cluster_def[k+3]
        dat$cluster_def[k+2]<-dat$cluster_def[k+3]
        dat$change_ind_def[k+3]<-0}}}

  #7) Calculate the correlation and ratio between the signals for each cluster
  rep_limit_sig1<-min(dat$sig1_uncor)
  rep_limit_sig2<-min(dat$sig2_uncor)
  cluster_corrs<-data.frame(cluster_def=unique(dat$cluster_def))
  cluster_corrs$correlation<-0
  cluster_corrs$correlation_log<-0
  cluster_corrs$ratio<-1
  for(k in 1:length(cluster_corrs$cluster_def)){
    dat_sel<-subset(dat,cluster_def==cluster_corrs$cluster_def[k]&sig1_uncor>rep_limit_sig1&sig2_uncor>rep_limit_sig2)
    if(dim(dat_sel)[1]>2&var(dat_sel$sig1_uncor)>0&var(dat_sel$sig2_uncor)>0){
      cluster_corrs$correlation[k]<-cor(dat_sel$sig1_uncor,dat_sel$sig2_uncor,use="complete")
      cluster_corrs$correlation_log[k]<-cor(log(dat_sel$sig1_uncor),log(dat_sel$sig2_uncor),use="complete")
      cluster_corrs$ratio[k]<-mean(dat_sel$sig1_uncor,na.rm=TRUE)/mean(dat_sel$sig2_uncor,na.rm=TRUE)
      }
    }

  #8) Merge correlation and ratio statistics into time series.
  dat<-merge(dat,cluster_corrs,by="cluster_def")
  #Set default to no corrections: <-0
  dat$sig1_corrected<-0
  dat$sig2_corrected<-0
  #Set default corrected values to the basic corrected ones
  dat$sig1_cor<-dat$sig1_uncor
  dat$sig2_cor<-dat$sig2_uncor
  #If correlation is high and direction is right, DO correct: <-1
  dat$sig1_corrected[dat$correlation_log>correlation_threshold&dat$ratio<1]<-1
  dat$sig2_corrected[dat$correlation_log>correlation_threshold&dat$ratio>1]<-1

  #9) Define the correction values and correct the affected values:
  if(missing(activityvar)){
    if(missing(no_ct_sig1)&missing(no_ct_sig2)){
      #Set values to the minimum reported in the dataset:
      no_ct_sig1<-min(dat$sig1_uncor)
      no_ct_sig2<-min(dat$sig2_uncor)
      }
    #If no_ct_sig1 and no_ct_sig2 were not missing, they were set by the user, so use those:
    dat$sig1_cor[dat$sig1_corrected==1&dat$sig1_uncor>no_ct_sig1]<-no_ct_sig1
    dat$sig2_cor[dat$sig2_corrected==1&dat$sig2_uncor>no_ct_sig2]<-no_ct_sig2
    }
  if(!missing(activityvar)){
    activities<-data.frame(activity=unique(dat[[activityvar]]))
    #Set default corrected values to the basic corrected ones
    activities$no_ct_sig1<-min(dat$sig1_uncor)
    activities$no_ct_sig2<-min(dat$sig2_uncor)
    #If there are enough observations available, take the medians of unaffected observations
    for(m in activities$activity){
      dat_no_ct_sig1<-dat[dat[,activityvar]==m&dat$sig1_corrected==0,]
      dat_no_ct_sig2<-dat[dat[,activityvar]==m&dat$sig2_corrected==0,]
      if(dim(dat_no_ct_sig1)[1]>5){
        activities$no_ct_sig1[activities$activity==m]<-median(dat_no_ct_sig1$sig1_uncor)}
      if(dim(dat_no_ct_sig2)[1]>5){
        activities$no_ct_sig2[activities$activity==m]<-median(dat_no_ct_sig2$sig2_uncor)}
      #Then assign those medians to the crosstalk affected values
      dat$sig1_corrected[dat[,activityvar]==m&dat$sig1_uncor<=activities$no_ct_sig1[activities$activity==m]]<-0
      dat$sig2_corrected[dat[,activityvar]==m&dat$sig2_uncor<=activities$no_ct_sig2[activities$activity==m]]<-0
      dat$sig1_cor[dat$sig1_corrected==1&dat[,activityvar]==m]<-activities$no_ct_sig1[activities$activity==m]
      dat$sig2_cor[dat$sig2_corrected==1&dat[,activityvar]==m]<-activities$no_ct_sig2[activities$activity==m]
      }
    }
  dat_cor<-dat[,c("change_ind_temp","change_ind_def","cluster_def","correlation_log","sig1_corrected","sig2_corrected","sig1_cor","sig2_cor")]
  newnames<-c(paste0(signal1,"_cor_01"),paste0(signal2,"_cor_01"),paste0(signal1,"_cor"),paste0(signal2,"_cor"))
  names(dat_cor)<-c("change_ind_temp","change_ind_def","cluster_def","correlation_log",newnames)
  dataset_cor<-cbind(dataset,dat_cor)

  #10) Write 1 time series plot per cluster (up to maximum of plot_nr) to the plot_folder specified
  if(!missing(plot_folder)){
    if(suppressMessages==FALSE){message(paste0("Time series plots will be exported to: ",plot_folder))}
    #Plot text settings
    text_s<-element_text(colour="black",size=14)
    text_b<-element_text(colour="black",size=16)
    #Make a plot for all unique new cluster numbers
    if(missing(plot_nr)){plot_nr<-length(unique(dataset_cor$cluster_def))}
    for (k in head(unique(dataset_cor$cluster_def),plot_nr)){
      starttime_cluster<-head(subset(dataset_cor,cluster_def==k,select=timevar),1)[1,1]
      stoptime_cluster<-tail(subset(dataset_cor,cluster_def==k,select=timevar),1)[1,1]
      duration_cluster<-stoptime_cluster-starttime_cluster
      starttime_graph<-starttime_cluster-duration_cluster/3-60
      stoptime_graph<-stoptime_cluster+duration_cluster/3+60
      dat_sel1<-subset(dataset_cor,PosixTime>starttime_graph&PosixTime<stoptime_graph)
      dat_sel2<-subset(dat_sel1,PosixTime>=starttime_cluster&PosixTime<stoptime_cluster)
      correlation_log<-round(dat_sel2$correlation_log[1],digits=2)
      old_times_of_change<-subset(dat_sel1,change_ind_temp==1,select=timevar)
      new_times_of_change<-subset(dat_sel1,change_ind_def==1,select=timevar)
      main_title<-paste0("Cluster ",k,", correlation (log) = ",correlation_log)
      #Reorganize the data:
      dat_sel1a<-subset(dat_sel1,select=c(timevar,signal1,signal2,newnames[3:4]))
      dat_sel1a_melt<-melt(dat_sel1a,id=timevar)
      names(dat_sel1a_melt)<-c("time","band","value")
      p1<-ggplot(data=dat_sel1a_melt,aes(x=time,y=value))+
       geom_line(aes(color=band))+
       geom_vline(data=old_times_of_change,aes(xintercept=as.numeric(PosixTime)),color="yellow")+
       geom_vline(data=new_times_of_change,aes(xintercept=as.numeric(PosixTime)),color="black")+
       scale_size_manual(values=c(1,1,0.5,0.5))+
       scale_color_manual(values=c("#A6CEE3","#FB9A99","#1F78B4","#E31A1C"))+
       theme(title=text_b,axis.title=text_s,axis.text=text_s,legend.text=text_s,legend.title=element_blank(),legend.position="bottom")+
       ggtitle(main_title)+labs(x="",y="Exposure level")+
       scale_x_datetime(labels=date_format("%m/%d %H:%M"))+
       scale_y_log10(limits=c(0.000001,10))
      #Correlation scatterplot
      dat_sel2a<-subset(dat_sel2,select=c(timevar,signal1,signal2))
      names(dat_sel2a)<-c(timevar,"signal1","signal2")
      p2<-ggplot(data=dat_sel2a,aes(signal1,signal2))+geom_point()+
       labs(x=paste0("Exposure level ",signal1),y=paste0("Exposure level ",signal2))+
       geom_smooth(method='lm',formula=y~x,linetype="dashed",color="red",size=1)+
       theme(title=text_b,axis.title=text_s,axis.text=text_s,legend.text=text_s)+
       scale_x_log10(limits=c(0.000001,10))+
       scale_y_log10(limits=c(0.000001,10))
      #Combine the plots and save
      p<-grid.arrange(p1,p2,layout_matrix=cbind(1,1,1,1,1,1,2,2,2))
      ggsave(paste0(plot_folder,"cluster_",k,".png"),plot=p,units="cm",dpi=600,width=30,height=10)
      }
    }

  #11) Produce statistics about the correction as 2nd part of output
  if(stats==TRUE){
    n_obs<-dim(dataset_cor)[1]
    duration_hours<-as.numeric(difftime(tail(dataset_cor$PosixTime,1),head(dataset_cor$PosixTime,1),units="hours"))
    duration_days<-as.numeric(difftime(tail(dataset_cor$PosixTime,1),head(dataset_cor$PosixTime,1),units="days"))
    nr_potential_clusters<-sum(dataset_cor$change_ind_temp)
    nr_definitive_clusters<-sum(dataset_cor$change_ind_def)
    eval(parse(text=paste0("nr_corrected_sig1<-sum(dataset_cor$",signal1,"_cor_01)")))
    eval(parse(text=paste0("nr_corrected_sig2<-sum(dataset_cor$",signal2,"_cor_01)")))
    eval(parse(text=paste0("mean_sig1_uncor<-mean(dataset_cor$",signal1,",na.rm=T)")))
    eval(parse(text=paste0("mean_sig2_uncor<-mean(dataset_cor$",signal2,",na.rm=T)")))
    eval(parse(text=paste0("mean_sig1_cor<-mean(dataset_cor$",signal1,"_cor,na.rm=T)")))
    eval(parse(text=paste0("mean_sig2_cor<-mean(dataset_cor$",signal2,"_cor,na.rm=T)")))
    perc_corrected_sig1<-(nr_corrected_sig1/n_obs)*100
    perc_corrected_sig2<-(nr_corrected_sig2/n_obs)*100
    perc_reduction_sig1<-(mean_sig1_uncor-mean_sig1_cor)/mean_sig1_uncor*100
    perc_reduction_sig2<-(mean_sig2_uncor-mean_sig2_cor)/mean_sig2_uncor*100
    statistics<-cbind(n_obs,duration_hours,duration_days,
      nr_potential_clusters,nr_definitive_clusters,nr_corrected_sig1,nr_corrected_sig2,
      mean_sig1_uncor,mean_sig2_uncor,mean_sig1_cor,mean_sig2_cor,
      perc_corrected_sig1,perc_corrected_sig2,perc_reduction_sig1,perc_reduction_sig2)
    if(suppressMessages==FALSE){
      message(paste0(signif(perc_corrected_sig1,3),"% Of observations were corrected for ",signal1,": mean exposure reduced by ",signif(perc_reduction_sig1,digits=3),"%"))
      message(paste0(signif(perc_corrected_sig2,3),"% Of observations were corrected for ",signal2,": mean exposure reduced by ",signif(perc_reduction_sig2,digits=3),"%"))
      }
    dataset_cor<-list(df=dataset_cor,stats=statistics)
    }

  #12) Return resulting dataset
  return(dataset_cor)
  }
