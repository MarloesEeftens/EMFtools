\name{correct_crosstalk}
\alias{correct_crosstalk}
\title{Enables the detection, correction and visualization of crosstalk in time series of radiofrequency electromagnetic field (RF-EMF) measurements taken by portable devices such as ExpoM-RF.}
\description{Given a dataframe with a time series of RF-EMF measurements which is suspectedly affected by crosstalk, the function enables you to detect the extent of the crosstalk by analyzing how correlated the aggressor and victim frequency bands (specified by the user) are. Highly correlated parts of the time series are corrected downward and returned as additional variables signal1_cor and signal2_cor. The user has the option to visualize the corrections through plots, and to get a report on the effect of the corrections made.}
\usage{correct_crosstalk(dataset,timevar,signal1,signal2,activityvar,no_ct_sig1,no_ct_sig2,stats,plot_folder,plot_nr,window_width,change_threshold,correlation_threshold,suppressMessages)}
\arguments{
  \item{dataset}{A dataframe with a time series of RF-EMF measurements which are not yet corrected for crosstalk. Specify as follows: dataset=dat1.}
  \item{timevar}{The name of the variable which contains the timestamp. The format of this variable should be POSIXct. Specify as follows: timevar="PosixTime".}
  \item{signal1}{The name of a numeric variable which contains the first frequency band considered for correction. Specify as follows: signal1="DECT".}
  \item{signal2}{The name of a numeric variable which contains the second frequency band considered for correction. Specify as follows: signal2="DCSDL".}
  \item{activityvar}{(Optional) There is a possibility to correct crosstalk-affected values down to the median crosstalk-unaffected value experienced during the same activity (e.g. in personal measurements). To do that, you can specify the (character) variable containing the activity information as follows: activityvar="act". If no activityvar is specified, the default is to correct to no_ct_sig1 and no_ct_sig2, see below.}
  \item{no_ct_sig1}{(Optional) There is a possibility to specify the level that signal1 should be corrected to, in case it is affected by crosstalk. Specify as follows: no_ct_sig1=1.659145e-05. If no activityvar is specified, no_ct_sig1 defaults to the lowest value present in the uncorrected data of signal1.}
  \item{no_ct_sig2}{(Optional) Similar to no_ct_sig1, see above. no_ct_sig2 Defaults to the lowest value present in the uncorrected data of signal2.}
  \item{stats}{(Optional, defaults to stats=FALSE.) TRUE will produce some statistics about the correction (see below), and return the function output in the form of a list consisting of [[1]], the corrected dataset and [[2]] a file detailing the statistics above, listing the following items:
     n_obs (number of observations),
     duration_hours (duration of the measurement in hours),
     duration_days (duration of the measurement in days),
     nr_potential_clusters (number of observations flagged originally as cluster starts),
     nr_definitive_clusters (number of cluster starts remaining after resolving consecutive ones),
     nr_corrected_sig1 (number of observations corrected for signal1),
     nr_corrected_sig2 (number of observations corrected for signal2),
     mean_sig1_uncor (mean of signal 1 before correction),
     mean_sig2_uncor (mean of signal 2 before correction),
     mean_sig1_cor (mean of signal 1 after correction),
     mean_sig2_cor (mean of signal 2 after correction),
     perc_corrected_sig1 (percentage of observations corrected in signal1),
     perc_corrected_sig2 (percentage of observations corrected in signal2),
     perc_reduction_sig1 (percentage exposure reduction -relative to uncorrected- after correction signal1),
     perc_reduction_sig2 (percentage exposure reduction -relative to uncorrected- after correction signal2)
}
  \item{plot_folder}{Specify a folder into which a time series plots for each cluster can be exported so that you can visually check the corrections.}
  \item{plot_nr}{Specify the number of plots you want to export. If you have a long time series of measurements, specifying plot_nr allows you to export just a few plots for visual checking.}
  \item{window_width}{Number of observations (window) required before and after an observation to detect a sudden change in signal1 or signal2 or in the ratio between them.}
  \item{change_threshold}{Change in signal required within the signals or in the ratio between the signals to trigger the start of a potential new data cluster. Defaults to 10: a 10-fold change (increase or decrease) over the interval of window_width observations is required to flag the start of a potential new cluster.}
  \item{correlation_threshold}{The correlation threshold between signal1 and signal 2 that should be exceeded in order for the cluster to be flagged as crosstalk. Defaults to 0.20, because we usually expect very little natural correlation between the frequency bands.}
  \item{suppressMessages}{Defaults to FALSE. Specify "suppressMessages=TRUE" if you do not want to display messages about settings, performance, etc.}
}
\details{}
\value{
  \item{$df}{The same dataframe as provided in dataset, but with additional variables signal1_cor and signal2_cor}
  \item{$stats}{If stats==TRUE, some statistics on the impact of the correction.}
}
\references{A paper describing an application of the function "correct_crosstalk" has been submitted for publication.}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{}
\seealso{}
\examples{
#Load some example data:
data(expom_example1)
head(expom_example1)
#Correct crosstalk between dect and dcsdl bands:
example1_cor<-correct_crosstalk(dataset=expom_example1,timevar="PosixTime",signal1="DECT",signal2="DL1800",activityvar="activity",stats=TRUE)
head(example1_cor$df)
example1_cor$stats

#Save some plots from the time series in my_plot_folder:
my_wd<-"V:/EEH/R_functions/EMFtools"
if(!file.exists(paste0(my_wd,"/","EMF_plot_folder","/"))){dir.create(file.path(my_wd,"EMF_plot_folder"))}
my_plot_folder<-paste0(my_wd,"/","EMF_plot_folder","/")
example1_cor<-correct_crosstalk(dataset=expom_example1,timevar="PosixTime",signal1="DECT",signal2="DL1800",activityvar="activity",stats=FALSE,plot_folder=my_plot_folder,plot_nr=10)
}
\keyword{}
