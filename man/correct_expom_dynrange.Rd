\name{correct_expom_dynrange}
\alias{correct_expom_dynrange}
\title{Corrects a time series of ExpoM-RF measurements to their lower and upper dynamic range, as provided by Fields at Work.}
\description{Given a dataframe with a time series of Expom RF-EMF measurements, this function enables you to cap the low measurements to half of the reporting limit and the high measurements to the upper reporting limit. Values are assumed to be in mW/m².}
\usage{correct_expom_dynrange(dataset,bandnames,prefix,old_suffix,new_suffix,new_total)}
\arguments{
  \item{dataset}{A dataframe with a time series of RF-EMF measurements which are not yet capped to their dynamic range.}
  \item{bandnames}{(Optional, defaults to paste0(prefix,c("FM","TV","DL800","UL800","UL900","DL900","UL1800","DL1800","DECT","UL2100","DL2100","WIFI2G","UL2600","DL2600","WIMAX","WIFI5G"),old_suffix)). A vector with the 16 bandnames of the ExpoM-RF. If the names are those from the import_expom_RF function, the default can be used with or without the optional prefix and/or suffix.}
  \item{prefix}{(Optional, no default) Use e.g. prefix="EXPOM_" if the bandnames in your dataset have a common prefix, e.g. EXPOM_dect, EXPOM_UL2100, etc.}
  \item{old_suffix}{(Optional, no default) Use e.g. old_suffix="_uncorr" if the bandnames in your dataset have a suffix. You can change the suffix in the correction using e.g. new_suffix="_corr".}
  \item{new_suffix}{(Optional, no default) Use e.g. new_suffix="_corr" if you want the bandnames to display a new suffix in the variable name after correction.}
  \item{new_total}{(Optional, defaults to FALSE) Specify new_total=TRUE if you want to calculate a new corrected total of the 16 corrected bands.}
}
\details{}
\value{This function returns the same dataframe as provided in dataset, but with the dynamic range correction applied. The variables are overwritten unless a new_suffix is supplied.}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{Based on code by Benjamin Struchen.}
\seealso{}
\examples{
dynrange_corrected<-correct_expom_dynrange(expom_example1,new_suffix="_corr",new_total=TRUE)
head(dynrange_corrected)
}
\keyword{}
