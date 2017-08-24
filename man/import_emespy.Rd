\name{import_emespy}
\alias{import_emespy}
\title{Import (usually very huge) .xls files downloaded from the EMEspy device in R.}
\description{Given the filename of a file downloaded from the EMEspy, this function imports the specified file into the R environment as a dataframe.}
\usage{import_emespy(filename,prefix,suffix)}
\arguments{
  \item{filename}{The complete filename (including path, name and extension) to the file you want to import into R.}
  \item{prefix}{(Optional, no default) Sometimes it is handy to import the column names with a prefix (e.g. "EMESPY_") so they are unique within the dataframe if you want to use the time series to compare with another instrument. The result of specifying a prefix is that all columns in the imported dataset (except for PosixTime) are named (e.g. "EMESPY_") EMESPY_dect, EMESPY_2100UL, etc.}
  \item{suffix}{(Optional, no default) Similar to above. The result of specifying a suffix (e.g. "_Vm") is that all columns related to frequency bands show this suffix in their variable name (e.g.) dect_Vm, 2100UL_Vm etc.}
}
\details{}
\value{The result is an R dataframe of your EMEspy file which is ready to work with.}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{The EMEspy example .xls files are huge and therefore (sorry!) only available from within SwissTPH. But chances are you have your own anyways! If not, just contact me.}
\seealso{}
\examples{
#Specify an .xls file:
my_filename<-"V:/EEH/R_functions/EMFtools/data/emespy_example.xls"
my_emespy_file<-import_emespy(filename=my_filename,prefix="EMESPY_")
str(my_emespy_file)
}
\keyword{}
