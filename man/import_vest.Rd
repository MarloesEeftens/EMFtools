\name{import_vest}
\alias{import_vest}
\title{Import .csv files from the body worn distributed sensor vest in R.}
\description{Given the filename of a file compiled from the vest, this function imports the speficied file into the R environment as a dataframe.}
\usage{import_vest(filename,prefix,suffix)}
\arguments{
  \item{filename}{The complete filename (including path, name and extension) to the file you want to import into R.}
  \item{prefix}{(Optional, no default) Sometimes it is handy to import the column names with a prefix (e.g. "VEST_") so they are unique within the dataframe if you want to use the time series to compare with another instrument. The result of specifying a prefix is that all columns in the imported dataset (except for PosixTime) are named (e.g. "VEST_") VEST_dect, VEST_2100UL, etc.}
  \item{suffix}{(Optional, no default) Similar to above. The result of specifying a suffix (e.g. "_Vm") is that all columns related to frequency bands show this suffix in their variable name (e.g.) DECT_Vm, 2100UL_Vm etc.}
}
\details{}
\value{The result is an R dataframe of your vest file which is ready to work with.}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{A small vest example .csv file -at least the way it looks for now- is available as an example, but the format will likely change. To be continued...}
\seealso{}
\examples{
#Write the example .csv to a folder of your choice (please change pathname!):
my_filename1<-"V:/EEH/R_functions/EMFtools/data/vest_example.csv"
write.table(vest_example,file=my_filename1,sep=",",quote=FALSE,row.names=FALSE)
#Import using the import function:
my_vest_file<-import_vest(filename=my_filename1,prefix="VEST_",suffix="_Vm")
str(my_vest_file)
}
\keyword{}
