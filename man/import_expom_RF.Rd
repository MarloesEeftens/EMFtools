\name{import_expom_RF}
\alias{import_expom_RF}
\title{Import .csv, .xls or .xlsx files downloaded from the ExpoM-RF device in R.}
\description{Given the pathname of a file downloaded from the ExpoM-RF, this function imports the speficied file into the R environment as a dataframe.}
\usage{import_ExpoM_RF(filename,prefix,suffix)}
\arguments{
  \item{filename}{The complete pathname (including path, name and extension) to the file you want to import into R.}
  \item{prefix}{(Optional, no default) Sometimes it is handy to import the column names with a prefix (e.g. "EXPOM_") so they are unique within the dataframe if you want to use the time series to compare with another instrument. The result of specifying a prefix is that all columns in the imported dataset (except for PosixTime) are named (e.g. "EXPOM_") EXPOM_dect, EXPOM_2100UL, etc.}
  \item{suffix}{(Optional, no default) Similar to above. The result of specifying a suffix (e.g. "_Vm") is that all columns related to frequency bands show this suffix in their variable name (e.g.) dect_Vm, 2100UL_Vm etc.}
  }
\details{}
\value{The result is an R dataframe of your ExpoM-RF file  which is ready to work with and includes the variables:
  \item{\code{PosixTime}{: POSIXct, date and time of each observation.}}
  \item{\code{FM}{: numeric, field strength [V/m] in the 87.5–108 MHz band.}}
  \item{\code{TV}{: numeric, field strength [V/m] in the 470–790 MHz band.}}
  \item{\code{DL800}{: numeric, field strength [V/m] in the 791–821 MHz band.}}
  \item{\code{UL800}{: numeric, field strength [V/m] in the 832–862 MHz band.}}
  \item{\code{UL900}{: numeric, field strength [V/m] in the 880–915 MHz band.}}
  \item{\code{DL900}{: numeric, field strength [V/m] in the 925–960 MHz band.}}
  \item{\code{UL1800}{: numeric, field strength [V/m] in the 1710–1785 MHz band.}}
  \item{\code{DL1800}{: numeric, field strength [V/m] in the 1805–1880 MHz band.}}
  \item{\code{DECT}{: numeric, field strength [V/m] in the 1880–1900 MHz band.}}
  \item{\code{UL2100}{: numeric, field strength [V/m] in the 1920–1980 MHz band.}}
  \item{\code{DL2100}{: numeric, field strength [V/m] in the 2110–2170 MHz band.}}
  \item{\code{WIFI2G}{: numeric, field strength [V/m] in the 2400–2485 MHz band.}}
  \item{\code{UL2600}{: numeric, field strength [V/m] in the 2500–2570 MHz band.}}
  \item{\code{DL2600}{: numeric, field strength [V/m] in the 2620–2690 MHz band.}}
  \item{\code{WIMAX}{: numeric, field strength [V/m] in the 3400–3600 MHz band.}}
  \item{\code{WIFI5G}{: numeric, field strength [V/m] in the 5150–5875 MHz band.}}
  \item{\code{TOTAL}{: numeric, total field strength [V/m] in all bands above.}}
  \item{\code{charging}{: numeric, whether (1) or not (0) the ExpoM-RF was charging.}}
  \item{\code{id}{: numeric, ID of the ExpoM-RF sampling device.}}
  \item{\code{latitude}{: numeric, Latitude in decimal degrees.}}
  \item{\code{longitude}{: numeric, Longitude in decimal degrees.}}
  \item{\code{speed}{: numeric, Estimated speed that the device is moving in [km/h].}}
  \item{\code{nr_sats}{: numeric, Number of satellites available to determine the coordinate.}}
  \item{\code{marker}{: numeric, Markers counted as the marker button was pressed.}}
  \item{\code{charge}{: numeric, Percentage charge remaining.}}
  \item{\code{charging}{: logical, whether (TRUE) or not (FALSE) the device was charging.}}
  \item{\code{overload}{: logical, Whether (TRUE) or not (FALSE) the device experienced an overload.}}
}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{As far as i know, R packages do not support example .xlsx files... The function should work also for them. A sample .xlsx file is only available from within SwissTPH. But chances are you have your own anyways!}
\seealso{}
\examples{
#Write the example .csv to a folder of your choice (please change pathname!):
my_filename1<-"V:/EEH/R_functions/EMFtools/data/expom_example2.csv"
write.table(expom_example2,file=my_filename1,sep="\t",quote=FALSE,row.names=FALSE)
#Import using the import function:
my_expom_file<-import_expom_RF(filename=my_filename1,prefix="EXPOM_",suffix="_Vm")
str(my_expom_file)

#Read an .xlsx file into R:
my_expom_file<-import_expom_RF(filename="V:/EEH/R_functions/EMFtools/data/expom_example3.xlsx")
}
\keyword{}
