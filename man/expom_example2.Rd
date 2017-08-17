\name{expom_example2}
\alias{expom_example2}
\docType{data}
\title{An example dataset as downloaded directly from an ExpoM-RF device as raw .csv data.}
\description{This .csv dataset was downloaded from an ExpoM-RF device.}
\usage{data("expom_example2")}
\format{
  A data frame with 3193 observations on the following variable.
  \describe{
    \item{\code{Date and Time}}{Contains the date and time at which the corresponding sample was taken.}
    \item{\code{Sequence number}}{Each measurement is labeled with a sequence number. Every time ExpoM - RF is switched on the sequence number starts at 1. If not interrupted by a power cycle, the sequence
number counts up to 65535 after which it restarts at 1.}
    \item{\code{FM Radio}}{Field strength [V/m] in the 87.5–108 MHz band.}
    \item{\code{TV}}{Field strength [V/m] in the 470–790 MHz band.}
    \item{\code{Mobile 800 MHz Downlink}}{Field strength [V/m] in the 791–821 MHz band.}
    \item{\code{Mobile 800 MHz Uplink}}{Field strength [V/m] in the 832–862 MHz band.}
    \item{\code{Mobile 900 MHz Uplink}}{Field strength [V/m] in the 880–915 MHz band.}
    \item{\code{Mobile 900 MHz Downlink}}{Field strength [V/m] in the 925–960 MHz band.}
    \item{\code{Mobile 1.8 GHz Uplink}}{Field strength [V/m] in the 1710–1785 MHz band.}
    \item{\code{Mobile 1.8 GHz Downlink}}{Field strength [V/m] in the 1805–1880 MHz band.}
    \item{\code{DECT}}{Field strength [V/m] in the 1880–1900 MHz band.}
    \item{\code{Mobile 2.1 GHz Uplink}}{Field strength [V/m] in the 1920–1980 MHz band.}
    \item{\code{Mobile 2.1 GHz Downlink}}{Field strength [V/m] in the 2110–2170 MHz band.}
    \item{\code{ISM 2.4 GHz}}{Field strength [V/m] in the 2400–2485 MHz band.}
    \item{\code{Mobile 2.6 GHz Uplink}}{Field strength [V/m] in the 2500–2570 MHz band.}
    \item{\code{Mobile 2.6 GHz Downlink}}{Field strength [V/m] in the 2620–2690 MHz band.}
    \item{\code{Mobile 3.5 GHz}}{Field strength [V/m] in the 3400–3600 MHz band.}
    \item{\code{WiFi 5 GHz}}{Field strength [V/m] in the 5150–5875 MHz band.}
    \item{\code{Total}}{Field strength [V/m] in all 16 bands.}
    \item{\code{Overload}}{An exclamation mark (!) in this field signalizes that ExpoM - RF measured a signal exceeding its specified measurement range. The affected sample is likely to be inaccurate due to
overload effects and should be discarded. Otherwise this entry is empty.}
    \item{\code{GPS Fix}}{0=invalid; 1=2D only (no accurate altitude information) 2=full GPS localization.}
    \item{\code{GPS Lat}}{NMEA format: degrees minutes.decimal_minutes.}
    \item{\code{GPS Lon}}{NMEA format: degrees minutes.decimal_minutes.}
    \item{\code{GPS Altitude}}{Meters above mean sea level. Caution: accuracy is strongly signal dependent.}
    \item{\code{GPS HDOP}}{GPS fix quality (lower = better). <2=very good; 2-5=OK; >5=limited accuracy.}
    \item{\code{GPS}}{Number of satellites in view; higher numbers mean higher GPS reliability.}
  }
}
\details{Data collected by Jana Soenksen, who paid a visit to the radio & TV broadcast tower North-East of Basel, Switzerland.}
\source{}
\references{Website of the ExpoM-RF producer: http://fieldsatwork.ch/index.php?page=downloads. See manual for more information about variables.}
\examples{
#Write the example .csv to a folder of your choice (please change pathname!):
my_filename1<-"V:/EEH/R_functions/EMFtools/data/expom_example2.csv"
write.table(expom_example2,file=my_filename1,sep="\t",quote=FALSE,row.names=FALSE)
#Import using the import function:
my_expom_file<-import_expom_RF(filename=my_filename1,prefix="EXPOM_",suffix="_Vm")
str(my_expom_file)
}
\keyword{datasets}
