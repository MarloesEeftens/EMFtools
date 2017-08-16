\name{expom_example1}
\alias{expom_example1}
\docType{data}
\title{An example dataset to practice various corrections and data interpretations.}
\description{This sample dataset was derived from ExpoM-RF measurements including 16 frequency bands, and activity information reported by the subject who carried the device.}
\usage{data("expom_example1")}
\format{
  A data frame with a time series of 1516 observations on the following 20 variables:
  \describe{
    \item{\code{PosixTime}{: POSIXct, date and time of each observation.}}
    \item{\code{FM}{: numeric, Power Flux Density [mW/m²] in the 87.5–108 MHz band.}}
    \item{\code{TV}{: numeric, Power Flux Density [mW/m²] in the 470–790 MHz band.}}
    \item{\code{DL800}{:  numeric, Power Flux Density [mW/m²] in the 791–821 MHz band.}}
    \item{\code{UL800}{: numeric, Power Flux Density [mW/m²] in the 832–862 MHz band.}}
    \item{\code{UL900}{: numeric, Power Flux Density [mW/m²] in the 880–915 MHz band.}}
    \item{\code{DL900}{: numeric, Power Flux Density [mW/m²] in the 925–960 MHz band.}}
    \item{\code{UL1800}{: numeric, Power Flux Density [mW/m²] in the 1710–1785 MHz band.}}
    \item{\code{DL1800}{: numeric, Power Flux Density [mW/m²] in the 1805–1880 MHz band.}}
    \item{\code{DECT}{: numeric, Power Flux Density [mW/m²] in the 1880–1900 MHz band.}}
    \item{\code{UL2100}{: numeric, Power Flux Density [mW/m²] in the 1920–1980 MHz band.}}
    \item{\code{DL2100}{: numeric, Power Flux Density [mW/m²] in the 2110–2170 MHz band.}}
    \item{\code{WIFI2G}{: numeric, Power Flux Density [mW/m²] in the 2400–2485 MHz band.}}
    \item{\code{UL2600}{: numeric, Power Flux Density [mW/m²] in the 2500–2570 MHz band.}}
    \item{\code{DL2600}{: numeric, Power Flux Density [mW/m²] in the 2620–2690 MHz band.}}
    \item{\code{WIMAX}{: numeric, Power Flux Density [mW/m²] in the 3400–3600 MHz band.}}
    \item{\code{WIFI5G}{: numeric, Power Flux Density [mW/m²] in the 5150–5875 MHz band.}}
    \item{\code{TOTAL}{: numeric, total Power Flux Density [mW/m²] in all bands above.}}
    \item{\code{charging}{: logical, whether (TRUE) or not (FALSE) the device was charging.}}
    \item{\code{activity}{: character, the activity that the study subject reported throughout the measurement.}}
    }
}
\details{The data in this file is based on actual, real-life measurements, though all personal information and coordinates have been removed. I shortened the time series and condensed the interesting events, to keep file size small(-ish). The subject in this file briefly charges his ExpoM-RF, makes several DECT phone calls, travels by car and spends time outside.}
\source{Data was collected in the general environment of Zurich, Switzerland for a study on environmental exposure to RF-EMF conducted by Swiss TPH.}
\references{}
\examples{
head(expom_example1)}
\keyword{datasets}
