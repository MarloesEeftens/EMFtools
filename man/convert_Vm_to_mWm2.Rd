\name{convert_Vm_to_mWm2}
\alias{convert_Vm_to_mWm2}
\title{Convert values, vectors and dataframe variables from field strenght [V/m] to Power Flux Density [mW/m²]}
\description{The field strength levels in most RF-EMF measurement files (e.g. from ExpoM-RF) are given in Volts per meter [V/m]. This function converts field strength values, numeric vectors or variables within dataframes to Power Flux Density values in microWatts per square meter [mW/m²].}
\usage{convert_Vm_to_mWm2(x,vars,old_suffix,new_suffix)}
\arguments{
  \item{x}{A value, numeric vector or R dataframe that includes the values to be converted.}
  \item{vars}{If x is a dataframe, indicate a single variable name OR a vector of variable names for conversion.}
  \item{old_suffix}{(Optional, no default) If a suffix was included in your original dataset (e.g. "_Vm"), you can remove this from your converted variables by specifying no new_suffix, or specify a new_suffix (e.g. new_suffix="mWm2") instead. If no old_suffix OR new_suffix is specified, the original variable names are again used for the mW/m² values in the returned dataset.}
  \item{new_suffix}{(Optional, no default) Same as old_suffix, see above, e.g. "_mWm2"}
}
\details{}
\value{If x is a vector, the result is a vector of the same size with all values converted to mW/m². If x is a dataframe, the result is a dataframe with all values within the indicated variables converted to mW/m².}
\references{}
\author{Marloes Eeftens, marloes.eeftens@swisstph.ch}
\note{}
\seealso{}
\examples{
#You can convert values / vectors:
value_in_mWm2<-convert_Vm_to_mWm2(5)
value_in_mWm2
vector_in_mWm2<-convert_Vm_to_mWm2(c(1,2,3,4,5))
vector_in_mWm2

#Or columns within a dataset:
my_filename1<-"V:/EEH/R_functions/EMFtools/data/expom_example2.csv"
#First save and import the .csv file that came with the package:
write.table(expom_example2,file=my_filename1,sep="\t",quote=FALSE,row.names=FALSE)
my_data_in_Vm<-import_expom_RF(filename=my_filename1,prefix="EXPOM_",suffix="_Vm")
vars_to_convert<-c(names(my_data_in_Vm[grep("_Vm",names(my_data_in_Vm))]))
my_data_in_mWm2<-convert_Vm_to_mWm2(x=my_data_in_Vm,vars=vars_to_convert,old_suffix="_Vm",new_suffix="_mWm2")
}
\keyword{}
