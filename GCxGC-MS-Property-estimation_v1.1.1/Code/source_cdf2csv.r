library(RNetCDF)

###CDF open
setwd(inputDrive)
cdf.dat <- open.nc(mainFilename.cdf,write=TRUE)

MStotint <- var.get.nc(cdf.dat, "total_intensity")
scantime <- var.get.nc(cdf.dat, "scan_acquisition_time")
eachscannum <- var.get.nc(cdf.dat, "point_count")


Timepara <- scantime[which(abs(eachscannum)<.Machine$integer.max )]
RTini <- min( Timepara )/60
RTruntime <- max( Timepara )/60-RTini
SamRate <- 1/(mean( Timepara[2:length(Timepara)]-Timepara[1:(length(Timepara)-1) ] ))

setwd(TempDrive)
write.csv(MStotint,"Measurement_data.csv",row.names=F)