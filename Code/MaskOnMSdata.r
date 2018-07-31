library("RNetCDF")

###CDF open
setwd(inputDrive)
cdf.dat <- open.nc(mainFilename.cdf,write=TRUE)

old.op <- options(max.print=9999999)
flag <- var.get.nc(cdf.dat, "flag_count")
scantime <- var.get.nc(cdf.dat, "scan_acquisition_time")
medmsmax <- var.get.nc(cdf.dat, "mass_range_max")
medmsmin <- var.get.nc(cdf.dat, "mass_range_min")
ionid <- var.get.nc(cdf.dat, "scan_index")
eachscannum <- var.get.nc(cdf.dat, "point_count")
MStotint <- var.get.nc(cdf.dat, "total_intensity")
MSvalue <- var.get.nc(cdf.dat, "mass_values")
MSint <- var.get.nc(cdf.dat, "intensity_values")

Timepara <- scantime[which(abs(eachscannum)<.Machine$integer.max )]
RTini <- min( Timepara )/60
RTruntime <- max( Timepara )/60-RTini
SamRate <- 1/(mean( Timepara[2:length(Timepara)]-Timepara[1:(length(Timepara)-1) ] ))

pixelnum <- length(flag)
maxscannum <- max(eachscannum)
minscannum <- min(eachscannum)
MSdatabox <- matrix(0,pixelnum, maxscannum+1 )
MSvaluebox <- MSdatabox
MSintbox <- MSdatabox

for (inum in 1:pixelnum) {
        if (abs(eachscannum[inum])<.Machine$integer.max){
                initial <- sum(eachscannum[1:inum])-eachscannum[inum]+1
                acqrange <- initial:(initial+eachscannum[inum]-1)
                if(eachscannum[inum]==0){acqrange <-0}
                remainrep <- rep(0,maxscannum-eachscannum[inum]+1 )
                MSvaluebox[inum,] <- c( MSvalue[ acqrange ], remainrep )
                MSintbox[inum,] <- c( MSint[ acqrange ], remainrep )
        }
}

rownames(MSvaluebox) <-1:(pixelnum)
rownames(MSintbox) <-1:(pixelnum)
MSvaluebox[which(MSintbox > 9e+36)] <- -1
MSintbox[which(MSintbox > 9e+36)] <- -1


row3 <- round(SamRate*MPeriod)
col3 <- round(length(scantime)/row3)
row3
col3
tic.box <- as.matrix( rowSums(MSintbox)  )




###length adjustment
shaded.mat.vector <- as.vector(t(shaded.mat))
format.mat.pre <- c(0,shaded.mat.vector)[1:length(shaded.mat.vector)]  #GCImage unieque; starting location=0.
###phase.shift adjustment
if (phase.shift.cdf < 0){
	add.pix.number <- round(phase.shift.cdf*SamRate)
	add.pix.vector <- rep(0,add.pix.number)
	format.mat <- c(add.pix.vector,format.mat.pre)[1:length(format.mat.pre)]
}
if (phase.shift.cdf > 0){
	reduc.pix.number <- round(phase.shift.cdf*SamRate)-1
	format.mat <- format.mat.pre[(reduc.pix.number+1):length(format.mat.pre)]
}
if (phase.shift.cdf == 0){
	format.mat <- format.mat.pre[1:length(format.mat.pre)]
}

if ( nrow(MSintbox) > length(format.mat) ){
	format.mat <- c(format.mat, rep(min(format.mat),nrow(MSintbox)-length(format.mat) ) )
}
if ( nrow(MSintbox) < length(format.mat) ){
	format.mat <- format.mat[1:nrow(MSintbox)]
}


MSintbox.multi <- MSintbox * (format.mat)


###CDF output
setwd(outputDrive)
if ( nrow(MSintbox.multi) > length(eachscannum) ){
        loop.length <- length(eachscannum)
}
if ( nrow(MSintbox.multi) <= length(eachscannum) ){
        loop.length <- nrow(MSintbox.multi)
}

for (i in 1:loop.length ) {
        if (abs(eachscannum[i])<.Machine$integer.max){
                initial <- sum(eachscannum[1:i])-eachscannum[i]+1
                acqrange <- initial:(initial+eachscannum[i]-1)
                if(eachscannum[i]==0){acqrange <-0}
                        MSvalue[acqrange] <- MSvaluebox[i, 1:length(acqrange) ]
                        MSint[acqrange] <- MSintbox.multi[i, 1:length(acqrange) ] #eval(parse(text=paste("MSintbox",sep="")))
                }
        }

ncnew <- create.nc( output.name.cdf,large=TRUE )

scan_number <- dim.def.nc(ncnew, dimname="scan_number", dimlength=length(scantime))
point_number <- dim.def.nc(ncnew, dimname="point_number", dimlength=length(MSint) ) 

var.def.nc(ncnew,  varname="flag_count", dimensions="scan_number",  vartype="NC_INT")
var.def.nc(ncnew, varname="scan_acquisition_time", dimensions="scan_number",vartype="NC_DOUBLE" )
var.def.nc(ncnew, varname="mass_range_max", dimensions="scan_number",vartype="NC_DOUBLE" )
var.def.nc(ncnew, varname="mass_range_min", dimensions="scan_number",vartype="NC_DOUBLE" )
var.def.nc(ncnew, varname="scan_index", dimensions="scan_number",vartype="NC_INT" )
var.def.nc(ncnew, varname="point_count", dimensions="scan_number",vartype="NC_INT" )
var.def.nc(ncnew, varname="total_intensity", dimensions="scan_number",vartype="NC_DOUBLE" )
var.def.nc(ncnew, varname="mass_values", dimensions="point_number",vartype="NC_DOUBLE" )
var.def.nc(ncnew, varname="intensity_values", dimensions="point_number",vartype="NC_FLOAT" )

var.put.nc(ncnew, variable="flag_count",data=flag)
var.put.nc(ncnew, variable="scan_acquisition_time",data=scantime)
var.put.nc(ncnew, variable="mass_range_max",data=medmsmax)
var.put.nc(ncnew, variable="mass_range_min",data=medmsmin)
var.put.nc(ncnew, variable="scan_index",data=ionid)
var.put.nc(ncnew, variable="point_count",data=eachscannum)
var.put.nc(ncnew, variable="total_intensity",data=MStotint)
var.put.nc(ncnew, variable="mass_values",data=MSvalue)
var.put.nc(ncnew, variable="intensity_values",data=MSint)
close.nc(ncnew)

