paste(rownames(prop_coef),".contour",sep="")

###Depiction of shaded chromatogram
###
setwd(codeDrive)
all.prop.estimate(u1,u2)
source("function_all_property_estimation.r")
source("source_shaded_chrom.r")

shaded.mat.export <- as.vector(t(shaded.mat.multi))
setwd(outputDrive)


output.name.csv <- paste(topic.name,"_Hz_",row/MPeriod,"_MP_",MPeriod,"_RTini_",RTini,"_",mainFilename,sep="")
write.csv(shaded.mat.export,output.name.csv, row.names=FALSE)

setwd(codeDrive)
output.name.cdf <- paste(topic.name,"_Hz_",round(row/MPeriod),"_MP_",MPeriod,"_RTini_",round(RTini,digits=2),"_phase.s_",phase.shift.cdf,"_",mainFilename.cdf,sep="")
source("MaskOnMSdata.r")
