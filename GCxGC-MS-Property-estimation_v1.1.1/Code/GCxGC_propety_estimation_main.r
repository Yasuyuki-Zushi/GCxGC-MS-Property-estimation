library(plotrix)
library(pracma)
library(RNetCDF)


###Input file preparation
mainFilename.cdf <- Input_measuremet_data
phase.shift.cdf <- phase.shift <- Phase_Shift


###cdf to csv
setwd(codeDrive)
source("source_cdf2csv.r")


mainFilename <- "Measurement_data.csv"
MPeriod <- Modulation_Period

montecarlo.rev <- 10000
No.ramda <- 3
No.alpha <- 3


setwd(TempDrive)
mainFile <- as.numeric( scan(mainFilename, what="",skip=1 ) )
RTruntime <- length(mainFile)/SamRate/60


###Information of alkane
setwd(inputDrive)
RTdata_Alkane <- read.csv("Ni_data_Alkane.csv",header=T)
###Precondition; Remove NA row
RTdata_Alkane_remNA <- RTdata_Alkane[complete.cases(RTdata_Alkane),]
RTdata_Alkane_remNA <- RTdata_Alkane_remNA[-which(RTdata_Alkane_remNA$Literature=="percepta"),]

###Alkanes
setwd(inputDrive)
Ni_data <- RTdata_Alkane_remNA

if ( parameter_calibration == "Yes" ){

        ###Information of calibration chemicals
        setwd(inputDrive)
        RTdata_MM <- read.csv("Chemical_list_for_LFER_Caliration.csv", header=T)
        ###Precondition; Remove NA row
        RTdata_MM_remNA <- RTdata_MM[complete.cases(RTdata_MM),]
        ###For 2 parameter Eq
        megamix.smiles <- RTdata_MM_remNA$SMILES



        ###Combine chemical list for LFER calibration
        ###Delete alkane(s), which are the longest (with neighbors counted by "del.long.alkane")
        del.long.alkane <-2
        alkane.ext.num <- seq(1,nrow(RTdata_Alkane_remNA)-del.long.alkane,2)
        RTdata_Alkane_remNA[alkane.ext.num ,1:13]
        All_dataset.update <- rbind(RTdata_MM_remNA[,1:13],RTdata_Alkane_remNA[alkane.ext.num ,1:13])
        All_dataset.update <- All_dataset.update[order(All_dataset.update$RT1),]


        ###Remove elements except for CHX from the combined chemical list
        setwd(codeDrive)
        source("element_remove.r")
        element_remove("N","n")
        element_remove("P","p")
        element_remove("O","o")
        element_remove("S","s")
        element_remove("N","n")
        nrow(All_dataset.update)


        ###Alkanes
        #setwd(inputDrive)
        #Ni_data <- RTdata_Alkane_remNA


        ###Alpha value's parameterization from DB1
        train.megamix <- All_dataset.update

        setwd(inputDrive)
        Column.SysP.data <- read.csv("Column_System_Parameter.csv",header=T)
        Column1.SysP <- Column.SysP.data[which(Column.SysP.data$Stationary.phase==first.column),]


        logL1.train <-Column1.SysP$e*train.megamix$E + Column1.SysP$s*train.megamix$S + Column1.SysP$a*train.megamix$A +
         Column1.SysP$b*train.megamix$B + Column1.SysP$l*train.megamix$L + Column1.SysP$c 

        ###Caluculate Ni*
        setwd(codeDrive)
        source("function_idealCvector.r")
        megamix.train.C <- idealCvector(t1=train.megamix$RT1,t2=train.megamix$RT2)


        nNi.megamix.train <- megamix.train.C$nNi.fun.output
        nt2.megamix.train <- megamix.train.C$nt2.fun.output
        t2.megamix.train <- train.megamix$RT2

        lm.logL1 <- summary( lm(logL1.train~nNi.megamix.train) )
        lm.logL1
        logL1.alpha1 <- lm.logL1$coefficients[2,1]
        logL1.alpha1.SE <- lm.logL1$coefficients[2,2]
        logL1.alpha2 <- lm.logL1$coefficients[1,1]
        logL1.alpha2.SE <- lm.logL1$coefficients[1,2]


        ###Alpha value parameterization of BPX50
        setwd(inputDrive)
        nAlkaneDescrptor <- read.csv("Ni_data_Alkane.csv",header=T)
        nAlkaneDescrptor <- nAlkaneDescrptor[-which(nAlkaneDescrptor$Literature=="percepta"),]

        Column.SysP.data <- read.csv("Column_System_Parameter.csv",header=T)
        Column2.SysP <- Column.SysP.data[which(Column.SysP.data$Stationary.phase==second.column),]

        logL2.train.alkane <-Column2.SysP$e*nAlkaneDescrptor$E + Column2.SysP$s*nAlkaneDescrptor$S + Column2.SysP$a*nAlkaneDescrptor$A +
         Column2.SysP$b*nAlkaneDescrptor$B + Column2.SysP$l*nAlkaneDescrptor$L + Column2.SysP$c 


        lm.logL2 <- summary( lm(logL2.train.alkane~nAlkaneDescrptor$No_C) )
        lm.logL2
        logL2.alpha1.dush <- lm.logL2$coefficients[2,1]
        logL2.alpha1.dush.SE <- lm.logL2$coefficients[2,2]
        logL2.alpha2.dush <- lm.logL2$coefficients[1,1]
        logL2.alpha2.dush.SE <- lm.logL2$coefficients[1,2]


        logL2.train <-Column2.SysP$e*train.megamix$E + Column2.SysP$s*train.megamix$S + Column2.SysP$a*train.megamix$A +
         Column2.SysP$b*train.megamix$B + Column2.SysP$l*train.megamix$L + Column2.SysP$c 

        ###Perform nls
        data.nls.logL2 <- as.data.frame( cbind( logL2.train, nNi.megamix.train, t2.megamix.train, nt2.megamix.train) )

        nls.logL2 <- summary( nls(logL2.train ~ logL2.alpha1.dush * nNi.megamix.train + logL2.alpha2.dush +
        log10( {t2.megamix.train-logL2.alpha3}/{nt2.megamix.train-logL2.alpha3} ),data=data.nls.logL2, start=list(logL2.alpha3=-0.5) ) )
        nls.logL2

        logL2.alpha3 <- nls.logL2$coefficients[1,1]
        logL2.alpha3.SE <- nls.logL2$coefficients[1,2]


        alpha1 <- logL1.alpha1
        alpha2 <- logL1.alpha2
        alpha3 <- logL2.alpha3

        alpha1.dush <- logL2.alpha1.dush
        alpha2.dush <- logL2.alpha2.dush

        cat("alpha1: ",alpha1, ", alpha2: ",alpha2, ", alpha3: ", alpha3, ", alpha1.dush: ", alpha1.dush, ", alpha2.dush: ", alpha2.dush)
        gs <- gramSchmidt( cbind( logL1.train, logL2.train ) )

        beta.orth <- gs$R[1,2]/gs$R[1,1]


        ###Parameterization for logP = ramda1*u1 + ramda2*u2 + ramda3
        setwd(inputDrive)
        exp.prop.data <- read.csv("Exp_property_data.csv", header=T)

        logK.id.list <- 4:ncol(exp.prop.data) #4:15 as a default


        prop_coef <- as.data.frame( matrix(NA,length(logK.id.list),12) )
        rownames(prop_coef) <- colnames( exp.prop.data )[logK.id.list]
        colnames(prop_coef) <- c("ramda1","ramda2","ramda3","ramda1.se","ramda2.se","ramda3.se","R2","unit","RMSE","training.data.min","training.data.max","n_from_DB")


        count.i <- 1
        for (input.id in logK.id.list ){

        	colnames( exp.prop.data )
        	target.logK = colnames( exp.prop.data )[input.id]

        	setwd(codeDrive)
        	logK@=@target.logK
        	source("source_logK_estimation.r")


        	prop_coef$ramda1[count.i] <- ramda1.logK
        	prop_coef$ramda2[count.i] <- ramda2.logK
        	prop_coef$ramda3[count.i] <- ramda3.logK
        	prop_coef$ramda1.se[count.i] <- ramda1.logK.SE
        	prop_coef$ramda2.se[count.i] <- ramda2.logK.SE
        	prop_coef$ramda3.se[count.i] <- ramda3.logK.SE
        	prop_coef$R2[count.i] <- R2
        	prop_coef$unit[count.i] <- "see other"
        	prop_coef$RMSE[count.i] <- RMSE.v
        	prop_coef$training.data.min[count.i] <- min(logK.fill)
        	prop_coef$training.data.max[count.i] <- max(logK.fill)
        	prop_coef$n_ASM.in.PCcalc[count.i] <- data.from.DB.n
        	
        	count.i <- count.i + 1
        }
        default.prop_coef <- prop_coef


        default.alpha <- t( c(alpha1, alpha2, alpha3, alpha1.dush, alpha2.dush, beta.orth) )
        colnames(default.alpha) <- c("alpha1","alpha2","alpha3","alpha1.dush","alpha2.dush","beta.orth")


        setwd(inputDrive)
        write.csv(default.alpha, "default.alpha.csv")
        write.csv(default.prop_coef, "default.prop_coef.csv")
}

if ( parameter_calibration == "No" ){
	setwd(codeDrive)
	source("source_default.parameter.use.r")
}






###ramda.box
setwd(codeDrive)
source("source_Coeff_PE.r") #,echo=T


###Original Chromatogram
setwd(codeDrive)
source("source_basic_chromato_xx1yy1zz1.r") #,echo=T


###Contour map
setwd(codeDrive)
source("function_gcxgc_contour.r")
gcxgc_contour(xx1)


####ideal Carbon for all pixel
setwd(codeDrive)
source("function_idealCvector.r")
whole.chrom.C <- idealCvector(t1,t2)
nNi <- whole.chrom.C$nNi.fun.output
nt2 <- whole.chrom.C$nt2.fun.output


LogL1 <- alpha1*nNi + alpha2
LogL2 <- alpha1.dush*nNi + log10( (t2-alpha3) / (nt2-alpha3) ) + alpha2.dush

u1 <- LogL1
u2 <- LogL2 - beta.orth*LogL1


###Property estimation for all pixel
setwd(codeDrive)
source("function_all_property_estimation.r")
all.prop.estimate(u1,u2)


cstep <- 100
cfactor <- 3.5
clevels <- seq(0,(max(mainFile/1)+0.1)^(1/cfactor),length.out=cstep )^(cfactor)


windows(width=10,height=5)
par(mar=c(5,7,1,1))
filled.contour(xx1,yy1,zz1,xlab="GC1 (min)",ylab="GC2 (sec)", col=red_blue(cstep),levels=clevels[1:cstep] )


###Click points for property estimation
setwd(codeDrive)
source("function_all_property_estimation.r")
all.prop.estimate(u1,u2)


setwd(inputDrive)
Analyze_RTlist <- read.csv("Input_RTs.csv", header=T)


###Execute property estimation for all the click points
list.i <- 1 #initialization
for (list.i in 1:nrow(Analyze_RTlist)  ){

	RT1.click <- Analyze_RTlist[list.i,4]
	RT2.click <- Analyze_RTlist[list.i,5]
	setwd(codeDrive)
	source("source_basemap_property.r")

	setwd(outputDrive)
	write.csv(point.res,paste("RT1_",Analyze_RTlist[list.i,4],"RT2_",Analyze_RTlist[list.i,5],"_bootstrap.csv",sep="") )

}


###Calculate potential
setwd(codeDrive)
source("source_domain_of_potential.r")

setwd(codeDrive)
source("source_ChromatoClip.r")



###end