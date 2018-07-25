sample.size=nrow(All_dataset.update)
seed.fix=TRUE


if (seed.fix==TRUE){
set.seed(1)
}
ext.rowid <-sample(1:nrow(All_dataset.update),size=sample.size)
train.megamix.forRamda <- All_dataset.update[ext.rowid,]

###Caluculate Ni*
setwd(codeDrive)
source("function_idealCvector.r")
megamix.train.C <- idealCvector(t1=train.megamix.forRamda$RT1,t2=train.megamix.forRamda$RT2)
nNi.megamix.train.forRamda <- megamix.train.C$nNi.fun.output
nt2.megamix.train.forRamda <- megamix.train.C$nt2.fun.output
t2.megamix.train.forRamda <- train.megamix.forRamda$RT2


logL1.train.forRamda <- alpha1 * nNi.megamix.train.forRamda + alpha2
logL2.train.forRamda <- alpha1.dush * nNi.megamix.train.forRamda + alpha2.dush + log10( {t2.megamix.train.forRamda-alpha3}/{nt2.megamix.train.forRamda-alpha3} )


u1.train <- logL1.train.forRamda
u2.train <- logL2.train.forRamda - beta.orth * logL1.train.forRamda


###Evaluate 2-parameter model
setwd(inputDrive)
logKdata <- read.csv("Exp_property_data.csv", header=T)


Env.SysP <- read.csv("Environ_System_Parameter.csv", header=T)


target.logK <- logK
if ( sum(which(Env.SysP$Property==target.logK))!=0 ){
	logK.ID <- which(Env.SysP$Property==target.logK)
	tar.SysP <- Env.SysP[logK.ID,]

	if ( nrow(tar.SysP)==0 ){
		logK.ASM <- as.data.frame(matrix(NA,sample.size,1 ))
	} else {

		###mechanical ASM choose
		###Default equation
		logK.ASM <- as.data.frame(as.matrix( #EL model for gas-liquid
		tar.SysP$e * train.megamix.forRamda$E + tar.SysP$s * train.megamix.forRamda$S +
		tar.SysP$a * train.megamix.forRamda$A + tar.SysP$b * train.megamix.forRamda$B + tar.SysP$l * train.megamix.forRamda$L + tar.SysP$c))


		if ( is.na(tar.SysP$l) ){ 
			logK.ASM <- as.data.frame(as.matrix( #EV model for liquid-liquid
			tar.SysP$e * train.megamix.forRamda$E + tar.SysP$s * train.megamix.forRamda$S +
			tar.SysP$a * train.megamix.forRamda$A + tar.SysP$b * train.megamix.forRamda$B + tar.SysP$v * train.megamix.forRamda$V + tar.SysP$c))
		}

		if ( is.na(tar.SysP$e) ){ #VL model for both
			logK.ASM <- as.data.frame(as.matrix(
			tar.SysP$l * train.megamix.forRamda$L + tar.SysP$s * train.megamix.forRamda$S +
			tar.SysP$a * train.megamix.forRamda$A + tar.SysP$b * train.megamix.forRamda$B + tar.SysP$v * train.megamix.forRamda$V + tar.SysP$c))
		}


		if ( is.na(tar.SysP$a || tar.SysP$b) ){
			logK.ASM <- as.data.frame(as.matrix(
			tar.SysP$e * train.megamix.forRamda$E + tar.SysP$s * train.megamix.forRamda$S +
			tar.SysP$ab * (train.megamix.forRamda$A*train.megamix.forRamda$B) + tar.SysP$v * train.megamix.forRamda$V + tar.SysP$c))
		}

		if ( is.na(tar.SysP$e) & is.na(tar.SysP$l) ){ #special case such as less descriptors
			logK.ASM <- as.data.frame(as.matrix(
			tar.SysP$s * train.megamix.forRamda$S +
			tar.SysP$a * train.megamix.forRamda$A + tar.SysP$b * train.megamix.forRamda$B + tar.SysP$v * train.megamix.forRamda$V + tar.SysP$c))
		}

	}

	colnames(logK.ASM) <- "logK.ASM"
	logK.cal <- cbind(train.megamix.forRamda[,1],train.megamix.forRamda$Name,logK.ASM)

} else {

	logK.ASM <- as.data.frame(matrix(NA,sample.size,1 )) 
	colnames(logK.ASM) <- "logK.ASM"
	logK.cal <- cbind(train.megamix.forRamda[,1],train.megamix.forRamda$Name,logK.ASM)
}


###Add ASM estimation to the place of non Exp data
target.chem.rowID <- match(logK.cal[,1],logKdata$chemID)
eval(parse(text=paste("logK.fill <- logKdata[target.chem.rowID,]$",logK,sep="") ))

if ( any(is.na(logK.fill)) ){
	logK.blank.id <- which(is.na(logK.fill))
	cat(paste("The No. of ASM value: ",length( logK.blank.id ),"\n",sep=""))
	cat(paste("The No. of Exp./inhouseDB value: ",sample.size-length( logK.blank.id ),"\n",sep=""))
	if ( any(is.na(logK.cal[,3])) ){
		logK.fill <- logK.fill[-c(logK.blank.id)]
		u1.train <- u1.train[-c(logK.blank.id)]
		u2.train <- u2.train[-c(logK.blank.id)]
		logK.cal@<- logK.cal[-c(logK.blank.id),]
	} else {
		logK.fill[logK.blank.id] <- logK.cal[logK.blank.id,3] 
	}
	data.from.DB.n <- sample.size-length( logK.blank.id )
} else {
	cat(paste("The No. of Exp./inhouseDB value: ",sample.size,"\n",sep=""))
	data.from.DB.n <- sample.size
}


res.lm.logK <- lm(logK.fill ~ u1.train + u2.train )
lm.logK <- summary( lm(logK.fill ~ u1.train + u2.train ) )
R2adj <- round(lm.logK$adj.r.squared,digits=2)
cat(paste(logK,", R2 adjusted:",round(lm.logK$adj.r.squared,digits=3),"\n",sep=""))
cat(paste("n: ",length(logK.fill),"\n\n",sep=""))


ramda1.logK <- lm.logK$coefficients[2,1]
ramda1.logK.SE <- lm.logK$coefficients[2,2]
ramda2.logK <- lm.logK$coefficients[3,1]
ramda2.logK.SE <- lm.logK$coefficients[3,2]
ramda3.logK <- lm.logK$coefficients[1,1]
ramda3.logK.SE <- lm.logK$coefficients[1,2]
sample.n <- length(lm.logK$residuals)
R2 <- round(lm.logK$adj.r.squared,digits=3)
RMSE.v <- {{sum({predict(res.lm.logK)-logK.fill}^2)}/length(logK.fill)}^0.5
Propetry.model.2para <- ramda1.logK * u1.train + ramda2.logK * u2.train + ramda3.logK
Property.data <- logK.fill

windows()
par(mar=c(5,5,3,2))
distance <- max(Propetry.model.2para)-min(Propetry.model.2para)
zoom.v <- 0.4
if ( any(is.na(logK.cal[,3])) ){
        plot(Property.data,Propetry.model.2para,col="red",pch=19,cex=2.5,cex.lab=1.8,cex.axis=1.8,xlab="Training data",ylab="Predicted data (GCxGC model)",
		xlim=c(min(Propetry.model.2para)-distance*zoom.v,max(Propetry.model.2para)+distance*zoom.v), ylim=c(min(Propetry.model.2para)-distance*zoom.v,max(Propetry.model.2para)+distance*zoom.v) )
} else {
	plot(Property.data,Propetry.model.2para,col="red",pch=19,cex=2.5,cex.lab=1.8,cex.axis=1.8,xlab="Training data",ylab="Predicted data (GCxGC model)",
		xlim=c(min(Propetry.model.2para)-distance*zoom.v,max(Propetry.model.2para)+distance*zoom.v), ylim=c(min(Propetry.model.2para)-distance*zoom.v,max(Propetry.model.2para)+distance*zoom.v) )
		points(Property.data[logK.blank.id], Propetry.model.2para[logK.blank.id], col="white",pch=19,cex=1.8)
}
text(Property.data,Propetry.model.2para,logK.cal[,2],cex=1)
abline(0,1,lwd=2);abline(a=1,b=1,lty=2);abline(a=-1,b=1,lty=2)
mtext(paste(logK,";  R-sqr(adj.) = ",R2adj,"; RMSE = ", round(RMSE.v,digits=3),"; n = ",length(Property.data),sep=""),adj=0,cex=1.3)


cat(paste("Ramda1: ",ramda1.logK,"\n","Ramda2: ",ramda2.logK,"\n","Ramda3: ",ramda3.logK,"\n\n",sep=""))
cat(paste("Ramda1 SE: ",ramda1.logK.SE,"\n","Ramda2 SE: ",ramda2.logK.SE,"\n","Ramda3 SE: ",ramda3.logK.SE,"\n",sep=""))
