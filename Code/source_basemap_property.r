if ( list.i <= 1 ){
	windows(width=10,height=5)

	par(mar=c(5,7,1,11))
	plot(NA,xlim=range(xx1),ylim=range(yy1),frame=FALSE,axes=F,xaxs="i",yaxs="i",ylab="",xlab="")
	.filled.contour(x=xx1, y=yy1, z=zz1,levels=clevels,col=red_blue(cstep))

	par(new=T,mar=c(5,7,1,11),xaxs="i", yaxs="i", las=1,mgp=c(2.0, 1, 0))
	contour(xx1,yy1,matrix(0,nrow(zz1),ncol(zz1)),xlab="GC1 (min)",ylab="GC2 (sec)", yaxs="i",col.lab="blue",col.axis="blue")


	####Log Kow
	par(new=T,mar=c(5,7,1,11),xaxs="i", yaxs="i", las=1,mgp=c(3, 1, 0))
	contour(xx1,yy1,logKo_w.contour, yaxs="i",ylab=paste("Log Ko_w"),axes=FALSE,col=rgb(0,0,0,1),lwd=1.2,levels=seq(-30,30,1),labcex = 1.1)


	####Log Kaw
	par(new=T,mar=c(5,7,1,11),xaxs="i", yaxs="i", las=1)
	contour(xx1,yy1,logKa_w.contour,xaxs="i", yaxs="i",axes=FALSE,col=rgb(0.6,0.6,0.6,1),lwd=1.2,levels=seq(-30,30,1),labcex = 1.1 )
	axis(4, yaxt ="n")
	mtext("Log Ka_w", side = 4, line = 3, las=3,col=rgb(0.6,0.6,0.6,1) )


	####Log Koa
	par(new=T, mar=c(5,7,1,11) )
	contour(xx1,yy1,logKo_a.contour,,xaxs="i", yaxs="i",axes=FALSE,col=rgb(1,0.5,0.5,1),lwd=1.2,levels=seq(-30,30,1),labcex = 1.1)
	axis(1, xaxt ="n")
	mtext("Log Koa", side = 1, line = 3, las=1,col=rgb(1,0.5,0.5,1))

}

###Obtain click point(s)
setwd(codeDrive)
source("source_point_choose.r")

RT1.memo <- point1$x
RT2.memo <- point1$y
suppressWarnings( rm( point1, RT1.click, RT2.click ) )


RT1.memo.list <- rep(RT1.memo, length(prop.me.list.all))
RT2.memo.list <- rep(RT2.memo, length(prop.me.list.all))
RT_for_estim <- cbind( RT1.memo.list, RT2.memo.list)
estim.result <- round(cbind( t(prop.lower), t(prop.me.list.all), t(prop.upper) ),digits=3)
estim.table <- cbind( estim.result, RT_for_estim )

setwd(outputDrive)
write.csv( estim.table, paste("ID",list.i, "RT1_", RT1.memo, "RT2_", RT2.memo,".csv", sep="") )


if ( list.i <= 1 ){
	lower.estim.table <- estim.table[,1]
	med.estim.table <- estim.table[,2]
	upper.estim.table <- estim.table[,3]
} else {
	lower.estim.table <- rbind(lower.estim.table, estim.table[,1])
	med.estim.table <- rbind(med.estim.table, estim.table[,2])
	upper.estim.table <- rbind(upper.estim.table, estim.table[,3])	
}

if ( list.i == nrow(Analyze_RTlist) ) {
	lower.estim.table <- cbind(Analyze_RTlist$Retention.I..min., Analyze_RTlist$Retention.II..sec., lower.estim.table )
	med.estim.table <- cbind(Analyze_RTlist$Retention.I..min., Analyze_RTlist$Retention.II..sec., med.estim.table )
	upper.estim.table <- cbind(Analyze_RTlist$Retention.I..min., Analyze_RTlist$Retention.II..sec., upper.estim.table )

	colnames(lower.estim.table) <- colnames(med.estim.table) <- colnames(upper.estim.table) <- c("RT1 (min)","RT2 (sec)", rownames(estim.table) )

}





