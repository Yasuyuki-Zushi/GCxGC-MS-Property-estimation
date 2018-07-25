setwd(outputDrive)
shaded.mat.box.ms <- matrix(NaN,col,row)
eval(parse( text=paste("shaded.mat.ms <- replace(shaded.mat.box.ms,", remain.condition,", 1)") ))
shaded.mat.multi <- shaded.mat.ms*zz1


shaded.mat.box <- matrix(0,col,row)
eval(parse( text=paste("shaded.mat <- replace(shaded.mat.box,", remain.condition,", 1)") ))


windows(width=10,height=5)
par(mar=c(5,7,1,11))
plot(NA,xlim=range(xx1),ylim=range(yy1),frame=FALSE,axes=F,xaxs="i",yaxs="i",ylab="",xlab="")
.filled.contour(x=xx1, y=yy1, z=zz1,levels=clevels,col=red_blue(cstep))


###Shaded the domain
par(new=T,mar=c(5,7,1,11),xaxs="i", yaxs="i", las=1)
.filled.contour(x=xx1, y=yy1, z=shaded.mat.ms,levels=seq(-10,10,1),col=rgb(1,0,0,0.25))


###Countor map and labels
par(new=T,mar=c(5,7,1,11),xaxs="i", yaxs="i", las=1,mgp=c(2.0, 1, 0))
contour(xx1,yy1,matrix(0,nrow(zz1),ncol(zz1)),xlab="GC1 (min)",ylab="GC2 (sec)", yaxs="i",col.lab="blue",col.axis="blue")

####Log Kow
par(new=T,mar=c(5,7,1,11),xaxs="i", yaxs="i", las=1,mgp=c(3, 1, 0))
contour(xx1,yy1,logKo_w.contour, yaxs="i",ylab="Log Kow",axes=FALSE,col=rgb(0,0,0,1),lwd=1.2,levels=seq(-30,30,1),labcex = 1.1)

####Log Koa
par(new=T, mar=c(5,7,1,11) )
contour(xx1,yy1,logKa_w.contour,xaxs="i", yaxs="i",axes=FALSE,col=rgb(1,0.5,0.5,1),lwd=1.2,levels=seq(-30,30,1),labcex = 1.1)
axis(1, xaxt ="n")
mtext("Log Kaw", side = 4, line = 3, las=3,col=rgb(1,0.5,0.5,1))

###Log Kaw
par(new=T,mar=c(5,7,1,11),xaxs="i", yaxs="i", las=1)
contour(xx1,yy1,logKo_a.contour,xaxs="i", yaxs="i",axes=FALSE,col=rgb(0.6,0.6,0.6,1),lwd=1.2,levels=seq(-30,30,1),labcex = 1.1 )
axis(4, yaxt ="n")
mtext("Log Koa", side = 1, line = 3, las=1,col=rgb(0.6,0.6,0.6,1) )
