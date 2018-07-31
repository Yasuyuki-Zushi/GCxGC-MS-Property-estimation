windows(width=10,height=5)
par(mar=c(5,7,1,1))
filled.contour(xx1,yy1,zz1,xlab="GC1 (min)",ylab="GC2 (sec)",col=red_blue(cstep),levels=clevels )

for (graph.i in 1:nrow(prop_coef) ){
        graph.name <- paste(rownames(prop_coef),".contour",sep="")[graph.i]
        graph.variant <- eval(parse(text=paste(graph.name)))
        level.box <- replace(graph.variant, is.nan(graph.variant),0)
        level.range <- round(quantile(level.box,c(0.005,0.995)))
        level.step <- seq(level.range[1],level.range[2],1)
        level.step

        ####Log K
        par(new=T,mar=c(5,7,1,11),xaxs="i", yaxs="i", las=1,mgp=c(3, 1, 0))
        contour(xx1,yy1,graph.variant, yaxs="i",axes=FALSE,col=rainbow(graph.i),lwd=1.2,levels=level.step,labcex = 1.1)
}

