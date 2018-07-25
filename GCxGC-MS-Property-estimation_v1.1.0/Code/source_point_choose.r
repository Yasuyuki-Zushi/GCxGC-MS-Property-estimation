if ( any(ls()=="RT1.click") && any(ls()=="RT2.click") ){
	point1 <- list(RT1.click, RT2.click)
	names(point1) <- c("x","y")
} else {
	point1 <<- locator(1)
}

cat("Selected peak")
writeLines(sep="\n","")
cat("RT1: ", point1$x, "RT2: ", point1$y)
writeLines(sep="\n","")



###Estimate a value at click point
setwd(codeDrive)
source("function_point_estimate.r")
point.res <- point.estimate(point1$x,point1$y)

###Remove variant
point.res.graph <- point.res[1:length(point.res)]
prop_coef.graph <- prop_coef[1:nrow(prop_coef),]

###Box plot
logP.lower.variant <- paste( rownames(prop_coef.graph), ".lower",sep="",collapse=",")
ylim.min <- eval(parse(text=  paste("min(",logP.lower.variant,")",sep="") ))-1

logP.upper.variant <- paste( rownames(prop_coef.graph), ".upper",sep="",collapse=",")
ylim.max <- eval(parse(text=  paste("max(",logP.upper.variant,")",sep="") ))


setwd(outputDrive)
windows(6,6)	
png(file=paste(Analyze_RTlist[list.i,2],"_boxplot.png",sep=""),bg="transparent")
par(mar=c(10,4,2,2))
boxplot(point.res.graph,
main=paste("Point ","RT1: ",round(point1$x,digits=3)," ","RT2: ",round(point1$y,digits=3)),
names=names(point.res.graph),ylim=c(ylim.min,ylim.max),cex.axis=1.5,las=2)


prop.text.list <- paste(rownames(prop_coef.graph),collapse=",")
prop.me.list <- t(as.matrix( eval(parse( text=paste("c(",prop.text.list,")",se="" ))) ))
colnames(prop.me.list) <- rownames(prop_coef.graph)
rownames(prop.me.list) <- "prop.me.list"


prop_coef.graph.all <- prop_coef[1:nrow(prop_coef),]

prop.text.list.all <- paste(rownames(prop_coef.graph.all),collapse=",")
prop.me.list.all <- t(as.matrix( eval(parse( text=paste("c(",prop.text.list.all,")",se="" ))) ))
colnames(prop.me.list.all) <- rownames(prop_coef.graph.all)
rownames(prop.me.list.all) <- "prop.me.list.all"



text(x=c(1:length(prop.me.list) ), ylim.min ,round( as.numeric(prop.me.list),digits=1) ,cex=1.1,col="red" )
dev.off()
dev.off()


###Spider chart
prop.upper.text <- paste(rownames(prop_coef),".upper",sep="",collapse=",")
prop.upper.text2 <- paste("c(",prop.upper.text,")",sep="")
prop.upper <- as.matrix(eval(parse(text= paste( prop.upper.text2 ) )))
rownames(prop.upper) <- rownames(prop_coef)
colnames(prop.upper) <- "prop.upper"
prop.upper <- t(prop.upper)

prop.lower.text <- paste(rownames(prop_coef),".lower",sep="",collapse=",")
prop.lower.text2 <- paste("c(",prop.lower.text,")",sep="")
prop.lower <- as.matrix(eval(parse(text= paste( prop.lower.text2 ) )))
rownames(prop.lower) <- rownames(prop_coef)
colnames(prop.lower) <- "prop.lower"
prop.lower <- t(prop.lower)

###Remove variant
prop.upper.graph <- prop.upper[1:length(prop.upper)]
names(prop.upper.graph) <- colnames(prop.upper)[1:length(prop.upper)]
prop.lower.graph <- prop.lower[1:length(prop.lower)]
names(prop.lower.graph) <- colnames(prop.lower)[1:length(prop.lower)]


Sys.sleep(0.1)

windows(6,6)
png(file=paste(Analyze_RTlist[list.i,2],"_profile.png",sep=""),bg="transparent")


start.v <- 1.56

radial.range <- c(-20,15)
radial.plot( prop.me.list, labels=colnames(prop.me.list),rp.type="p",start=start.v,
radial.lim=radial.range,clockwise=T,add=F,mar=c(2,4,4,4),lwd=2,
main=paste("Point ","RT1: ",round(point1$x,digits=3)," ","RT2: ",round(point1$y,digits=3)) )

radial.plot( prop.upper.graph, rp.type="p",start=start.v,lwd=1,radial.lim=radial.range,line.col="blue",clockwise=T,lty=2,add=T )
radial.plot( prop.lower.graph, rp.type="p",start=start.v,lwd=1,radial.lim=radial.range,line.col="blue",clockwise=T,lty=2,add=T )


dev.off()
dev.off()


cat("Variant: ","prop.me.list", ": Medians of the property value")
writeLines(sep="\n","")
print(t(prop.me.list))
cat("Variant: ","prop.lower", ": 95% lower conf. of the property value")
writeLines(sep="\n","")
print(t(prop.lower.graph))
cat("Variant: ","prop.upper", ": 95% upper conf. of the property value")
writeLines(sep="\n","")
print(t(prop.upper.graph))
