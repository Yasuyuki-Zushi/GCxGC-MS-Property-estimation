all.prop.point.estimate <- function (u1,u2) {
	###Confidence interval of LogP
	for ( coef.i in 1:nrow(prop_coef) ){
        	prop.name <- rownames(prop_coef)[coef.i]
        	logk.conf.mat <- paste(prop.name, "_conf.mat",sep="")
        	for (ramda.i in 1:No.ramda){
                	eval(parse(text=paste( logk.conf.mat," <<- ", ramda.mat.box[coef.i,1],"*u1 + ",ramda.mat.box[coef.i,2],"*u2 + ",ramda.mat.box[coef.i,3], sep="" )))
                	eval(parse(text=paste( prop.name, " <<- ", ramda.me.box[coef.i,1],"*median(u1) + ",ramda.me.box[coef.i,2],"*median(u2) + ",ramda.me.box[coef.i,3], sep="" )))
                	if (all( is.nan(logk.conf.mat) )==F){
                        	eval(parse(text=paste( prop.name, ".upper <<- quantile(",logk.conf.mat,",c(0.975) )",sep="")))
                        	eval(parse(text=paste( prop.name, ".lower <<- quantile(",logk.conf.mat,",c(0.025) )",sep="")))
                	} else {
                        	eval(parse(text=paste( prop.name, ".upper <<- NaN",sep="")))
                        	eval(parse(text=paste( prop.name, ".lower <<- NaN",sep="")))
                	}
        	}
	}
	cat( "Calculated:" )
	cat( "logP.upper "," logP.lower"," logP_conf.mat", sep="")
	writeLines(sep="\n","")

	cat("Calculated:" )
	cat( rownames(prop_coef) )
	writeLines(sep="\n","")
}
