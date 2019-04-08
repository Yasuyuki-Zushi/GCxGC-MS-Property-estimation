point.estimate <- function(x,y) {
	t2 <- rep(y, montecarlo.rev )

	####Calculate ideal Carbon
	setwd(codeDrive)
	source("function_idealCvector.r")
	idealC.point1 <- idealCvector(x,y)
	nNi <- rep( idealC.point1$nNi.fun.output, montecarlo.rev )
	nt2 <- rep(idealC.point1$nt2.fun.output, montecarlo.rev )

	LogL1 <- alpha1*nNi + alpha2
	LogL2 <- alpha1.dush*nNi + log10( (t2-alpha3) / (nt2-alpha3) ) + rep(alpha2.dush, montecarlo.rev)

	u1 <- LogL1
	u2 <- LogL2 - beta.orth*LogL1

	###Property estimation
	setwd(codeDrive)
	source("function_all_property_point_estimation.r")
	all.prop.point.estimate(u1,u2)

	###Montecarlo simulation
	prop.name.all <- rownames(prop_coef)[1:nrow(prop_coef)]
	prop.variant.list <- paste(prop.name.all,collapse=",")
	res.point.rowname <- paste(prop.variant.list,sep=",")

	logk.conf.mat <- paste(prop.name.all, "_conf.mat",sep="")

	eval(parse( text=paste( "res.point.",logk.conf.mat," <- ", logk.conf.mat,@sep="" ) ))
	text.res.point <- paste( "res.point.",logk.conf.mat,sep="",collapse=",") 
	eval(parse( text=paste( "res.point.list <- list(",text.res.point,")",sep="" ) ))

	names(res.point.list) <- rownames(prop_coef)
	res.point.list

}

