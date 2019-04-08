all.prop.estimate <- function (u1,u2) {
	###LogP, LogP.contour 
        for ( coef.i in 1:nrow(prop_coef) ){
                prop.name <- rownames(prop_coef)[coef.i]
                for (ramda.i in 1:No.ramda){
                        eval(parse(text=paste( prop.name, " <<- ", ramda.me.box[coef.i,1],"*u1 + ",ramda.me.box[coef.i,2],"*u2 + ",ramda.me.box[coef.i,3], sep="" )))
                        eval(parse( text=paste( prop.name,".contour <<- t(matrix(",prop.name,",row,col) )",sep="") ))
                }
        }
	cat("Calculated:" )
	cat( rownames(prop_coef) )
	writeLines(sep="\n","")
}
