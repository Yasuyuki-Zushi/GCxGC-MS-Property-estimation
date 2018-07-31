if ( any(ls()!="conf95_sigma") ){
conf95_sigma <-1.96
}


ramda.me.box <- matrix(NA,nrow(prop_coef),No.ramda )
ramda.se.box <- matrix(NA,nrow(prop_coef),No.ramda )
ramda.conf.box <- matrix(NA,nrow(prop_coef),No.ramda )
ramda.mat.box <- matrix(NA,nrow(prop_coef),No.ramda )
for (coef.i in 1:nrow(prop_coef) ){
        prop.name <- rownames(prop_coef)[coef.i]
        for (ramda.i in 1:No.ramda){
                eval(parse( text=paste("ramda",ramda.i,prop.name," <- ","prop_coef$ramda",ramda.i,"[",coef.i,"]",sep="") ))
                #eval(parse( text=paste("ramda",ramda.i,prop.name,"_95conf <- ","prop_coef$Conf95_ramda",ramda.i,"[",coef.i,"]",sep="") ))
                eval(parse( text=paste("ramda",ramda.i,prop.name,"_se <- ","prop_coef$ramda",ramda.i,".se[",coef.i,"]",sep="") ))
                eval(parse( text=paste("ramda",ramda.i,prop.name,".mat <- ","rnorm(",montecarlo.rev,",ramda",ramda.i,prop.name,",ramda",ramda.i,prop.name,"_se)",sep="") ))
                
                ramda.me.box[coef.i,ramda.i] <- paste( "ramda",ramda.i,prop.name, sep="" )
                #ramda.conf.box[coef.i,ramda.i] <- paste( "ramda",ramda.i,prop.name,"_95conf", sep="" )
                ramda.se.box[coef.i,ramda.i] <- paste( "ramda",ramda.i,prop.name,"_se", sep="" )
                ramda.mat.box[coef.i,ramda.i] <- paste( "ramda",ramda.i,prop.name,".mat", sep="" )
                }
}


cat( "Calculated ramda1-3 for " )
cat( rownames(prop_coef) ) 
writeLines(sep="\n","")
cat ( "'Variant' " )
cat( "ramda.me.box:",ramda.me.box[1],"ramda.se.box:",ramda.se.box[1],"ramda.mat.box:",ramda.mat.box[1] ) 
writeLines(sep="\n","")
