idealCvector <- function(t1,t2) {
	nNi.vector <- as.vector( matrix(0,length(t1),1) )
	nt2 <- as.vector(matrix(0,1,length(t2)))

	for ( t1.i in 1:length(t1) ){
		if ( t1[t1.i] > max( Ni_data$RT1 ) ){
			carbon.id <- nrow(Ni_data) - 1
		} else {
			carbon.id <- min( which(  t1[t1.i] - Ni_data$RT1  <= 0 ) ) - 1
		}
		if ( t1[t1.i] <= min(Ni_data$RT1) ){
			carbon.id <- min( which(  t1[t1.i] - Ni_data$RT1  <= 0 ) ) 
		}

		###Interpolation for alkane
		No_C <- Ni_data$No_C[carbon.id] + (t1[t1.i]-Ni_data$RT1[carbon.id])*{(Ni_data$No_C[carbon.id+1] - Ni_data$No_C[carbon.id])/(Ni_data$RT1[carbon.id+1] - Ni_data$RT1[carbon.id])}
		nNi.vector[t1.i] <- No_C@#+ ( t1[t1.i] - Ni_data$RT1[carbon.id] ) / ( Ni_data$RT1[carbon.id+1] - Ni_data$RT1[carbon.id] )
		nt2[t1.i] <- Ni_data$RT2[carbon.id] + ( ( Ni_data$RT2[carbon.id+1] - Ni_data$RT2[carbon.id] ) / ( Ni_data$RT1[carbon.id+1] - Ni_data$RT1[carbon.id] ) ) * ( t1[t1.i] - Ni_data$RT1[carbon.id] )
	}
	nNi <- nNi.vector

	idealC <- list( nNi.fun.output = nNi, nt2.fun.output = nt2 )
	idealC
}
