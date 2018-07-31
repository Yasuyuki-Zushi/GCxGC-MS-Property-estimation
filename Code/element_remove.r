element_remove <- function(X,x,data.list=All_dataset.update){
	element <- X
	element.s <- x
	smiles <- as.character(data.list$SMILES)
	del.row.id <- 0
	for ( check.ii in 1:nrow(data.list)  ){
		smiles.split <- strsplit(smiles[check.ii],NULL)
		element.num <- sum(length(which(smiles.split[[1]]==element)) + length(which(smiles.split[[1]]==element.s)))
		if ( element.num>=1 ){ #Œ³‘f‚Ì”
			del.row.id <- rbind(del.row.id, check.ii)
		}
	}
	if ( length(del.row.id) > 1 ){
		del.row.id <- del.row.id[-1]
		All_dataset.update <<- data.list[-c(del.row.id),]
	} else {
		All_dataset.update <<- data.list
	}
	cat(X,"deleted,")
	cat("Use output as All_dataset.update")
}
