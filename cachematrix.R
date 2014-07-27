## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
computedMatrices <- list()
numberComputedMatrices <- 0
makeCacheMatrix <- function(x = matrix()) {
	## compare matrix to already loaded matrices
	if (numberComputedMatrices == 0) {
		# first item, compute inverse
		inv1 <- solve(x)
		computedMatrices <- list("mat"=NULL,"inverse" =NULL)
		numberComputedMatrices <- 0
	}
	else {
		for (i=1:numberComputedMatrices) {
			found <- FALSE
			#If this is a duplicate
			if(!(all.equal(computeMatrices[i]$m, x) != TRUE)) {
				## duplicate
				found <- TRUE
				inv1 <- computeMatrices[i]$inverse
			}
		}
		if (!found){
			#load a new matrix
			inv1 <- solve(x)
			computedMatrices <- list(computedMatrices,list("mat"=x,"inverse" = inv1))
			numberComputedMatrices <-  numberComputedMatrices + 1
		}
	}
	return inv1
}
			
			
			
			
				
				
	
	## If it has been computed, load the pre-computed inverse
	## else compute the matrix, and load it and inverse 
	## return this inverse
	


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## Call compute inverse
}
