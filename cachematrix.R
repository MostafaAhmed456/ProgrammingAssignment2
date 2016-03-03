## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
         
	    matrix_inverse <- NULL
        set <- function(z){
		
		    x <<- z
			matrix_inverse <<- NULL
		
        }
		get <-function() x
		set_inverse <-function(inverse) matrix_inverse <<- inverse
		get_inverse <- function() matrix_inverse
		list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
		 

}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" created by the above function
##If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		matrix_inverse <- x$get_inverse()
		if(!is.null(matrix_inverse)){
		  message("there is cached data")
		  return(matrix_inverse)
		
		}
		matrix<-x$get()
		matrix_inverse<-solve(matrix,...)
		x$set_inverse(matrix_inverse)
		matrix_inverse
		
}
