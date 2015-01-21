## Author : Nalpin  
##
## makeCacheMatrix: Returns a data structure (list) that 
##                  provide 4 functions to access the state (mutable 
##                  vars that represents matrix data and matrix inverse)
##                  of a provided matrix 
## cacheSolve :     Given a data structure created with makeCacheMatrix
##                  returns a cached inverse if it has already been calculated 
##                  (and the matrix data has not changed)

## makeCacheMatrix: Given a matrix
## Returns: a list of 4 functions to access or change 
## the state (matrix data and matrix inverse)
## $get      : returns the matrix data
## $set      : changes the matix data
## $getSolve : retuns the matrix inverse
## $setSolve : changes the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
	
	x.s <- NULL
	set <- function(m) {
		x <<- m
		x.s <<- NULL
	}
	get <- function() x
	setsolve <- function(s) x.s <<- s
	getsolve <- function() x.s
	
	list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)

}


## cacheSolve: Given an object created with makeCacheMatrix
## Returns: Matrix data inverse, cached version if it has
## previously calculated and matrix data unchanged
## otherwise call solve and set the new inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	if(!is.null(s)) 
		return(s)
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
