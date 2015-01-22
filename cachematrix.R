## The use of both functions will allow you to cache the inverse of a matrix,
## so that when you need it, it can be looked up in the cache rather 
## than recomputed.

## This function makeCacheMatrix creates a special "vector", containing 
## functions: to set the value of the matrix, get the value of the matrix,
## set the value of the inverse, or get the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	setmat <- function(y) {
		x <<- y
		inv <<- NULL
	}
	getmat <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(setmat = setmat, getmat = getmat, 
		setinv = setinv, getinv = getinv)
}


## This function cacheSolve calculates the inverse of 'x' if needed. 
## It checks if the inverse has already been calculated. If so,
## it gets the inverse from the cache; otherwise, it calculates the inverse 
## and sets the value in the cache via setinv function.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}       
	data <- x$getmat()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
