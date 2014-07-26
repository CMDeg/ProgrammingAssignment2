## The first function creates a list of functions for storing the inverse of a matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL 
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set=set, get=get, 
		setInverse=setInverse,
		getInverse=getInverse)
}


## cacheSolve verifies first, if there is an inverse matrix already stored in cache. If yes, it retrieves the inverse matrix.
# If not, the inverse matrix is calculated, using the setInverse function.


cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
