## These pair of functions will return the inverse of a SQUARE matrix.
## In addition to returning the inverse, it will also cache the result so that if
## the matrix with exact same value is passed to it, it will retrieve from cache 
## instead of re-calculating.

## This function will return a list with functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix (in cache)
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function will calculate the inverse of the matrix created with the above function.
## It will skip computation and retrieve the result from cache if the function has 
## previously been performed on a similar matrix.

cacheSolve <- function(x, ...) {
      
	## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}
