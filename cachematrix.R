## the two functions cache and compute the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, 
		setinverse = getinverse, 
		getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by
## makeCacheMatrix; if the inverse has already been calculated
## and the matrix has not changed, then this function should
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	x <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i ## Return a matrix that is the inverse of 'x'
}
