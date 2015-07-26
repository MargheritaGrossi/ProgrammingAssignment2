## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The following two functions are used to cache the inverse
## of a matrix. We assume that the matrix supplied is always invertible.

## The function `makeCacheMatrix` creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	set <- function(y) {
	        x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


## The function `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	## If the inverse is already cached return it
	if(!is.null(inv)) {
	        message("getting cached data")
		return(inv)
	}
	## otherwise compute the inverse
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}

## Example:
## > x <- matrix(1:4, nrow=2, ncol=2)
## > m <- makeCacheMatrix(x)
## > s <- cacheSolve(m)
## > x %*% s
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1

