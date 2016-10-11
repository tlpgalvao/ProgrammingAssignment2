## Caching the inverse of a Matrix
## makeCacheMatrix and cacheSolve are functions that allow to cache
## the inverse of a matrix. This avoids the repetead computation
## of the Matrix inversion, which is computationally expensive. 


## makeCacheMatrix creates an object that can cache the inverse
## of a matrix.

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


## The cacheSolve function computes the inverse of a matrix created 
## by makeCacheMatrix. If the matrix is still the same and its inverse 
## has already been, then cacheSolve will the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     matrix <- x$get()
     inv <- solve(matrix, ...)
     x$setinverse(inv)
     inv
}

