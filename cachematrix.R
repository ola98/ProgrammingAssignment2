## Description:
## Cache inverse of a matrix so subsequent calls for inverse of same matrix is retrieved from cache
## Assumes supplied matrix is always invertible.
##
## Test Case: 
##
## > M <- matrix(c(4, 2, 7, 6), 2, 2)
## > Mobj <- mm <- makeCacheMatrix(M)
## > cacheSolve(Mobj)
## > cacheSolve(Mobj)
##

## Take a matrix and return a cacheable object with setters and getters for the matrix.

makeCacheMatrix <- function(x = matrix()) {
	
    m <- NULL
    set <- function(y) {
	 x <<- y
       m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m

    list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## Cache inverse of matrix x if it already exists in the cache.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
		message("getting cached data")
    		return(m)
       }
   	 data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
        ## Return a matrix that is the inverse of 'x'
}
