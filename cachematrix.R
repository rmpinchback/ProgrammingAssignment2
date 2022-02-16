## Week 3 Assignment, Programming Assignment 2
##
## Computing a matrix inverse can be computationally expensive.
## The functions here make use of a cache so that inverses are only
## computed the first time.  makeCacheMatrix() supplies a list of 4
## functions to handle the details of the caching, and cacheSolve()
## is the function that either returns a cached inverse, or computes
## the inverse and caches it before returning the result.



## Provides a list of 4 functions used to maintain and access 
## a cached inverse of the supplied matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Examines the cache to see if the matrix inverse is already present
## and if so returns it; if the inverse is not present, computes the
## inverse, caches it, and then returns it.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
