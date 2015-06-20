## Taking inverse of a matrix is a costly operation. In order to cache
## the inverse of a matrix, first a decorator function makeCacheMatrix
## creates the functions related to given matrix to taken inverse of.
## After creating the list of functions it can be stored to calculate
## inverse with cacheSolve using cache mechanism provided by 
## makeCacheMatrix function. cacheSolve function calculates the inverse
## once and after that it returns the cached data. Function also prints
## that it gets the inverse data from the cache

## makeCacheMatrix creates a state objects for given matrix 'x' and
## decorates the matrix with functions for caching its inverse

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


## cacheSolve functions takes a cache-ready matrix created with makeCacheMatrix function
## and returns the inverse of this matrix 'x' using functions provided by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
