##------------------------------------------------------------------------------------------
## This file contains two functions.
##
## The first, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.
##
## The second, cacheSolve, computes the inverse of the special "matrix" returned by
## makeCacheMatrix.  If the inverse has already been calculated (and the matrix has not
## changed), then the cacheSolve function should retrieve the inverse from the cache. 
##------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x = matrix(), ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setInverse(inv)
    inv
}

