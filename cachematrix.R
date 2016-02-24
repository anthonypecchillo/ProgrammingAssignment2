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
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}