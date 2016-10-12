## These functions cache the inverse of a provided matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
           m <- NULL
           set <- function(y) {
             x <<- y
             m <<- NULL
           }
           get <- function() x
           setmatrixInverse <- function(matrix) m <<- matrix
           getmatrixInverse <- function() m
           list(set = set, get = get,
                setmatrixInverse = setmatrixInverse,
                getmatrixInverse = getmatrixInverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
           m <- x$getmatrixInverse()
           print(m)
           if(!is.null(m)) {
             print("getting cached data")
             return(m)
           }
           data <- x$get()
           m <- solve(data, ...)
           x$setmatrixInverse(m)
           print("blah")
           m
}
