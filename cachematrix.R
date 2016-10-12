## Put comments here that give an overall description of what your
## functions cache inverse of matrix

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
          set <- function(y) {
            x <<- y
            m <<- NULL
          }
          get <- function() x
          setmatrix <- function(matrix) m <<- matrix
          getmatrix <- function() m
          list(set = set, get = get,
               setmatrix = setmatrix,
               getmatrix = getmatrix)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getmatrix()
          print(m)
          if(!is.null(m)) {
            print("getting cached data")
            return(m)
          }
          data <- x$get()
          m <- solve(data, ...)
          x$setmatrix(m)
          print("blah")
          m
}
