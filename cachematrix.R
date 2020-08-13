## These functions demonstrate the importance of R's lexical scoping by using
## the caching of a matrix inverse

## This function creates an R object that stores a vector and its inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL # Initialise objects
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function uses the first function to get the inverse of the cached value
## stored in the first function's environment, if it has not changed

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}    ## Return a matrix that is the inverse of 'i'

