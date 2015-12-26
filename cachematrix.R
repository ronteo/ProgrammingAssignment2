## Performs matrix inversion utilzing cache


## Returns a list of functions that creates a special "matrix" capable of caching it's inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function(y) {
      x <<- y
      i <<- NULL
    }
  get <- function() x
  
  getInverse <- function() i
  setInverse <- function(inverse){i <<- inverse}
  
  list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
#If the inverse has already been computed, the cache result is returned
cacheSolve <- function(x, ...) {
      
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
