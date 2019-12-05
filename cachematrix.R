## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly
## Below two functions attempt to implement the calculation and retrieval of cached inverse of any invertible matrix

## The function makeCacheMatrix implements the structure to store the matrix as well as its cached inverse. 
## It returns a list of four functions to retrieve/initialise the matrix as well as its inverse respectively

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  list(
    set = function(y){
      x <<- y
      inverse <<- NULL
    },
    get = function() x,
    setinverse = function(inv) inverse <<- inv,
    getinverse = function() inverse
  )
}


## The function cacheSolve uses the special matrix structure that is created by the function makeCacheMatrix to return its inverse.
## The inverse of an unchanged matrix is only calculated once and is stored as a cache for subsequent retrievals.
## At the time of initialisation, the inverse saved in the cache is reset to NULL thereby forcing the function to recaclclate the updated inverse for the first time.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
