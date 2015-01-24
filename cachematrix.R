## These functions implement a series of matrix operations where the inverse of a stored matrix is cached.
## If a stored matrix had not been changed since the last inversion was computed, the cached inverse
## would be returned if queried, thus reducing computational needs.  
## Any matrix provided to be stored is presumed to be invertible. 
##--------------------------------------------------------------------------------------------

## makeCacheMatrix
## This function implements 4 basic operations on a stored matrix and its corresponding inverse:
## set: this sets the content of a stored matrix (the provided matrix is presumed to be invertible)
## get: this retrieves the content of a stored matrix
## setinverse: this sets the content of the inverse of the stored matrix
## getinverse: this retrieves the content of the inverse; the cached inverse would be returned if applicable

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(i) {
    inverse <<- i
  }
  
  getinverse <- function() {
    inverse
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cachesolve
## This function calculates the inverse of the stored matrix.  If the inverse was previously calculated, 
## then the cached inverse value is returned.  Otherwise, the inverse is calculated and returned.

cachesolve <- function(x, ...) {
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
