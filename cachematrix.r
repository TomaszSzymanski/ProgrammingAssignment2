## The function makeCacheMatrix creates a matrix 
## for setting and getting the value of the matrix 
## as well as setting and getting its inverse.
## If the inverse has already been calculated,
## result is retrieved from the cache.

makeCacheMatrix <- function(x = matrix()) {
  res <- NULL
  set <- function(y){
    x <<- y
    res <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) res <<- inverse
  getInverse <- function() res
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function compute inverse matrix or, if
## inverse matrix has already been calculated,
## retrieves result from the cache.

cacheSolve <- function(x, ...) {
  res <- x$getInverse()
  if(!is.null(res)){
  message("getting cached data")
    return(res)
  }
  data <- x$get()
  res <- solve(x, ...)
  x$setInverse(res)
  res
}
