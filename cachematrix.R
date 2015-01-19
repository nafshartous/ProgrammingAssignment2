## Functions that facilitate a matrix object that caches it's inverse

## makeCacheMatrix constructs a set of functions for manipulating
## a matrix and it's cached inverse value.  

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() { x }
  setInverse <- function(i)  { inverse <<- i }
  getInverse <- function() { inverse }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve takes as a paremeter the list object created by
## makeCacheMatrix.  It then checks the cache before attempting to 
## compute the inverse and update the cache.  

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    message("getting cached data")
    
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  
  inverse
}
