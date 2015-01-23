## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Here we write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  get <- function() x
  set <- function(y){
    if(y==x){
      message("same matrix value, no need to reset and clear inverse")
    }else{
      x <<- y
      inverseX <<- NULL
    }
    
  }
  getInverse <- function() inverseX
  setInverse <- function(inv) inverseX <<- inv
  list(get = get, set = set, getInv = getInverse, setInv = setInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv
  if(!is.null(inv)) {
    message("getting cached inverse of matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
