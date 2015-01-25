## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Here we write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## inverseX defualt NULL which is used to test whether the inversed matix has been computed
## set method is thought to be the only way to modify the cached matrix value
## use identical to test whether the matix is changed, if not, no need to recompute inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL
  get <- function() x
  set <- function(y){
    if(identical(x,y)){
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
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inverse of matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

## test cases:
## >m <- matrix(1:4,2,2)
## >m1 <- matrix(1:4,2,2)
## >m2 <- matrix(2:5,2,2)
## >cm <- makeCacheMatrix(m)
###### first time solve the matrix #####
## >cacheSolve(cm)
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##### second time solve #####
## >cacheSolve(cm)
## getting cached inverse of matrix
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##### change value by set method, value not changed #####
## >cm$set(m1)
## same matrix value, no need to reset and clear inverse
##### third solve with value not changed #####
## >cacheSolve(cm)
## getting cached inverse of matrix
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##### change value by set method, value changed #####
## >cm$set(m2)
##### forth solve with value changed #####
## >cacheSolve(cm)
##        [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1