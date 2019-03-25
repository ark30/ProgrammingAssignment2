## Pair of functions that cache the inverse of a matrix 

## This function creates a specific matrix that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  ## Cached inverse of a matrix 
  inv <- NULL 
  
  ## get/set for matrix 
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  
  ## get/set for matrix inverse 
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse 
  
  # return list of matrix functions
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}


## Finds the inverse of a matrix. If the inverse has already been calculated, the cached inverse is provided 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv) 
  }
  
  ## Find inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  ## Cache inverse 
  x$setinv(inv)
  
  ## Return the inverse
  return(inv)
}
