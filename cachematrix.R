## The inverse of a Matrix
## It always time-wasting to inverse a matrix and there may be some advantages 
##to program this action, because it will save your time
## These functions are used to create a special "matrix" object and computes the inverse of the special "matrix"

## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmatrixinv <- function(inverse) m <<- inverse  
  getmatrixinv <- function() m
  
  list( set=set, get=get, setmatrixinv=setmatrixinv, getmatrixinv=getmatrixinv )
}


## cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getmatrixinv()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setmatrixinv(inv)
  
  return(inv)
  
}


