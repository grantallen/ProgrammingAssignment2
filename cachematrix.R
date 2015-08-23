## Put comments here that give an overall description of what your
## functions do

## This function sets up an invertible matrix that can cache in the cacheSolve funciton


makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set=function(y) {
    x <<- y
    inv <<- NULL
    
  }
  get =function() x
  setinv=function(inverse) inv<<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Returns a matrix with the inverse of the matrix 'x'. 
## Solve function is looked up in the cache to save time 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
