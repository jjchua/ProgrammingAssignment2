## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  
  set <- function(newx) {
    x <<- newx
    xinv <<- NULL
  }
  
  get <- function() {
    x
  } 
  
  setinverse <- function(newxinv) {
    xinv <<- newxinv
  }
  
  getinverse <- function() {
    xinv
  }
  
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  xinv <- x$getinverse()
  if (!is.null(xinv)) {
    message("getting cached inverse")
    return(xinv)
  }
  
  xinv <- solve(x$get(), ...)
  x$setinverse(xinv)
  xinv
}
