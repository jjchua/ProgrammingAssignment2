##  Programming Assignment 2
##
##  The aim of these functions is to use caching to avoid
##  the cost of having to re-compute the inverse of a matrix 
##  repeatedly whenever the inverse is required.

##  @brief  makeCacheMatrix() creates a "special matrix" object 
##          that can cache its inverse.
##
##  @param  x is the plain matrix from which the new special 
##          matrix object will be constructed (default is 1x1
##          matrix containing NA).  x must be nonsingular.
##
##  @return list of accessor and mutator methods for x
##          and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Private variable for caching x-inverse
  xinv <- NULL
  
  ## Mutator method for x
  ## The cache should be cleared whenever x is updated.
  set <- function(newx) {
    x <<- newx
    xinv <<- NULL
  }
  
  ## Accessor method for x
  get <- function() {
    x
  } 
  
  ## Mutator method for x-inverse
  setinverse <- function(newxinv) {
    xinv <<- newxinv
  }
  
  ## Accessor method for x-inverse
  getinverse <- function() {
    xinv
  }
  
  ## Return public methods
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


##  @brief  cacheSolve() returns the inverse of a special matrix
##          x which was constructed using makeCacheMatrix() above. 
##          If the inverse has already been calculated (and the 
##          matrix has not changed), then the cacheSolve() would 
##          retrieve the inverse from the cache (rather than repeat
##          the costly calculation of the inverse).
##
##  @param  x is the special matrix contructed using makeCacheMatrix().
##
##  @param  ... are the optional parameters for the solve() function.
##
##  @return Matrix that is the inverse of x.  (Note that the return
##          value is a plain matrix, rather than a special matrix
##          constructed using the makeCacheMatrix() above.)

cacheSolve <- function(x, ...) {

  ## Try retrieving cached inverse of x.
  ## If it was calculated previously, return it.
  xinv <- x$getinverse()
  if (!is.null(xinv)) {
    message("getting cached inverse")
    return(xinv)
  }
  
  ## If this was the first invocation for this x,
  ## calculate the inverse and store in cache.
  xinv <- solve(x$get(), ...)
  x$setinverse(xinv)
  xinv
}
