################################################
##                                            ##
##  Author: Francisco J. Chavez               ##
##                                            ##
##  Course: Coursera, R Programming           ##
##                                            ##
##  This program provides 4 functions         ##
##                                            ##
##  1.  makeCacehMatrix                       ##
##  2.  cacheSolve                            ##
##                                            ##
################################################

makeCacheMatrix <- function(x = matrix()) {

  ##  This fuction creates a special matrix object
  ##  that can be cache its inverse
  
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  setInverse <- function() m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
  
}


cacheSolve <- function(x, ...) {

  ## This functions computes the inverse of a special matrix
  ## Returned by an object of type makeCacheMatrix.
  
  m <- x$getInverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- m$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}


