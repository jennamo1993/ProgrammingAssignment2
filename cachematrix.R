## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## This function sets the value, gets the value, sets the inverse and then gets the inverse.
## The cachesolve will compute the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

##Results below:
##> checkresults <- matrix(rnorm(4),2,2)
##> checkresults1 <- makeCacheMatrix(checkresults)
##> cacheSolve(checkresults1)
##            [,1]        [,2]
##[1,] -0.3987363 -0.74188109
##[2,]  0.5944739 -0.05660539
