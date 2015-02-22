## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverseVar <- NULL
  setVar <- function(y) {
    x <<- y
    inverseVar <<- NULL
  }
  getVar <- function() x
  
  setinverse <- function(inverse) inverseVar <<- inverse
  
  getinverse <- function() inverseVar
  
  list(setVar=setVar, getVar=getVar, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inverseVar <- x$getinverse()
  if(!is.null(inverseVar)) {
    message("getting cached data.")
    return(inverseVar)
  }
  data <- x$getVar()
  inverseVar <- solve(data)
  x$setinverse(inverseVar)
  inverseVar

}
