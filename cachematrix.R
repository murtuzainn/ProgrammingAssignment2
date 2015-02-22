##  This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

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


## This function returns the inverse of the matrix and 
## assumes that the matrix is always invertible. 
## It first checks if the inverse has already been computed. If so, 
## it gets the result and skips thecomputation. If not, 
## it computes the inverse, sets the value in the cache via
## setinverse function. 

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
