##  Below are two functions that are used to create a special object that 
##  stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <- function(y) {
            x <<- y
            I <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) I <<- inverse
      getInverse <- function() I
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## The following function solves the inverse of the special "vector" created with 
## the above function. However, it first checks to see if the inverse already been 
## solved. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it solves the inverse of the data and sets the value of the inverse
## in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
      I <- x$getInverse()
      if(!is.null(I)) {
            message("getting cached data")
            return(I)
      }
      data <- x$get()
      I <- solve(data, ...)
      x$setInverse(I)
      I
}