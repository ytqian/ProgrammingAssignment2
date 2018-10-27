## Programming Assignment 
## Getting the input matrix, set its value and its inverse matrix.
## Return the inverse matrix of the original input as the result.

## Basically this section of the code is copied from the example, 
## but instead of caching the mean value of a vector, now it caches
## the inverse matrix of a matrix input. All means are replaced by 
## inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<-inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Again, the code is pretty much alike to the example. In this part,
## the function retrieve a matrix, first check if the matrix has any
## NULL values, and then proceed to use Solve function to calculate 
## the inverse matrix, presuming all inputs matrices are invertible.
## THe function will return the inverse matrix of the input at the end.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
