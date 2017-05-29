## Some computations can be very time consuming to get done. Cache can help us
## to simplify the process by storing an element in memory that will later be used
## by a different funciont. In this case, we are going to use the cache to develop a function
## that will allow us to invert a matrix.

## The first function will create a new matrix that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The second function will return the inverse of a matrix by using 
## cached data when available thanks to the previous function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        matx <- x$get()
        inv <- solve(matx, ...)
        x$setInverse(inv)
        inv
}



