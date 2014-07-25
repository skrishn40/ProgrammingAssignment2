## makeCacheMatrix returna a function with four inbuilt functions for get, set, getinverse and setinverse.
## GetInverse gets cached matrix value if available. 
## SetInverse uses the passed argument to set the cached value
## Get and Set functions set the source matrix and local variables

makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinverse <- function(mat) inv <<- mat
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve uses cached data if available otherwise it uses solve function to get the inverse of argument
cacheSolve <- function(x, ...) {
  inv1 <- x$getinverse()
  if(!is.na(inv1)) {
    message("getting cached data")
    return(inv1)
  }
  data <- x$get()
  inv1 <- solve(data, ...)
  x$setinverse(inv1)
  inv1
}