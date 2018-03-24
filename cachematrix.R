## makeCacheMatrix returna a function with four inbuilt functions for get, set, getinverse and setinverse.
## GetInverse gets cached matrix value if available. 
## SetInverse uses the passed argument to set the cached value
## Get and Set functions set the source matrix and local variables

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL      # set inv variable to NULL
  set <- function(y) {     # set value of matrix in parent environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x      # get value of matrix argument
  setinverse <- function(inverse) inv <<- inverse  # sets the parent variable to inverse argument
  getinverse <- function() inv                    # gets the inverse value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function will accept special "Vector" of type makeCacheVector as argument
# it will provide cache value if already available otherwise it will solve the inverse
# and store in cache for future retrieval

cacheSolve <- function(x, ...) {  # X = special vector of type makeCacheMatrix
  inverse <- x$getinverse()       # get cache value if available
  if(!is.null(inverse)) {         # if not null, use it
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()                 
  inverse <- solve(data, ...)     # otherwise, use Solve to compute the Inverse matrix
  x$setinverse(inverse)           # .. and store it in the "cache"/parent env. variable
  inverse                               # and also return the inverse
}
