## These are two functions that cache the inverse of the matrix 

## This first function create a matrix object that caches its own inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,get = get, setInverse = setInverse,getInverse = getInverse)
}


## This second function analyses the matrix from makeCacheMatrix.If the inverse
## been calculated and the matrix hasn't change, the cachesolve should
##retrieve the inverse matrix from cache.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
        ## Return a matrix that is the inverse of 'x'
  x$setInverse(i)
  i
}

