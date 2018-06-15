# There are two functions. The makeCacheMatrix function creates a special "matrix" object
# that can cache its inverse. The cacheSolve function computes the inverse of the special
# "matrix" returned by makeCacheMatrix. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) INV <<- solve
  getinverse <- function() INV
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  INV <- x$getinverse()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV <- solve(data, ...)
  x$setinverse(INV)
  INV
}
