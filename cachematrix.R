## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  cache <- x$getinverse()
  if (!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  matrix <- x$get()
  cache <- solve(matrix, ...)
  x$setmatrix(cache)
  cache
}
