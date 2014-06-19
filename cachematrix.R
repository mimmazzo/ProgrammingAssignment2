## These functions creates a special "matrix" object an use it to cache the inverse
## of a matrix.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <<- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix function. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setSolve(s)
  s
}
