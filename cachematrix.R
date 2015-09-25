## makeCacheMatrix takes a square-invertable matrix and creates a special
## "cacheable matrix", which is really a list containing functions to:
##    1. set the value of the inner matrix
##    2. get the value of the inner matrix
##    3. set the value of the inverse of an inner matrix
##    3. get the value of the inverse of an inner matrix
## With makeCacheMatrix (a kind of container of previously computed
## data), it is possible (together with function 'cacheSolve') to
## avoid recomputing cached results.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve takes a special "cacheable matrix" (obtained by using
## function 'makeCacheMatrix') and computes its invert. In case of taking
## a previously computed matrix, it returns the cached result instead of
## recomputing it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setSolve(m)
  m
}
