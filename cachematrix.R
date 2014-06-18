## makeCacheMatrix creates a list that contains 4 functions
## for setting and retrieving values of a matrix and its inverse
## cacheSolve checks the cache for the inverse of the matrix
## and either returns it from the cache or solves for it if 
## nto present.

## Function for setting and retrieiving matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## function for retrieving cached inverse or solving for inverse

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m}
