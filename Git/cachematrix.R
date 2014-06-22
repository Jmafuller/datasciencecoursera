## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## cacheSolve: Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolv <- function(solve) m <<- solve
  getsolv <- function() m
  list(set = set, get = get,
       setsolv = setsolv,
       getsolv = getsolv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
