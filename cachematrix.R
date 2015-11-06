## A pair of functions that cache the inverse of a matrix.
##ASSUMPTION
##assume that the matrix supplied is always invertible.
##
##USAGE
## a <- makeCacheMatrix(matrix(c(7,4,9,3), nrow = 2, ncol = 2))b
## a$get()
## a$getsolve()
## cachesolve(a)
## a$getsolve()

##makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
##
##set: sets the value of the matrix
##get: gets the value of the matrix
##setsolve: sets the value of the inverse using solve function
##getsolve: get the value of the inverse using solve function
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) m <<- inverse
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), then the cache is returned
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
