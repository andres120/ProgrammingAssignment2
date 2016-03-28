## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
# The first function, `makeCacheMatrix` creates a special "matrix", which is
# just a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of that matrix's inverse
# 4.  get the value of that matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve
# The following function calculates the solve of the special "matrix"
# created with the above function. However, it first checks to see if the
# mean has already been calculated. If so, it 'get's the mean from the
# cache and skips the computation. Otherwise, it calculates the mean of
# the data and sets the value of the mean in the cache via the 'setsolve'
# function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
