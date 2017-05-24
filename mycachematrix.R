## Matrix inversion is usually a costly computation and there
##may be some benefit to caching the inverse of a matrix rather
##than compute it repeatedly (there are also alternatives to matrix
##inversion that we will not discuss here). Your assignment is to
##write a pair of functions that cache the inverse of a matrix.

## Create special matrix and related functions

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	  set <- function(y) {
	    X <<- y
	    m <<- NULL
	  }
	  get <- function() X
	  setsolve <- function(solve) m <<- solve
	  getsolve <- function() m
	  list(set = set, get = get,
	       setsolve = setsolve, getsolve = getsolve)
}


## Calculate inverse marix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- X$getsolve()
        ## Return inverse of 'x' from cache 
	  if(!is.null(m)) {
	    message("getting cached data")
	    return(m)
	  }
	  data <- X$get()
	  m <- solve(data, ...)
	  X$setsolve(m)
  	  m
}
