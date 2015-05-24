## These functions create a matrix object which has a cache for its inverse
## because matrix inverse is a costly calculation
# makeCacheMatrix() constructs the object,
# cacheSolve() solves it if necessary, otherwise returning the cached inverse


# make a special object which is a matrix which caches its own inverse...
# ...so that it doesn't take ages to calculate again!
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # the inverse
  get <- function() x # no parameters, just return x
  set <- function(y) {
    x <<- y  # set the value of x in the upper context
    i <<- NULL # to say explicitly, no inverse yet
  }
  getsolve <- function() i # return our cached inverse
  setsolve <- function(inv) i <<- inv
  list(set = set, get = get, getsolve = getsolve, setsolve = setsolve)
}


## Return a matrix that is the inverse of 'x'
# if we have already cached x^-1, return that, otherwise calculate the inverse
# ... and return it
cacheSolve <- function(x, ...) {

  i <- x$getsolve()
  if(!is.null(i)) { # if i is not null, then it contains the cached inverse
    message("getting cached data")
    return(i)       # so we return that
  }
  data <- x$get()   # if we are here, i is null, so we get() the matrix
  i <- solve(data, ...) # ... solve it
  x$setsolve(i)         # ... and use setsolve() to cache the inverse for next time
  i                     # (not forgetting to return the inverse this time!)
}
