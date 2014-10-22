## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # cache the inverse matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i # setter of cache 
  getinv <- function() inv # getter of cache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # check if we have a cache matrix already
  if(!is.null(inv)) { 
    # use the cache matrix
    message("getting cached data")
    return(inv)
  }
  
  # calculate the inverse matrix, and then cache it
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
