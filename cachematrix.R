## Put comments here that give an overall description of what your
## functions do

## Funciton to create our inverse caching matrix

makeCacheMatrix <- function(om = matrix()) {
  inv <- NULL
  setmatrix <- function(y) {
    om <<- y
    inv <<- NULL
  }
  getmatrix <- function() om
  setinv <- function(inv_in) inv <<- inv_in
  getinv <- function() inv
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinv = setinv,
       getinv = getinv)
}


## Function to return the inverse of our matrix, using the cached inverse if it's avaible and solving for the inverse if it's not

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$getmatrix(), ...)
  x$setinv(inv)
  return(inv)
}
