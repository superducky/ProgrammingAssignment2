## makeCacheMatrix and cacheSolve are a way of caching the inverse of a matrix
## the first time it is caculated, and returning the cached inverse the second
## time onwards if the matrix has not changed.

## Usage: Call makeCacheMatrix function with a matrix, 
## then call cacheSolve with the result. The first time cacheSolve is called,
## the matrix is caculated and cached. The second time onwards, the cached 
## inverse is returned (without recalculating) if the matrix has not changed.


## makeCacheMatrix returns a vector of 4 functions that get and set the matrix
## and its inverse. Its environment consists of the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes the result of makeCacheMatrix as the argument, 
## and returns the inverse of its matrix
## Called for the first time, the inverse has not been calculated yet (NULL),
## so cacheSolve calculates and caches it. Next time onwards, 
## the cached inverse is returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
