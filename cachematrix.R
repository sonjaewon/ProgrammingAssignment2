## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly (there are also alternatives
## to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions
## that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    if ( !isTRUE(all.equal(x,y)) ) {
      x <<- y
      invm <<- NULL 
    }
  }
  get <- function() x
  setinvm <- function(invm_) invm <<- invm_
  getinvm <- function() invm
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinvm()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinvm(invm)
  invm
}
