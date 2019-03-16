## Functions to create a mechanism to cache the inverse of a matrix.

## The function creates a special matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special matrix computed by the function above

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  if(nrow(data) != ncol(data)){return(message("Matrix must be square"))}
  if(det(data) != 0  ){
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv  
  }else{
    message("Cannot compute inverse")
  }
}
