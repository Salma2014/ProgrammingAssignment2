#Below are 2 functions that are used to create a special object to store a matrix and 
# cache's its inverse

#The first function, makeCacheMatrix creates a matrix, which is a list containing a function to:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse matrix
# - get the value of the inverse matrix

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

#This 2nd function (cacheSolve) calculates the inverse of the special "matrix" created with the above function. It first
# checks to see if its inverse has already been calculated. If so, it gets the inverse from the cache and
# skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse 
# in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

