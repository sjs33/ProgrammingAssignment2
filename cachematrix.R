## makeCacheMatrix() must be called first and passed an intertible matrix.
##    This function then creates a list of functions used to initialize the operation
## cacheSolve() is the command used to actually call for calculating the inverted matrix.
##    The argument for the cacheSolve() function must be the list created by the makeCacheMatrix function.

## By storing the variables outside the local function, these functions avoid recalculating the inversion
## function if the original matrix has not changed.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
