## Functions to return the inverse of a passed matrix

## Creates the matrix with its own set of functions
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL   ## Set up variable for inverse but set it to Null
    set <- function(y) {
    x <<- y
    i <<- NULL
    
  }
  get <- function()   # Function to retrieve the matrix
  setinverse <- function(inv) i <<- inv   # ... to set the inverse
  getinverse <- function() i   # ... to return the inverse
  list(set = set, get = get,   # list of the functions
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Accepts a function created by makeCacheMatrix and either
## a) returns its inverse if this hasn't already be done or
## b) retrieves it from memory if it has.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()  # See if we've already retrieved the inverse
  if(!is.null(i)) {   # If we have, just return that.
    message("Retrieving cached data...")
    return(i)
  }
  d <- x$get()  ## Otherwise, get the matrix and find its inverse
  i <- solve(d)
  x$setinverse(i) ## Now that we have it, store it for later retrieval
  i
}
