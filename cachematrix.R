## Main idea of the program
#
# Using the concept of lexical scoping, we write a pair of functions
# that store the matrix value and also its inverse. When the inverse of 
# the same matrix is needed repeatedly , the cached copy of the inverse 
# of the matrix is retrieved in order to save computation time.
#

## What does the makeCacheMatrix function do???
# makeCacheMatrix is a function which stores a martix and a cached value
# of the inverse of the matrix.
#
# It returns a list of functions as follows:
# $ set - sets the value of a matrix
# $ get - gets the value of a matrix
# $ setinverse - sets the inverse of the matrix
# $ getInverse - gets the set value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## store matrix values x : matrix  entered, inv: its inverse
  
  inv <- NULL
  set <- function(y = matrix()) { ## sets a new matrix value to x
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## gets the stored matrix
  getinverse <- function() inv ## gets the stored inverse matrix
  setinverse <- function(t = matrix()){## seta a new matrix value to inverse
    inv <<- t
    
  }
  # returns a list with each element of the list as a function
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


# The following function calculates the inverse of the "x" matrix created 
# with makeCacheMatrix. Cached values if present will be retrieved rather
# than computing the inverse again and again for same matrix.



cacheSolve <- function(x, ...) {
        ## get cached inverse if present.
  inv <- x$getinverse()
  if(!is.null(inv)){ 
    message("getting the cached inverse matrix")
    return(inv)
  }
  
  ## if inverse not present, then calculate new inverse and store it.
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
