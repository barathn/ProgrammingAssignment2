## makeCacheMatrix is used to create the MatrixObject
## cacheSolve is used to return the inverse of Matrix Created by makeCacheMatrix.

# makeCacheMatrix does the following operations
# --> Get the Matrix
# --> Set the Matrix
# --> Get the Inverse Matrix
# --> Set the Inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
  # To store the cached inverse matrix
  inv_x <- NULL
  
  # Get Matrix
  get <- function() x
  
  # Set Matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  #Get Inverse
  getinverse <- function() inv_x
  
  #Set Inverse
  setinverse<- function(inverse) inv_x <<-inverse
  
  #Return Value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve is used to return the inverse of Matrix Created by makeCacheMatrix.
# The function does the following
# check for existing inverse matrix
# Do the inverse of the matrix
# Return the inverse

cacheSolve <- function(x, ...) {
  # Inverse Matrix of x
  inv_x <- x$getinverse()
  
  # Check for Existing Inverse Matrix
  if (!is.null(inv_x)) {
    message("Getting Cached Inverse Matrix")        
  } else {
    # Else Compute Inverse Matrix
    dt <- x$get()
    inv_x <- solve(dt, ...)
    
    # Set the Inverse, Cache the Inverse
    x$setinverse(inv_x)        
  }
  
  #Return the Value
  return(inv_x)
}
