## The following functions create a matrix object that is capable of storing 
## it's inverse, and subsequently handles lifecycle events when the original 
## matrix is updated.

## makeCacheMatrix creates an object that creates a field to cache the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  # Field to store matrix inverse
  minv <- NULL
  
  # Setter function to set this object's new matrix and clear out the inverse
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  # Returns the original matrix
  get <- function() x
  
  # Setter that allows us to store the original matrix's inverse
  setinverse <- function(inverse) minv <<- inverse
  
  # Retrieves the inverse if it has been stored
  getinverse <- function() minv
  
  # Returns and exposes these functions so that they are accessible on the object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the cached matrix object and stores it into 
## the cached matrix object or it will retrieve the existing inverse and return 
## without re-computing

cacheSolve <- function(x, ...) {
  # Try to retrieve the inverse
  minv <- x$getinverse()
  
  # If it exists return, and we are done!
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  
  # Otherwise, get the original matrix
  data <- x$get()
  
  # Solve for the inverse
  minv <- solve(data, ...)
  
  # Set the inverse on the cache matrix object
  x$setinverse(minv)
  
  # Return the inverse
  minv
}
