# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This variable will hold the cached inverse of the matrix
  
  # This function sets the value of the matrix and invalidates the cached inverse
  set <- function(y) {
    x <<- y  # Assign the input matrix to the variable x in the parent environment
    inv <<- NULL  # Reset the cached inverse, as the matrix has changed
  }
  
  # This function returns the value of the matrix
  get <- function() {
    x  # Return the matrix
  }
  
  # This function sets the value of the cached inverse
  setInverse <- function(inverse) {
    inv <<- inverse  # Assign the input inverse to the variable inv in the parent environment
  }
  
  # This function returns the value of the cached inverse
  getInverse <- function() {
    inv  # Return the cached inverse
  }
  
  # Return a list of all the functions defined above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Attempt to get the cached inverse
  
  # If the cached inverse is not NULL, return it
  if (!is.null(inv)) {
    message("getting cached data")  # Print a message indicating that cached data is being used
    return(inv)  # Return the cached inverse
  }
  
  # Get the matrix from the special "matrix" object
  mat <- x$get()
  
  # Compute the inverse of the matrix
  inv <- solve(mat, ...)
  
  # Cache the computed inverse for future use
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}
