
# Create a matrix that can cache its inverse
makeCachematrix <- function( mt = matrix()) {
  
  # Set inverse property
  i <- NULL
  
  # Set the matrix function
  set <- function(matrix) {
    mt <<- matrix
    i <<- NULL
  }
  
  # Get the matrix function
  get <- function() {
    mt
  }
  
  # Set the inverse of the matrix function
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  # Get the inverse of the matrix function
  getInverse <- function() {
    i
  }
  
  # Return a list of our functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Compute the inverse of the special matrix returned by "makeCachematrix"
# above. If the inverse has already been calculated (and the matrix has not
# changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  mt <- x$getInverse()
  
  # Just return the inverse if its already set
  if( !is.null(mt) ) {
    return(mt)
  }
  
  # Get the matrix from our obj
  data <- x$get()
  
  # Calculate the inverse using matrix mtultiplication
  mt <- solve(data, ...)
  
  # Set the inverse to the obj
  x$setInverse(mt)
  
  # Return the matrix
  mt
}

# Used to test cacheSolve
a <- makeCachematrix()     #create functions
a$set(matrix(0:3, 2, 2))   #create matrix in working environment
cacheSolve(a)              #1st run returns inverted matrix
