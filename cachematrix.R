## Two functions to allow the caching of the expensive solve fn
#
# For some matrix m, create the cached solver:
# cm <- makeCacheMatrix(m)
#
# For a cached solver, the inverse is generated/returnedwith:
# cacheSolve(cm)

# Create the cached matrix object from a matrix object.
# This object contains a list of four functions:
# set : initialise objects enviroment's variables
# get : return original matrix
# setsolve : set the inverse matrix
# getsolve : return the inverse matrix, 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # initialise the matrices x & m in the function's environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # return the original matrix
  get <- function() x
  
  # calculate the matrix inverse, save in m 
  setsolve <- function(solve) m <<- solve
  
  # return m; the inverse or NULL if cacheSolve has not been called
  getsolve <- function() m
  
  # Return the list of functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# Only call solve(x) if cached inverse is not available
cacheSolve <- function(x, ...) {
  # Get the previous inverse or NULL if calc has not been done     
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # First time through; get the original matrix and calc inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  ## Return a matrix that is the inverse of 'x'
  m
}
