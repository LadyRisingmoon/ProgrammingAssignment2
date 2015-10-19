# Instructions: Write two functions
# First function: makecacheMatrix
# Second function: cacheSolve

# The first function (makecacheMatrix)
# makeVector creates a special "vector"
# really a list containing a function to
#   set the value of the vector
#   get the value of the vector
#   set the value of the mean
#   get the value of the mean

# The second function (cacheSolve)
# computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed)
# then the cacheSolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  
# Initialize myCache to NULL  
  myCache <- NULL
  
# Create the matrix
  set <- function(y) {
    x <<- y
    myCache <<- NULL
  }
  
# Get the matrix value   
  get <- function() x

# Invert the matrix and store that as myCache
  setSolve <- function(solve) myCache <<- solve
  getSolve <- function() myCache
  list(set = set, 
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}

#  This function computes the inverse of the special "matrix" returned 
#  by makeCacheMatrix above. If the inverse has already been calculated
#  (and the matrix has not changed), then the cacheSolve should retrieve 
#  the inverse from the cache.

# Return a matrix that is the inverse of 'x'
# NB: '...' Further arguments passed to or from other methods
cacheSolve <- function(x, ...) {
  myCache <- x$getSolve()
  if(!is.null(myCache)) {
    return(myCache)
  }
  
  data <- x$get()
  myCache <- solve(data, ...)
  x$setSolve(myCache)
  myCache
}

# Test the solution!!!
x <- c(1, 3, 3)
y <- c(1, 4, 3)
z <- c(1, 3, 4)
matrixData <- rbind(x, y, z)

print(matrixData)

result <- makeCacheMatrix(matrixData)

# Test that matrix was inverted and overwritten in cache!!!
print(cacheSolve(result))


