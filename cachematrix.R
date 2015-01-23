# This collection of functions caches the inverse of an invertible matrix

# The MakeCacheMatrix function creates a special "matrix" object that can cache 
# its inverse.
MakeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the matrix
  setinverse <- function(inverse) matrix <<- inverse
  # get the value of the inverse of the matrix
  getinverse <- function() matrix
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}

# The CacheSolve function computes the inverse of the special "matrix" returned
# by MakeCacheMatrix. If the inverse has already been calculated (and the matrix
# has not changed), then CacheSolve retrieves the inverse from the cache.
CacheSolve <- function(x, ...) {
  matrix <- x$getinverse()
  # check if the inverse has already been calculated
  if(!is.null(matrix)){#} && old.matrix == x){
    # if yes, return the cached matrix instead of redoing the calculation
    message("getting cached data.")
    return(matrix)
  }
  # if no, calculate the inverse
  data <- x$get()
  matrix <- solve(data)
  x$setinverse(matrix)
  #old.matrix <- matrix#$get()
  return(matrix)
}


# My test unit for Assignment 2

# Load the test matrices. These are invertible.
x = rbind(c(1, 2, 3), c(0, 1, 4), c(5, 6, 0))
y = rbind(c(2, 3), c(2, 2))

# Print the test matrices
cat(x)
print(y)
cat("\n")

# Find and cache the inverse of the first matrix
matrix = MakeCacheMatrix(x)  # find the inverse of the first matrix

print("Find the inverse of the input matrix:")
print(CacheSolve(matrix))  # invert matrix since the inverse doesn't exist
cat("\n")
print("The input matrix hasn't changed so load the cached inverse:")
print(CacheSolve(matrix))  # load the cached inverse
cat("\n")

# Change the input matrix and calculate the inverse
matrix = MakeCacheMatrix(y)  # find the inverse of the second matrix
print("The input has changed so recalculate the inverse")
print(CacheSolve(matrix))  # invert matrix since the matrix has changed
cat("\n")
print("The input matrix hasn't changed so load the cached inverse:")
print(CacheSolve(matrix))

