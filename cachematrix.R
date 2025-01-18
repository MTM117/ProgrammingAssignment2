## This script contains two functions:
## 1. makeCacheMatrix: Creates a special "matrix" object that can cache its 
## inverse.
## 2. cacheSolve: Computes the inverse of the special "matrix" or retrieves 
## it from the cache.

## makeCacheMatrix:
## This function creates a special "matrix" object that can store:
## - The matrix itself.
## - The cached value of its inverse.

## It provides the following functionalities:
## - set(): Assigns a new matrix to the object and clears the cached inverse.
## - get(): Retrieves the stored matrix.
## - setinverse(): Stores the inverse of the matrix in the cache.
## - getinverse(): Retrieves the cached inverse (if available).

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve:
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix.
## If the inverse is already cached, it retrieves the cached value to 
## save computation time.
## If the inverse is not cached, it computes the inverse, stores it in the 
## cache, and then returns it.

## Parameters:
## - x: A "matrix" object created by makeCacheMatrix.
## - ...: Additional arguments to pass to the `solve` function.
##
## Returns:
## - The inverse of the matrix stored in the special "matrix" object.

cacheSolve <- function(x, ...) {
        i <- x$getinverse() ## Return a matrix that is the inverse of 'x'
        if(!is.null(i)) {
          message("getting cahed data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Example usage:
# m <- matrix(c(1, 2, 3, 4), 2, 2)   # Define a 2x2 matrix.
# specialMatrix <- makeCacheMatrix(m) # Create the special "matrix" object.
# cacheSolve(specialMatrix)          # Compute and cache the inverse.
# cacheSolve(specialMatrix)          # Retrieve the cached inverse.