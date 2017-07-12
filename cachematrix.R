## Caching the Inverse of a Matrix

## makeCacheMatrix creates a matrix that is essential a list containing a function
## that does the following:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse of the matrix
## 4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve() computes the inverse of the matrix created by makeCacheMatrix().
## If cacheSolve() is run a second time without any changes to the matrix, it
## will return the same output as the initial run of cacheSolve because it is
## returning the cached data. Otherwise it will create a new matrix from the data,
## calculate the inverse of the data and set that output as the cache value 



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  iv
}