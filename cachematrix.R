## This file contains functions to calculate inverse of a matrix and also cache it.
## If the inverse of the matrix does not exist, it is calculated and cached.
## If it is cached, then the inverse is fetched from the cache, and not recalculated.

## THis function returns a list of functions. The list includes : 
## 1) Function to get the value of the matrix
## 2) Set the value of the matrix
## 3) Get the value of the inverse of the matrix
## 4) Set the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(new_value) {
    x <<- new_value
    inv_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_x <<- inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns the inverse of the given matrix.
## If the matrix is not cached, it calculates it and caches it.
## If the inverse is already cached, it would directly return that.

cacheSolve <- function(x, ...) {
    inv_x <- x$getinverse()
    if(!is.null(inv_x)) {
      message("getting cached inverse matrix")
      return(inv_x)
    }
    matrix <- x$get()
    inv_x <- solve(matrix, ...)
    x$setinverse(inv_x)
    inv_x
}
