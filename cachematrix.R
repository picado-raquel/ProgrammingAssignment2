## Put comments here that give an overall description of what your
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
Y## our assignment is to write a pair of functions that cache the inverse of a matrix.

## 1- set the value of the vector
## 2- get the value of the vector
## 3- set the value of the mean
## 4- get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the mean of the special 
## "vector" created with the above function. However, it first 
## checks to see if the mean has already been calculated. If so, 
## it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the 
## value of the mean in the cache via the setmean function.


cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

