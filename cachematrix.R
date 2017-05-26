## Put comments here that give an overall description of what your
## functions do
## Below are a pair of functions that can be used to create a special object that will store a matrix and caches its inverse.


## Write a short comment describing this function
## By using this function a special "matrix" object is created that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## The function below computes the inverse of the special "matrix" returned by the "makeCacheMatrix" found above. 
## If the inverse has previously been calculated the the "cachesolve" function should retrive the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
