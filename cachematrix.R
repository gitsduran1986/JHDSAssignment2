## Put comments here that give an overall description of what your
## functions do

## Here I have created two functions, one that creates a so-called cacheMatrix object from a Matrix and another that 
## calculates the inverse of that Matrix.  The advantage of using this approach over just using the solve function is that
## once an inverse has been calculated, theinverse of the Matrix can be stored and retrieved without further calculation

## makeCacheMatrix returns a CacheMatrix object and take a regular Matrix as input.  If no argument is provided, assume a 
## 1x1 matrix with value of 0

makeCacheMatrix <- function(x = matrix(0)) {
  if (dim(x)[1] != dim(x)[2] || det(x) == 0) {
    print("Warning: matrix is not invertible, using cacheSolve will result in error")
  }
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## CacheSolve takes the object from the previous function as input, returns its inverse (if invertible).  
## Will first seek out cached inverse before calling the solve function to find inverse. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if (dim(x$get())[1] != dim(x$get())[2] || det(x$get()) == 0) {
    print("Error: matrix is not invertible, solve function will return error")
  }
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#test
m1 <- matrix(1:4,nrow=2)
cm1 <- makeCacheMatrix(m1)
inverse <- cacheSolve(cm1)
inverse <- cacheSolve(cm1)



