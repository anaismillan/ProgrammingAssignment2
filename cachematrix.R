## The function makeCacheMatrix creates a special "matrix" that can cache its inverse. 
##The matrix that we submit must be an invertible square matrix.
## If first sets the value of the matrix, then gets the value of the matrix. 
##After that, it sets the value of the inverse of the matrix and finally gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  ma <- NULL
  set <- function(y) {
    x <<- y
    ma <<- NULL
  }
  get<- function()x
  setinverse <- function(inverse) ma <<- inverse
  getinverse <- function() ma
  list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)
}

## The function cacheSolve returns the inverse of the special "matrix" returned by makeCacheMatrix. 
##It first checks if the inverse has been computed. If it was computed, it skips the computation and gets the result. 
##If it was not computed, it computes the inverse, sets the value in the cache with setinverse function. 

cacheSolve <- function(x, ...) {
  ma <- x$getinverse()
  if(!is.null(ma)){
    message("getting cached data")
    return(ma)
  }
  matrix <- x$get()
  ma<- solve(matrix)
  x$setinverse(ma)
  ma
}
