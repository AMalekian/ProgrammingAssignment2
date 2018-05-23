## Put comments here that give an overall description of what your
## functions do

## his function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setinv <- function(matinv) inv <<- matinv
      getinv <- function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
        message("getting cached data")
        return(inv)
      }
      newm <- x$get()
      inv <- solve(newm, ...)
      x$setinv(inv)
      inv
}
