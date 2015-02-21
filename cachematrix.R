##  Caching the Inverse of a Matrix

##Hello!!
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    iom <- NULL
     set <- function(y) {
      x <<- y
      iom <<- NULL
     }
    get <- function() x
    setinverse <- function(inverse) iom <<- inverse
    getinverse <- function() iom
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  iom <- x$getinverse()
  if(!is.null(iom)) {
    message("Getting Cached Data :)")
    return(iom)
  }
  data <- x$get()
  iom <- solve(data)
  x$setinverse(iom)
  iom
}
