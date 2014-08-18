## This program contains two functions.
##
## The first one creates a special "matrix" object 
## that can cache its inverse.
##
## The second one find the inverse of the special "matrix" 
## returned by the makeCacheMatrix function above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve 
## the inverse from the cache and return that.

## Create a matrix to cache the inverse results.
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Lookup the cached martix to find matches.
## If not find, run solve and cache the result.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
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