## Tedi Kondi 20171103
## creates special matrix object that saves in cache its inverse. 
## Computes the inverse of the special "matrix" object. 
##If the inverse has already been calculated , then the cachesolve should retrieve the inverse from the cache.

## creates matrix object that saves in cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s <<- inverse
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  computes the inverse of a "matrix". If the inverse has already been calculated , then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}