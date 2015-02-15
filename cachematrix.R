## This code cacehes a matrix and its inverse

## makeCacheMatrix caches a matrix and has methods that set a matrix inverse and get a stored matrix inverse

makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve computes the inverse of a matrix and sets the value into the cache.  When the inverse
## for the same matrix is called again, cacheSolve checks the cache instead of recomputing the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
