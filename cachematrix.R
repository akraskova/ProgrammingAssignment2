#solvation for CacheMatrix based on the studies
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #clearing the inverse old cache and difine the function as a set of
  #values of the matrix
  set <- function(y) {
    x <<- y #set the value
    m <<- NULL #clear cach
  }
  get <- function() x
  #define function to set the inverse.
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
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
