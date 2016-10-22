## Solving and caching the inverse of a matrix


## This function handles getting and setting the matrix and its inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolveInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setsolveInverse = setsolveInverse,
       getInverse = getInverse)
  
}


## This function checks the matrix's inverse is already cached, if not then solves and adds to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolveInverse(m)
  m
  
}
