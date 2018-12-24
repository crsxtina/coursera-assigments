## output the inverse of a matrix and cache the result so it doesn't need to be computed repeatedly, which would be a costly operation

## this function is used to cache the inverse of a matrix object

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
          x <<- y 
          i <<- NULL ## "<<-" assigns a value to an object in an environment that is different from the current environment
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix created by the makeCacheMatrix function above. If the inverse has already been calculated then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	
	  i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)  ## Return a matrix that is the inverse of 'x'
  x$setinverse(i)
  i
      
}
