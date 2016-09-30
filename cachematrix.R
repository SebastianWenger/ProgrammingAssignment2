## MakeCacheMatrix and cacheSolve work together to speed up matrix computation,
## with makeCacheMatrix storing a inputted matrix and caching its inverse. and
## with cacheSolve deciding either to compute the inverse of the inputted
## matrix, or retrieving a previously solved inverse from the cache.

## makeCacheMatrix creates a matrix object, that stores its inverse in a cache.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}



## Solves for the inverse of the matrix created in "makeCacheMatrix" function, and if it has been  
## solved for previously, retrieved from the cache.

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) { 
      i <- x$getInverse()
      if (!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      m <- x$get()
      i <- solve(m, ...)
      x$setInverse(i)
      i
        
}
