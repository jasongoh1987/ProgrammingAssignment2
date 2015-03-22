## This file contain a pair of functions that cache and compute the inverse of a matrix.

## makeCacheMatrix function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x=matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}

## cacheSolve function computes the inverse of the "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been computed and cached, then this function will retrieve the
## computed inverse from the cache.
cacheSolve <- function(x, ...)
{
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}