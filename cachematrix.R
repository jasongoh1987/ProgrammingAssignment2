## This file contain a pair of functions that cache and compute the inverse of a matrix.

## makeCacheMatrix function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x=matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}

## cacheSolve function computes the inverse of the "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been computed and cached, then this function will retrieve the
## computed inverse from the cache.
cacheSolve <- function(x, ...)
{
  inv <- x$getinverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  return(inv)
}