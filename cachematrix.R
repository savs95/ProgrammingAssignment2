## Same as the mean, it creates special matrix with can set, get , set inverse and set inverse values of a matrix

## Makes a matirix which allows to reterieve values of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) 
    {
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




## Checks if it already exists (the inverse), if yes, then return same value, else it computes

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
}
