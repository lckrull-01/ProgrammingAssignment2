## These two dunctions create the inverse of a provided matrix after checking
## if the inverse is stored in the cache memory

## This function creates a list containing a function to
## set the value of the matrix, get the vlaue of the matrix
## set the value of the inverse of the matrix, get the value of the inverse of the matrix

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


## This function checks if the value of the inverse of the matrix hasbeen calculated
## and if not it uses the solve function to get the inverse of the matrix and stores it
## The function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  print(data)
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
