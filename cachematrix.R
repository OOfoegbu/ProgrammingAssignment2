## This pair of functions creates a special "matrix" object that can cache its inverse
## and return the inverse matrix

## makeCacheMatrix function creates a list that sets the special matrix, gets the matrix
## sets the inverse of the matrix, and gets the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) v <<- inverse
  getinverse <- function() v
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function will calculate the inverse of the special matrix
## from the makeCacheMatrix function. This function will first check if the inverse
## has been calculated. If yes, it will return the cached inverse matrix along with a 
## message. If no, it will calculate the inverse using the solve function, and
## return the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v
}
