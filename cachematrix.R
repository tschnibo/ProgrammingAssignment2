## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## The following functions deliver the functionality of calculating the inverse of a matrix. If the matrix has been calculated before, the cached data is returned, in order to shorten calculation time.

## "makeCacheMatrix" delivers de variables and the function to store the inverse of the matrix. The special "matrix" object is generated.

makeCacheMatrix <- function(x = matrix()) {
  
  
  xinverse <- NULL   #variable for storage of the inverse
  set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
  get <- function() x   #function that returns x (the matrix)
  setinverse <- function(inv) xinverse <<- inv   #sets / stores the inverse
  getinverse <- function() xinverse      #reads out the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## With this function one can calculate the matrix inverse if this hasn't been done yet, otherwise the cached inverted matrix is returned and "getting cached data" is printed.

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
  
  ## Return a matrix that is the inverse of 'x'
}

