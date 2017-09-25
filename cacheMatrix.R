## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULLB <- matrix(c(1:9),3,3)
  }
  
  ##Test if has inverse
  get <- function() x
  
  a <- x[1]
  b <- x[3]
  c <- x[2]
  d <- x[4]
  
  det <- a * d - b * c
  # Check to see if matrix is singular
  if (det == 0) {
    stop('Determinant of matrix equals 0, no inverse exists')
  }
  else{
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }
}


## cacheSolve: This function computes the invmaterse of the special "matrix" returned by makeCacheMatrix above
##If the invmaterse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the invmaterse from the cache.
cacheSolve <- function(x, ...) {
  ## Computing the invmaterse of a square matrix can be done with the solve function in R. 
  ## For example, if X is a square invmatertible matrix, 
  ## then solve(X) returns its invmaterse.
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("cached data.")
    return(inv)
  }
  else{
    message("inverse calculated")
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }
}
