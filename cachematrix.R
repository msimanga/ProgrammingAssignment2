
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL 
  
  set <- function(y) { 
    x <<- y 
    s <<- NULL 
  }
  
  #Return input matrix x
  get <- function() x 
  
  
  #Get inverse of x and assign to variable s
  setsolve <- function(solve) s <<- solve
  
  
  #Return s
  getsolve <- function() s 
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #Get solved matrix otherwise NULL if it does not exist
  s <-x$getsolve()
  #Get original matrix
  o <-x$get()
  
  if(!is.null(s)) {
    message("Getting cached data")
    return(s)
  }
    
  ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
  
}
