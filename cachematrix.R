## The makeCacheMatrix function receives an argument of type matrix and creates an object of type makeCacheMatrix.
## The cacheSolve receives an object of type makeCacheMatrix and returns/caches its inverse.


## This function takes a matrix as an argument. It creates an object with the type makeCacheMatrix.
## The user is able to change the value of the matrix without having to re-initialize it. 
## It takes x as an argument and creates an object named s with a null value.
## The x and s values can be called using their get and set functions.
makeCacheMatrix <- function(x = matrix()) {
  s <<- NULL
  set <- function(y) {
  x <<- y
  s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function receives an object of type makeCacheMatrix. 
## The function creates the inverse of the matrix that the user has used as an input.
## If the value has already been cached, it returns the cached value.
## If the value has not yet been cached, it returns the value and caches the value for future use.
cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setinv(s)
  s
}