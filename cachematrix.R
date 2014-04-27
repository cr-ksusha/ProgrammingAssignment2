## makeCacheMatrix function creates an object of a matrix that can cache it's inverse
## cacheSolve calculates or returns cached inverse of a matrix object created by first function


## This function creates an object of a matrix which is a list containing functions get, set, getinv and setinv
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inversion) inv <<- inversion
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


### This function returns cached inversion of the matrix passed as an argument if the inverse has already been calculated.
## Otherwise it calculates the inverse and sets it in cache
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if (!is.null(inv)) {
    message("getting cached data")
    # return cached inversion
    return(inv)
  }

  data <- x$get()
  # calculate inversion
  inv <- solve(data)
  # save the value of inversion in cache
  x$setinv(inv)
  inv
}
