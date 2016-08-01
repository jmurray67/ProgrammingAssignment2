##The following functions work to search for a cached value (the inverse of a matrix in this case).
##If the inverse for the matrix has not been calculated yet, this set of functions work to calculate the inverse and cache it in an environment outside of the current environment.

##This function essentially 'prepares' the input matrix for the second function which actually performs the search and get/calculation and caching of the inverse.
##The preparation involves four things: 1) setting the value of the matrix 2) creating a way to get the set value of the matrix 3) setting the value of the inverse 4) creating a way to get the value of the inverse
##Note that the two 'setting' steps of this function use the '<<-' operator which is what results in the inverse value actually being cached.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##This function pulls the cached inverse value of the desired matrix, then checks whether the inverse value is NULL. 
##If it is not, then the value is reported; otherwise the function proceeds to calculate the inverse of the inputted matrix and then caches the inverse value. 
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
