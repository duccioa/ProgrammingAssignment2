## Given a matrix x, the function cacheSolve returns s as the inverse of x. 
## If s is NULL, meaning that it hasn't been calculated yet, the function calculates the inverse and returns it
## If the mean has been already calculated it fetches the values from the cache
## The function makeCacheMatrix is required to run cacheSolve, 
## as it creates the functions and the variables used by the latter



## makeCacheMatrix is a function that takes as an input a matrix x 
## it returns a list of four functions
## and a variable s set to NULL

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x  ##the function get returns the value of the original matrix
  setsolve <- function(solve) s <<- solve ##setsolve accesses the inverse and stores it in s using superassignement 
  getsolve <- function() s  ##getsolves returns the cached value of the inverse of the original matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  ##a list is created which stores the previous functions
}


## This function take the previous variable s and accesses it
## if the variable is not NULL, it returns the value of the variable, which is the inverse of the original matrix
## if the variable is NULL, it calculates the inverse of the original matrix and stores it in s

cacheSolve <- function(x, ...) {
  
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...) ## Calculate a matrix that is the inverse of 'x'
  x$setsolve(s)   ## Return a matrix that is the inverse of 'x'
  s
}
