## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this constructor function builds a function by passing a
#matrix as its argument. the new function yields a list with
#functions that allow you to get, set the matrix and get and
#set an inverted matrix. A second function (cacheSolve) is used
#to actually invert the matrix. The inverted matrix is held
#in the object returned by makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    s <<- NULL
    x <<- y
  }
  get <- function(){x}
  setsolve <- function(solve){s <<- solve}
  getsolve <- function(){s}
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
#this function performs the action of inverting the matrix
#held in the function created by makeCacheMatrix. It determines
#if an in inverted matrix is already stored in the chache or if
#the values of the matrix have changed since chaching the inverse.
#if the cache exists and no changes were made, this function returns
#the cached result. Otherwise it computes the inverse with solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
