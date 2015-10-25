## Put comments here that give an overall description of what your
## functions do

## return: a list containing functions to
##              1. set matrix
##              2. get matrix
##              3. set the inverse matrix
##              4. get the inverse matrix
## and this list will be providing input when running cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  invnos = NULL
  set = function(obj1) {
    # assigned a value to an object 
     x <<- obj1
    invnos <<- NULL
  }
  get = function() x
  setinv = function(inverse) invnos <<- inverse 
  getinv = function() invnos
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## return a matrix that is the inverse of 'x'
## return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  
  
  inv1 = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv1)){
      message("getting cached data")
     return(inv1)
  }
  
  # calculates the inverse if it has not been calculated before
  inv.data = x$get()
  inv1 = solve(inv.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv1)
  
  ## returns matrix that is the inverse of 'x'
  return(inv1)  
}
  
  
