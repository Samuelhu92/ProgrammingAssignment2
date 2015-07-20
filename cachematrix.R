#This function has two variables:x and i
#x is a matrix
#i is inversematrix of x after computing
makeCacheMatrix <- function(x = matrix()) {
  #variable defined in function 
  i <- NULL
  #assign y to x in global environment rather than create a new x in function "set"
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #constant function,return x, the datasets to be computed
  get <- function() x
  #assign inversematrix to i, that is the cache operation results
  setinverse <- function(inverse) i <<- inverse
  #constant function, return i
  getinverse <- function() i
  #function makeCacheMatrix returns a list, within 4 factors
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#input a return from makeCacheMatrix functin
cacheSolve <- function(x, ...) {
  #call function getinverse in makeCacheMatrix
  i <- x$getinverse()
  if(!is.null(i)) {
    #if TRUE, inversematrix already exists
    #return i
    message("getting cached data")
    return(i)
  }
  #if FALSE, call function in x 
  data <- x$get()
  i <- solve(data, ...)
  #call function setinverse in x, store result
  x$setinverse(i)
  i
}