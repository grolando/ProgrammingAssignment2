## The goal of the functions below is to cache the inverse of a matrix.
## This allows saving time in case of long or repeated operations.
## The functions rely on the lexical scoping used by the R language.

## The makeCacheMatrix function creates a list of functions to 
## set/get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## The cacheSolve function returns the inverse of a matrix. 
## If the inverse has already been calculated, the function returns the cached value.
## If the inverse has not been calculated yet, the function calculates and returns the inverse.
## The function also caches its value for future uses.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}



