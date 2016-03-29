## The first function makes a list with methods that get, set & inverse a matrix
## The second function checks If the Matrix inverse is already set, then cached value is returned else it calculates and set its inverse

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Invmatrix<- NULL
  
  set <- function(input = matrix()) {
    x <<- input 
    Invmatrix <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(Value) {
    Invmatrix <<- Value 
    return(Invmatrix)
  }
  
  getInverse  <- function() Invmatrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=makeCacheMatrix(1:6, nrow=3, ncol=3), ...) {
  
  
  findInverse <- x$getInverse() 
  
  if(!is.null(findInverse) && is.matrix(findInverse)) { 
    message("Getting cached data")
    return(findInverse)
  }
  
  Solvematrix <- x$get()  
  
  findInverse <- solve(Solvematrix)
  
  x$setInverse(findInverse)
}

