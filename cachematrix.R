## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly (there are also alternatives to matrix inversion that we 
## will not discuss here). Your assignment is to write a pair of functions 
## that cache the inverse of a matrix.

## These two functions are as follows:

## Description of function:
  ## makeCacheMatrix(): creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Input x: as a square invertible matrix, assumed to be invertable 
  ## Steps in Function: 
  ##   1. set matrix
  ##   2. get matrix
  ##   3. set inverse matrix
  ##   4. get inverse matrix (input for next function in project) 
  
  inv = NULL
  set = function(m) {
    # Use `<<-` to assign a value to an object in the local environment 
    # that is different from the current environment. 
    x <<- m
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Description of function:
## cacheSolve(): computes the inverse of the "matrix" returned by makeCacheMatrix(). 
##               If the inverse has already been calculated and the matrix has not 
##               changed, it'll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  ## This Function Returns a matrix that is the inverse of 'x' either from cache
  ## (where previously inverted) or as a fresh inversion
  ## Input x: output of makeCacheMatrix()
  ## Returns: the inverse of the original matrix input to makeCacheMatrix()
  
  invrtd = x$getinv()
  ## If the inverse has already been calculated
  if (!is.null(invrtd)){
    ## Inverted matrix is found in cache, returns cached matrix vs. calculates 
    message("getting cached data")
    return(invrtd)
  }
  ## Alternatively, inverted matrix is not in cache, so invert inputted matrix from makeCacheMatrix() 
  input_matrx = x$get()
  invrtd = solve(input_matrx, ...)
  # Sets the value of the inverse in the cache via the setinv function.
  x$setinv(invrtd)
  return(invrtd)
}
