## Assignment: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL                              
  set <- function(y) {                    
    x <<- y                             
    matrix_inverse <<- NULL                        
  }
  get <- function() x                     
  
  setinverse <- function(inverse) matrix_inverse <<- inverse  
  getinverse <- function() matrix_inverse                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
