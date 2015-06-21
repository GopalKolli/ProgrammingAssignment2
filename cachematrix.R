
##makeCacheMatrix function createS a special "matrix" object that can cache its inverse and
##cacheSolve function returns the inverse of Matrix

##makeCacheMatrix function createS a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {                            ##set function SETS THE MATRIX
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x                             ##get function RETURNS THE CURRENT MATRIX
  
  setinverse <- function(inverse) inv <<- inverse  ##setinverse function SETS THE INVERSE OF THE MATRIX
  
  getinverse <- function() inv  ##getinverse function RETURNS CURRENT INVERSE OF THE MATRIX
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)       ##THIS list IS RETURNED BY makeCacheMatrix function
  

}


## cacheSolve function returns the inverse of the Matrix

cacheSolve <- function(x, ...) {
  
  
  inv <- x$getinverse()
  if(!is.null(inv)) {               ##If inverse of Matrix 'x' is available in the cache, 
    message("getting cached data")  ##It is returned from here
    return(inv)
  }
  data <- x$get()                   ##If inverse of Matrix 'x' is not available in the cache,
  inv <- solve(data, ...)           ## It's calculated and the obtained vaue is returned
  x$setinverse(inv)
  inv
  
        ## Returns a matrix that is the inverse of 'x'
}
