## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Description: This function creates a special matrix object that can cache it's 
##              inverse matrix thus saving computation time if it's inverse is required  
##              frequently in a program.

makeCacheMatrix <- function(x = matrix()) {
  
  invX <- NULL
  
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) invX <<- inv
  
  getinverse <- function() invX
  
  # Create list of functions supported.
  list(set = set, get = get, getinverse = getinverse,
       setinverse = setinverse)  

}


## Write a short comment describing this function.
## Description: This function maintains inverse matrix in cache. The inverse is computed
##              when the inverse is called first time. For subsequent calls it returns
##              the precomputed inverse matrix that is maintained in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invX <- x$getinverse()
  
  ## if inverse matrix already exists in cache return it.
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  
  # if not compute inverse first time...
  data <- x$get()
  invX <- solve(data)
  x$setinverse(invX)
  
  invX  
}


## testing makeCacheMatrix() and cachesolve()
##
testvector <- makeCacheMatrix();

testvector$set( matrix(rnorm(16), nrow=4, ncol=4))

invmatrix <- cacheSolve(testvector)

testinv <- testvector$getinverse()

print(testinv)

testagain <- cacheSolve(testvector)

print (testagain)