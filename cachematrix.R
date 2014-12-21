## RProgram for Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## Function makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Function cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve will retrieve the inverse from the cache.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # This is reset to null every time makeCacheMatrix is called
  
  get <- function() { x }   # this function returns the value of the original matrix
  
  setInverseMatrix <- function(inverseMatrix)  { i <<- inverseMatrix }
  # this is called by cacheSolve() during the first cacheSolve()
  #  access and it will store the value using superassignment
  
  getInverseMatrix <- function() { i } # this will return the cached value to cachemean() on
  #  subsequent accesses
  
  list(get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix) 
  
}


## Computes the inverse of the matrix created using makeCacheMatrix

cacheSolve <- function(x, ...) { # the input 'x' is created by makeCacheMatrix
  i <- x$getInverseMatrix() # accesses the object 'x' and gets the Inverse Matrix
  
  if(!is.null(i)) {              # if its already cached
    message("getting cached data") 
    return(i)
  }
  
  matrix <- x$get() #Fetch the Matrix using get
  
  i <- solve(x)
  x$setInverseMatrix(i) # Cache the Matrix
  i # return inverse

}
