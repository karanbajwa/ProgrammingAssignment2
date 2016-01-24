## Usage Example
## m <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3) # create a invertable matrix of integers
## cachedMatrix <- makeCacheMatrix(m) # create the cached matrix
## cacheSolve(cachedMatrix) #returns inverse ---> matrix(c(-24,18,5,20,-15,-4,-5,4,1), nrow=3, ncol=3) 
## cacheSolve(cachedMatrix) (any subsequent call will return the cached value)


## This function creates a special "matrix" object that can cache its inverse. 
## The enhanced "matrix" is a list of functions that provide added functionality to matrix: 
## set: set current value of the matrix
## get: get current value of the matrix
## setinverse: set the value of the inverse matrix
## getinverse: get the value of the inverse matrix
## hasMatrixChanged: returns TRUE or FALSE to indicate if the matrix has changed

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse with NULL
  invMatrix <- NULL
  
  # Initialize symbol to indicate matrix has changed
  hasChanged <- TRUE

  #set the value of 'x' matrix, also reset value of 'invMatrix' to NULL
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
    hasChanged <<- TRUE
  }

  #getter function to return current value of 'x' matrix
  get <- function(){
    x
  } 
  
  #setter function to set the cached value of 'invMatrix'
  setinverse <- function(inverse){
    invMatrix <<- inverse
    hasChanged <<- FALSE
  }
  
  #getter function to return the cached value of 'invMatrix'
  getinverse <- function() {
    invMatrix
  }
  
  #getter function to indicate if the matrix has changed
  hasMatrixChanged <- function(){
    hasChanged
  }

  #defines all functions available on the 'x' matrix
  list(
    getinverse = getinverse,
    setinverse = setinverse,
    hasMatrixChanged = hasMatrixChanged,
    get = get,
    set = set
  )
}
  


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache, else the 
## inverse is calculated using 'solve' and stored back in cache for future use.

cacheSolve <- function(mat) {
  #get the current value of inv in 'mat'
  invMatrix <- mat$getinverse()
  
  #if current inverse 'invMatrix' is not null and matrix has not changed, 
  #then cache must be available, so use it
  
  if(!is.null(invMatrix) && !mat$hasMatrixChanged() ){
    message("geting inverse matrix from cache")
    return (invMatrix)
  }
  
  #else if 'invMatrix' is null, it means cache is not available, and inverse
  #needs to be calculated
  #get current matrix
  currentMatrix <- mat$get()
  
  #calculate the inverse and assign it to 'invMatrix'
  invMatrix <- solve(currentMatrix)
  
  #also set the calculated inverse in cached matrix object. So cached will be used next time
  mat$setinverse(invMatrix)
  
  return(invMatrix)
}