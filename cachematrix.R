## Usage Example
## m <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3) # create a invertable matrix of integers
## cachedMatrix <- makeCacheMatrix(m) # create the cached matrix
## cacheSolve(cachedMatrix) #returns inverse ---> matrix(c(-24,18,5,20,-15,-4,-5,4,1), nrow=3, ncol=3) 
## cacheSolve(cachedMatrix) (any subsequent call will return the cached value)

## This function creates a enhanced "matrix" object that can cache its inverse. 
## The enhanced "matrix" is a list of functions that provide added functionality to matrix: 
## set: set current value of the matrix
## get: get current value of the matrix
## setinverse: set the value of the inverse matrix
## getinverse: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse with NULL
  invMatrix <- NULL
  
  #set the value of 'x' matrix, also reset value of 'invMatrix' to NULL
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
    
  }

  #getter function to return current value of 'x' matrix
  get <- function(){
    x
  } 
  
  #setter function to set the cached value of 'invMatrix'
  setinverse <- function(inverse){
    invMatrix <<- inverse
  }
  
  #getter function to return the cached value of 'invMatrix'
  getinverse <- function() {
    invMatrix
  }
  
  #defines all functions available on the 'x' matrix
  list(
    getinverse = getinverse,
    setinverse = setinverse,
    get = get,
    set = set
  )
}
  
## Write a short comment describing this function


cacheSolve <- function(mat) {
  #get the current value of inv in 'mat'
  invMatrix <- mat$getinverse()
  
  #if current mean 'm' is not null, means cache is available and use it
  if(!is.null(invMatrix)){
    message("getting cached inverse matrix")
    return (invMatrix)
  }
  #else if 'm' is null, means cache needs to be calculated
  
  #get the vector from 'vec'
  data <- mat$get()
  
  #calculate the new mean and assign it to 'm'
  invMatrix <- solve(data)
  
  #also set the calculated mean in the vec's mean. So cached will be used next time
  mat$setinverse(invMatrix)
  
  return(invMatrix)
}

#m <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
#myMatrix <- makeCacheMatrix(m)
#myMatrix$getinverse()
#myMatrix$setinverse(matrix(c(1,2,3,4,5,6,7,8,9), nrow=3, ncol=3))
#cacheSolve(myMatrix)