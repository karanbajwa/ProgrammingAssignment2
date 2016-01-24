## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
    
  }

  get <- function(){
    x
  } 
  
  setinverse <- function(inverse){
    invMatrix <<- inverse
  }
  
  getinverse <- function() {
    invMatrix
  }
  
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