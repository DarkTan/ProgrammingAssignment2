# makeCacheMatrix:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #inverse uses for store the cached inverse matrix
  inverseData <- NULL
  #can set matrix
  set <- function(y){
    x <<- y
    inverse <- NULL
  }
  #can get matrix
  get <- function() x
  #set Inverse matrix
  setInverse <- function(inverse){
    inverseData <<- inverse
  }
  #get inverse matrix
  getInverse <- function() inverseData
  #return the new matrix
  list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}


# cacheSolve: Compute the inverse of the matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  #if Inverse matrix has been calculated, return itself
  if (!is.null(inverse)) {
    message("get cached data!")
    return(inverse)
  }
  #else we calculate
  data <- x$get()
  inverse <- solve(data, ...)
  #set cached inverse matrix 
  x$setinv(inv)
  #return
  inverse
}
