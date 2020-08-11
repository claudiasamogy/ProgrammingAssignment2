#This function creates an object (matrix) that can cache its inverse

makeCacheMatrix = function(x = matrix()){
  invers = NULL
  set = function(y){
    x <<- y
    invers <<- NULL
  }
  result = function() (x)
  setinverse = function(inverse) (invers <<- inverse)
  getinverse = function() invers
  list(set=set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#This second step computes the inverse of the object created in step one. 
cacheSolve = function(x, ...) {
  inv = x$getinverse()
  if(!is.null(invers)){
    message("Getting cached data")
    return(invers)
  }
  mat = x$get()
  invers = solve(mat, ...)
  x$setinverse(invers)
  invers
}