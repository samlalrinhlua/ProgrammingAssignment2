# makeCacheMatrix() will return a value for the matrix 'x' and cache it
# CacheSolve() will calculate inverse of matrix

makeCacheMatrix <- function(x=matrix()){
  
  #The first function, makeCacheMatrix creates a special "vector", 
  #which is really a list containing a function to
    #set the value of the matrix
    #get the value of the matrix
    #set the value of the inverse matrix
    #get the value of the inverse matrix
  
  # the list will be used as an input to the next function CacheSolve()
  m <- NULL
  # m will be used to store the inverse value
  
  set <- function(y){
    x <<- y
    m <<- NULL
  # use '<<-' to assign a value to an object in a different environment 
    }
  
  get <- function()x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
 list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
  
}

cacheSolve <- function(x, ...) {
  
  # 'x' is a value obtained from makeCacheMatrix()
  # This function will return a value that is an inverse of x
  m <- x$getmatrix()
  
  # if the inverse has already been calculated, print the following data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if the inverse has not been calculated, calculate here
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  return(m)
}
