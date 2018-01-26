## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
  #initiate the inverse variable
  inverse <- NULL
  
  #store matrix
  setMatrix <- function(mat) {
    x <<- mat
    inverse <<- NULL
  }
  
  #basic get/set functions
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  getMatrix <- function() x
  
  #list of functions
  list(setMatrix = setMatrix, setInverse = setInverse, getInverse = getInverse, getMatrix = getMatrix)
  
}


## Write a short comment describing this function

cacheSolve <- function(y, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- y$getInverse()
  
  #if inverse has already been calculated, return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #if inverse hasn't been calculated, then calculate it, store it and return it
  data <- y$getMatrix()
  inverse <- solve(data)
  y$setInverse(inverse)
  inverse
}

