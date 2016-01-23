## makeCachematrix is a function that creates matrix

makeCachematrix <- function(x = matrix()) {
  inv <- NULL
    set <- function(y) { #set is a function that changes the matrix makeCacheMatrix
    x <<- y #substitutes the vector x with y in the main function
    inv <<- NULL #restores to NULL the value of inv as old inv is not needed
  }

  get <- function() x # get returns the matrix stored in the main function
  
  setInverse <- function(inverse) inv <<- inverse #it stores the value 
  getInverse <- function() inv #returns it via getInverse
  
  list(set = set, get = get, #for storing all the 4 functions
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() #check the value of inverse stored
  if(!is.null(inv)) { #if not NULL return the following message
  message("getting cached data")
  return(inv)
  }
  storedmatrix <- x$get() #matrix stored with makeCacheMatrix
  inv <- solve(storedmatrix, ...) #solvecalculates the inverse of matrix
  x$setInverse(inv)
  inv
}
