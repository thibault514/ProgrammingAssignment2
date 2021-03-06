

## Write a short comment describing this function

setwd("/Users/charlesthibault/Documents/Learning R")

# Create a function that creates my special matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # check if x is really a matrix
  if(is.matrix(x)==FALSE){  
    print("x is not a Matrix")
  }
  
  # resets all of the content to NULL       
  inverse <- NULL
  # use the super assignment operator to give a value outside of this enviromnent
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # getters and setters...
  get <- function() x
  setinverse <- function(calculated_inverse) inverse <<- calculated_inverse
  getinverse <- function() inverse
  # put all of these guys in a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Gets my inverse

cacheSolve <- function(x, ...) {
  
  # if I have the inverse, get it from the MakeCacheMatrix      
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # Otherwise get the inverse and return my_inv      
  data <- x$get()
  my_inv=solve(data)
  x$setinverse(my_inv)
  # return the inverse
  my_inv
  
}


