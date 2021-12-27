## This function gives the inverse of a matrix
## it evaluates the cached data first
## then uses the solve command to find the inverse
## it involves lexical scoping <<- command
## which will ensure the variable keeps going through
## all the nevironments until it finds similarly named variable 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    # double arrow is used in conjuction with a closer 
    # it will keep going through the environments in order until
    #it finds a variable with that name, and it will assign it to that.
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## this function gives the inverse using solve command
## whose description is given as a comment below

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...) #solve is default function for solves 
  #the equation a %*% x = b for x, where b can be either a vector or a matrix.
  x$setInverse(inv)
  inv
}
