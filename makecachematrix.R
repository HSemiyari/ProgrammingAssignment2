##We have two functions "makecachematrix" and "cachsolve"
##The first function This function creates a special "matrix" 
##object that can cache its inverse. 
##The second function calculates the inverse
##(if inverse has not already exists)
makecachematrix <- function(m = matrix()) {
  # Assume the matrix is invertible. 
  inv.m <- NULL
  #Set the value of matrix
  set <- function (y) {
    m <<- y
    inv.m <<- NULL
  }
  #Get value of the matrix
  get <- function() m
  #Set the value of inverse matrix
  setinverse <- function (inverse) inv.m <<- inverse
  #Get the value of inverse matrix
  getinverse <- function() inv.m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function calculates the inverse of the matrix
#created with the above function. 
#It checks if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skip the computation.
#Otherwise, it calculate the inverse and set the value of 
# the inverse in the cache via the setinverse function.

cachesolve <- function (m, ...){
  inv.m <- m$getinverse()
  if(!is.null(inv.m)){
    message("getting cached data")
    return(inv.m)
  }
  M <- m$get()
  inv.m <- solve(M, ...)
  m$setinverse(inv.m)
  inv.m
}

