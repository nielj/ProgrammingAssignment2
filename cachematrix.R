## This function creates a special "matrix" object that can cache its inverse
#  assume that the matrix supplied is always invertible
#  the inverse of a square matrix can be done with the solve function in R
#  ex: if X is a square invertible matrix, then solve(X) returns its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setvalue <- function(value) { message("setvalue"); m <<- value }
  getvalue <- function() { message("getvalue"); m }
  message("returning an object type list")
  list(set = set, get = get,
       setvalue = setvalue,
       getvalue = getvalue)  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache (setting and getting the cache object values)

cacheSolve <- function(x, ...) {
  m <- x$getvalue()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  message("calculating inverse of matrix")
  m <- solve(data, ...)
  x$setvalue(m)
  m
}

## Test Script: Thanks to Chris Craig's class comments
## (ref: https://class.coursera.org/rprog-009/forum/profile?user_id=3499813)

# Testing by creating a square matrix and getting the inverse with outputs
A <- matrix(c(2,4,3,1),nrow=2,ncol=2,byrow=TRUE)
solve(A)

# use cacheSolve() to compute and store the inverse of A (via B)
B <- makeCacheMatrix(A)
cacheSolve(B)

## output below
#getvalue
#calculating inverse of matrix <- show that the amtrix inverse is being calculated
#setvalue
#[,1] [,2]
#[1,] -0.1  0.4
#[2,]  0.3 -0.2

# once the inverse has been calculated, subsequent calls to cacheSolve()
# with the same "matrix" object will not recalculate but return the result
cacheSolve(B)

# output below
#getvalue
#getting cached data <- show that this time the value is not calculated, but it is retrieved from cache
#[,1] [,2]
#[1,] -0.1  0.4
#[2,]  0.3 -0.2

