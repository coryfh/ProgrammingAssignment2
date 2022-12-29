## Creates special list vector that s
## 1. Sets & gets value of vector, 
## 2. sets & gets inverse values of the vector

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(solve) i <<- solve
  getInv <-  function() i
  list(set=set, get=get, 
       setInv=setInv, 
       getInv=getInv)
}

## Returns a matrix that is the inverse of inputted matrix x
cacheSolve <- function(x, ...) {
    i <- x$getInv()
    if(!is.null(i)){
      message("getting cached matrix")
      return (i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInv(i)
    i
}