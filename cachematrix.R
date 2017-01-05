# Create an R object to store a matrix x and its inverse invm
makeMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinv <- function(i) invm <<- i
  getinv <- function() invm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Retrieve the inverse matrix from cached value stored in makeMatrix
# This requires an argument returned by makeMatrix
# Call makeMAtrix FIRST
cacheInv <- function(x, ...) {
  
  invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached data")
   
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinv(invm)
  
  invm
}

#to run:
# x=matrix(1:4,2,2)
# y=makeMatrix(x)
# check using y$get()
# cacheInv(y)  to be run twice once calc. then cached!