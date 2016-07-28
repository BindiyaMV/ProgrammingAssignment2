##makeCacheMatrix creates a matrix 
##which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the inverse of the matrix
##get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## CacheSolve Function calculates the inverse of the matrix
##created with the above function. 

cacheSolve<- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("Cached Data : ")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
}
