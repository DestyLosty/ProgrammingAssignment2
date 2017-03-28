## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ##Initializing the value of inv
  set <- function(y){ ##Reinitializing the values of 'x' and 'inv'
    x <<- y
    m <<- NULL
  }
  get <- function() x   ##Printing (or assigning) the matrix 'x'
  setinverse <- function(inverse) inv <<- inverse ##Assigning the value of 'inverse' to 'inv'
  getinverse <- function() inv ##Printing (or assigning the matrix 'inv')
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) ##Naming functions

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse() ##Assigning the matrix 'x' cached in the first function.
  if(!is.null(inv)){     ##Printing a message.
    message("Getting Cached Data")
    return(inv)
  }
  data <- x$get() ##Reading cached data.
  inv <- solve(data,...)  ##Computing (if possible) the inverse matrix.
  x$setinverse(inv) ##Caching this data to the main function.
  inv ##Printing the inverse matrix.
}
