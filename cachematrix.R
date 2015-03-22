##the function below greates a list of functions to do with the matrix
##get set setinverse getinverse

## Write a short comment describing this function
## check if the matrix is singular if it is stop
makeCacheMatrix <- function(x=matrix()) 
{
  
  isNotSingular <- det(x)
  if (isNotSingular == 0)
  {
    stop("Matrix is singular no inverse")
  }
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<-inverse
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
  
}

## check is inverse matrix exist if not get it using solve()
## if it exists then just return the cached mtx
cacheSolve <- function(x, ...) 
{
  mtx <- x$getinverse()
  if ( is.null(mtx)) 
  {
    invmtx <- x$get()
    mtx <- solve(invmtx)
    x$setinverse(m)
    mtx
  }
  else
  {
    print("inverse exists getting cached inverse matrix")
    return(mtx)
  }
}




