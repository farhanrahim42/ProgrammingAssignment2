## Programs function will cache the inverse of matrix
##
## The example function:
##
## > source('cachematrix.R')
## > z <- makeCacheMatrix(matrix(c(4, 0, 0, 4), c(2, 2)))
## > cacheSolve(z)
## [,1] [,2]
## [1,]  0.25  0.0
## [2,]  0.0  0.25

## Create function matrix that contain =
##   - set the value of the matrix (set)
##   - get the value of the matrix (get)
##   - get the value of the inverse matrix (getinverse)
##   - set the value of the inverse matrix (setinverse)
##   

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Will create the inverse matrix
## If recalled cacheSolve,the value reuse from cached data if available

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  z <- x$get()
  i <- solve(z, ...)
  x$setinverse(i)
  i
}


