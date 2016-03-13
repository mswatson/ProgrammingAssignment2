## Write a short comment describing this function
## Return a matrix         that is the inverse of 'x'
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}