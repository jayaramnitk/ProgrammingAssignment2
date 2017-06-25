## This function retrives the inverse of an input matrix from cache


## makeCacheMatrix function creates a R object that stores matrix  its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(matr) m <<- matr
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


## cacheSolve retrives the inverse of the matrix from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m 
}
