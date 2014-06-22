## Matrix inversion is a process that has high computational cost(memory and time) 
##For large martices (eg. 1000X1000) it may take too long to compute the inverse and caching the value of inverse is useful

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve  ##Solve is a generic function for calculating inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## CacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)                      ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##A = matrix(c(1,5,6,8,4,9,22,43,33),nrow=3,ncol=3,byrow = FALSE)  ##An example matrix to verify above functions
##cacheSolve(makeCacheMatrix(A))