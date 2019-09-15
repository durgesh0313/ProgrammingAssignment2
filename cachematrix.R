## The following function calculates the mean of the special "vector" created with the above function. 
##However, it first checks to see if the mean has already been calculated. 
##If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the 
##mean of the data and sets the value of the mean in the cache via the setmean function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
## Return a matrix that is the inverse of 'x'
S <- matrix(c(1,2,3,4),2,2)
S1 <- makeCacheMatrix(S)
cacheSolve(S1)