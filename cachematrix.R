## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix 
#3. set the value of the inversed matrix
#4. get the value of the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
  ix <- NULL #inversed matrix
  set <- function(y) {
    x <<- y
    ix <-- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ix <<- inverse
  getinverse <- function() ix
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. 
# However, it first checks to see if the inversed matrix has already been calculated. 
# If so, it gets the inversed matrix from the cache and skips the computation. 
# Otherwise, it calculates the inversed matrix and sets the value of the inversed matrix in the cache via the setinverse function.

cacheSolve <- function(x) {
  ix <- x$getinverse()
  if(!is.null(ix)){
        message("getting cached data")
        return(ix)
    
  }
  matrix <- x$get()
  ix <- solve(matrix)
  x$setinverse(ix)
  ix
  ## Return a matrix that is the inverse of 'x' - ix
}
