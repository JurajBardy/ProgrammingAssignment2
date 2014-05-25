## Programming Assignment 2: pair of functions that cache the inverse of a matr## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  ## This function creates a special "matrix" object that can cache its inverse.
  
  iMatrix <- NULL # inverse matrix variable
  
  set <- function(y) {
    x <<- y # matrix variable
    iMatrix <<- NULL # in case of seting matrix variable, inverse matrix is deleted   
  }
  get <- function() x
  
  setInverse <- function(mx) iMatrix <<- mx
  
  getInverse <- function() iMatrix
       
  list(set = set, get = get,  
       setInverse = setInverse,       
       getInverse = getInverse) # special list
 }
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
    
  mx <- x$getInverse() # check if the inverse matrix is computed
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  data <- x$get() # if not, compute an inverse matrix by solve()
  mx <- solve(data)
  x$setInverse(mx)
  mx # return inverse matrix
  
}
