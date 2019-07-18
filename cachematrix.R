## Assignment: Caching the Inverse of a Matrix

## cachematrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ##Set Matric
  set <- function(matrix){
    x <<- matrix
    i <<- NULL
  }
  ##Get Matrix
  get <- function(){
 ##Return Matrix
    x
  }
  ## set the inverse of the matrix
  setInverse <- function(inverse){
    i <- inverse
  }
  ## get the inverse of the matrix
  getInverse <- function() {
   ## Return the inverse function
    i
  }  
  ##return list of actions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function that computes the inverse of the special
"matrix"

cacheSolve <- function(x, ...) {
## return matrix that is the inverse of x
  m <- x$getInverse()
##   return m if inverse is already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ## get the matrix
  data <- x$get()
  ## work out the matrix by multiplication
  m <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInverse(m)
  ## return the matrix
  m
}
