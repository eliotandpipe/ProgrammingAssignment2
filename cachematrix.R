## Assignment: Caching the Inverse of a Matrix

## cachematrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(matrix){
    x <<- matrix
    i <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setInverse <- function(inverse){
    i <- inverse
  }
  
  getInverse <- function() {
    i
  }  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function that computes the inverse of the special
"matrix"

cacheSolve <- function(x, ...) {

  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data) %*% data
  
  x$setInverse(m)
  
  m
}
