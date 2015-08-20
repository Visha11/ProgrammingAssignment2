## Both functions combined cache a matrix

## makeCacheMatrix function has 4 functions-For setting the value of matrix, getting the value of matrix,
## setting the valueof inverse and getting the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## CacheSolve function checks if the Inverse already exists through the getinverse() function
## Else solves for the inverse and assigns it through setinverse()

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data)
  x$setinverse(inv)
  inv
}
