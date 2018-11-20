## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function returns a list of objects and save the inverse of a matrix in cahce

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)

}


## Write a short comment describing this function

# This function checkes if the inverse is already cached and if not then solves the matrix for inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinverse(inv)
  inv
}


set.seed(1123)
mat4inv <- matrix(rnorm(25), nrow = 5, ncol = 5)

cacheMat <- makeCacheMatrix(mat4inv)
cacheSolve(cacheMat)
solve(mat4inv)
