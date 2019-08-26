## This function is proposed as part of Programming Assignment 2: Lexical Scoping
## from Data Science Course of Johns Hopkins Univertity
## The overall idea is to create a numeric square that can be invertible.
## The calculus for invertible matrix will be done one time and cached in the 
## memory. Since it was asked again to determine the invertible of the same matrix
## the result will be retrieved from cache and not calculated at all.

## Write a short comment describing this function   

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
