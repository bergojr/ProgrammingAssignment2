## This function is proposed as part of Programming Assignment 2: Lexical Scoping
## from Data Science Course of Johns Hopkins Univertity
## The overall idea is to create a numeric square that can be invertible.
## The calculus for invertible matrix will be done one time and cached in the 
## memory. Since it was asked again to determine the invertible of the same matrix
## the result will be retrieved from cache and not calculated at all.

## This function receives a invertible square matrix (this is very important) and
## return a list of function that can be used in the new object returned.
## get -> read the created matrix
## set -> substitute the content of previous matrix and set this inverser to NULL, there is
##        the inverse must be calculated.
## getinv -> call the function cachesolve
## setinv -> used by cachesolve


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


## This function will receive neither the square matrix passed to makeCacheMatrix or seted
## by set functio.
## case, the inverse is NULL, the inverse will be calculate and cached. If inverse is already
## cached, this cached value will be returned instead of a new calculated one.

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
