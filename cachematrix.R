## Returns a list object containing accessor functions for caching a matrix inversion calculation.
## set(y) - sets matrix x
## get() - returns matrix
## setSolve(x) - sets inverted matrix calculation x
## getSolve() - returns inverted matrix calculation
## Parameters
## x : matrix to be inverted (defaults to empty matrix)
## Invariants: x is a matrix and is invertable
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  # input matrix setter
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  # input matrix getter
  get <- function() x
  
  # cache inverted matrix calculation setter
  setSolve <- function(solve) s <<- solve
  
  # cache inverted matrix calculation getter
  getSolve <- function() s
  
  # return list object with function elements
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Calculates inverted matrix and caches result. Future calls return cached value.
## Params
## x : makeCacheMatrix(m) list object containing invertable matrix m
cacheSolve <- function(x) {
  
  # check if already calculated
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  # otherwise compute the inverse
  data <- x$get()
  s <- solve(data)
  # cache
  x$setSolve(s)
  
  ## Return a matrix that is the inverse
  s
}

m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
cm <- makeCacheMatrix(m)
a <- cacheSolve(cm)
b <- cacheSolve(cm)
m2 <- matrix(c(0, 8, 16, 0), nrow = 2, ncol = 2, byrow = TRUE)
cm2 <- makeCacheMatrix()
cm2$set(m2)
c <- cacheSolve(cm2)
d <- cacheSolve(cm2)