## R Programming Assignment 2
## Lin Chen
## June 19, 2014

##Test
##source("cachematrix.R")
##c=rbind(c(1, -1/4), c(-1/4, 1)) 
##l = makeCacheMatrix(c)
##cacheSolve(l)     ##first time will implement the solve() function
##cacheSolve(l)     ##second call will get the inverse from the cache

##-----------------------
##makecacheMatrix(x = matrix)
##Input: a matrix
##Ouput: a list contains the get, set, setinverse, and getinverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m

  list(set = set, get = get,
       setinverse = setInverse,
       getinverse = getInverse)
}

##-----------------------
##cacheSolve(x, ...)
##Input: a list contains the get, set, setinverse, and getinverse
##Ouput: the inverse of the matrix defined in the input list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data);
  x$setinverse(m)
  m
}
