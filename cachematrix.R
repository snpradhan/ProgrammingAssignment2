## makeCacheMatrix takes a matrix as a parameter and returns an object that
## allows the matrix inverse to be cached
## cacheSolve takes the matrix object as a paramter and retuns the inverse either by retrieving it 
## from the cache or computing the inverse and updating the cache

## This function creates an object that is a list containing the following
## a function that sets the value of the matrix
## a function that returns the value of the matrix
## a function that sets the value of the inverse matrix
## a function that returns the value of the inverse matrix
## a function that checks if the internal matrix has changed since the inverse was last computed

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  changed <- TRUE
  set <- function(y) {
    x <<- y
    i <<- NULL
    changed <<- TRUE
  }
  get <- function(){
    x
  } 
  setinverse <- function(inv) {
    i <<- inv
    changed <<- FALSE
  }
  getinverse <- function() {
    i
  }
  haschanged <- function() {
    changed
  }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse, haschanged = haschanged)
}


## The function takes the matrix object as a parameter and checks if the
## internal matrix has changed since the inverse was computed last.
## If the matrix has not changed then return the inverse from the cache,
## otherwise recomputes the inverse, updates the cache and return the newly 
## computed inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  changed <- x$haschanged()
  if(!is.null(i) & changed == FALSE) {
    message('getting cached inverse')
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  
}
