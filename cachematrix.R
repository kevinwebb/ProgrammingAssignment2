## Below are two functions that are used to create a special object that stores a matrix and cache it's inverse
## Example execution
## a <- makeCacheMatrix()
## a$set(matrix(5:8,2,2))
## cacheSolve(a)
## cacheSolve(a)
## Run a second time to show message that the cache is working

## This function creates a special "matrix", which is a list containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the matrix inversion
## 4. Get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<-y
    s <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated and if so gets the result from the cache and skips the calculation.
## Otherwise, it calculates the inverse of the matrix using the built-in solve function and sets the value in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  
  if(!is.null(s)) {
    message("Getting cached data")
    return(s)
  }
  
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
