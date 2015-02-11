## First attempt, following the approach in the instructions to the assignment.
## The following functions are basically adaptations of the functions in the
## instructions to invert matrices instead of calculating means of vectors.

## These functions allow caching the inverse of a matrix 
## by defining the matrix as an object to which functions
## are attached. Taking advantage of the lexical scoping of R,
## the functions ensure that the inverse of the matrix remains in
## memory for as long as the matrix is not redefined, and can be 
## retrieved by appropriate use of functions.

## The function has the same structure as in the assignment

## This function defines the object that will contain the data
## in the matrix. The function contains other functions inside that
## take care of managing the information aroud the matrix,
## including its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes as argument an object as defined by the 
## previous function and sets all the needed information for 
## subsequent retrieval, includign its inverse. Once the object
## set, retrieving the inverse can be done without recalculating it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## This section of the code calls the functions
# 1. create the object with values
x <- makeCacheMatrix(matrix(rnorm(16), 4, 4))
x$get()         # check that the matrix was correctly created
cacheSolve(x)   # retrieve the inverse, with calculation
cacheSolve(x)   # retrieve the inverse directly from memory
# 2. redo, with new matrix
x$set(matrix(rnorm(25), 5, 5))
cacheSolve(x)
cacheSolve(x)

## And now, let's try a different approach.
## Let's buid a single function that will get the same results, also 
## using lexical scoping.

## This function creates functions to access values of a matrix,
## its inverse and its determinant. All are kept in memory in the
## function's frame.
mymatrix <- function(x = matrix(), ...) {
	determinant <- NULL
	inverse <- NULL
	set <- function(y, ...) {
		x <<- y
		determinant <<- base::det(x)
		if (abs(determinant) > 1e-6) inverse <<- solve(x, ...)
		else inverse <<- NULL
	}
	get <- function() x
	inv <- function() inverse
	det <- function() determinant
	set(x, ...)
	list(set = set, get = get, inv = inv, det = det)
}

# test of mymatrix()
#source('mymatrix.R')
x <- mymatrix(matrix(rnorm(5*5), 5, 5))
x$get()
x$det()
x$inv()
solve(x$get())

