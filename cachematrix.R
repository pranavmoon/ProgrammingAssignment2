## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Below are two functions that are used to create a special 
## object that stores a numeric matrix and cache's its inverse.

## The first function, makeCaheMatrix creates a special "matrix", 
## which is really a list containing a function to

##    set the value of the matrix:  setmat
##    get the value of the matrix:  getmat
##    set the value of the inverse of a matrix: setinv
##    get the value of the inverse of a matrix: getinv

makeCacheMatrix <- function(x = matrix()) {

	  inv <- NULL
        setmat <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmat <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(setmat = setmat, getmat = getmat,
             setinv = setinv,
             getinv = getinv)
	
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$getmat()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
