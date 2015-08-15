## Put comments here that give an overall description of what your
## functions do

## Creates a matrix wrapper object that can cache its inverse
##  setInverse function uses solve() to create the inverse and cache it
##  getInverse function returns the inverse, which may be null if not
##      previously saved

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function( y ) {
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function() inverse <<- solve( x )
    getInverse <- function() inverse
    list( setMatrix = setMatrix, getMatrix = getMatrix,
          setInverse = setInverse, getInverse = getInverse )
}


## This function uses the functions of the makeCacheMatrix object 
##  to compute an inverse if necessary, or use the cached one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if ( !is.null( inv )) {
        message( "getting cached inverse" )
        return( inv )
    }
    x$setInverse()
    x$getInverse()
}
