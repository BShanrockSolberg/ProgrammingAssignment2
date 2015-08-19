#################################################################################
## The functions in this program are designed to reduce the cost of inverting
## a matrix.  There are two functions in this program:
##
## makeCacheMatrix: makes a list of functions whose elements:
##      1. create a matrix
##      2. cache the matrix
##      3. invert the matrix
##      4. cache the inverted matrix
##
## cacheSolve: Requires the list from makeCacheMatrix to function
##      Returns the inverted matrix, using cache if it exists or
##      performing the "solve" function if cache does not exist


## makeCacheMatrix --------------------------------------------------------------
##      Args:    x:  Must be a matrix
##      Returns: List with following functions:
##               (1) setMatrix  - caches matrix
##               (2) getMatrix  - reads matrix from cache
##               (3) setInverse - caches inverted matrix
##               (4) getInverse - reads inverted matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        ## define the four return functions
        s <- NULL
        setMatrix  <- function(y) {
                x <<- y
                s <<- NULL
        }       ## end function setMatrix
        getMatrix  <- function() x
        setInverse <- function(solve) s <<- solve
        getInverse <- function() s
        ## return the list with the four functions ordered properly
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}       ## end function makeCacheMatrix


## cacheSolve -------------------------------------------------------------------
##      Args:    List output from makeCacheMatrix
##      Returns: inverted matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        } 
        ## If not cached, calculate the inverse & return
        data <- x$getMatrix()
        s <- solve(data, ...)
        x$setMatrix(s)
        s
}       ## end function cacheSolve
