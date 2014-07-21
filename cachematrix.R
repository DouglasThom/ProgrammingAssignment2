## cachematrix.R
## created by Douglas Thom
## This script creates a caching mechanism for the long operation of
## inverting a matrix.
## Created for R Programming Course: Programming Assignment 2

## makeCacheMatrix creates a matrix manager allowing for the caching of
## a solve'd matrix.

makeCacheMatrix <- function(sourceMatrix = matrix()) {
        inverseMatrix <- NULL
        set <- function(setMatrix) {
                sourceMatrix <<- setMatrix
                inverseMatrix <<- NULL
        }
        get <- function() sourceMatrix
        setMatrix <- function(newMatrix) inverseMatrix <<- newMatrix
        getMatrix <- function() inverseMatrix
        list(set = set, get = get,
                setMatrix = setMatrix,
                getMatrix = getMatrix)
}

getwd
## cacheSolve enables the matrix manager to generate a solution
## for a matrix and save it to the cache.

cacheSolve <- function(x, ...) {
        solvedMatrix <- x$getMatrix()
        if ( !is.null( solvedMatrix ) ) {
                message( "Returning cached matrix." )
                return( solvedMatrix )
        }
        message( "Generating new matrix" )
        matrixData <- x$get()
        newSolution <- solve( matrixData, ... )
        x$setMatrix( newSolution )
        
        ## Return a matrix that is the inverse of 'x'
        newSolution
}
