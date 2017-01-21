## Written by Gordon Izatt 21 January 2017
## Programming Assignment 2 : Lexical Scoping
##
## This R script provides two functions 
## makeCacheMatrix and and cacheSolve intended to
## compute and cache the results of finding the
## inverse of a matrix
##
## Example of how to use these functions
## > test <- makeCacheMatrix(matrix(c(0,1,-1,0),2,2))
## > cacheSolve(test)
##       [,1] [,2]
## [1,]    0    1
## [2,]   -1    0

## The function makeCacheMatrix takes a matrix 
## as a parameter and returns a "special" 
## matrix object that can cache it's inverse.
## 
## The matrix must be square otherwise it reports
## an error message.
makeCacheMatrix <- function(pSourceMatrix = matrix()) {
    ## Initialise the cache
    vCacheInverseMatrix <- NULL
    
    ## Function to check square matrix
    checkSquareMatrixFunction <- function(pRows,pCol) {
        if (pRows != pCol) {
            ## Tell user we have a problem
            stop(paste("Matrix[",pRows,"x",pCol,"]","must be Square"))
        }
    }
    
    ## Function to set a new matrix
    setFunction <- function(pNewMatrix) {
        # Verify the matrix is square
        checkSquareMatrixFunction(nrow(pNewMatrix),ncol(pNewMatrix))
        
        # Now store the new matrix
        pSourceMatrix       <<- pNewMatrix
        
        # And as we have changed the matrix must clear cache
        vCacheInverseMatrix <<- NULL
    }
    
    ## Function to get the current matrix
    getFunction <- function() pSourceMatrix
    
    ## Function to collect the inverse of the matrix
    getCacheFunction <- function() vCacheInverseMatrix

    ## Function to set a cached version of the matrix
    setCacheFunction <- function(pInverseMatrix) 
        vCacheInverseMatrix <<- pInverseMatrix
    
    # Check Matrix is Square
    checkSquareMatrixFunction(nrow(pSourceMatrix),ncol(pSourceMatrix))
    
    # Return 
    list(set = setFunction, get = getFunction,
         setCache = setCacheFunction,
         getCache = getCacheFunction)
}


## The function cacheSolve computes the inverse of
## the "special" matrix returned by the function 
## makeCacheMatrix but has the advantage that if the
## inverse has already been been computed then it
## can provide the answer quicker using the cache
## of the inverse calculation
cacheSolve <- function(pMatrix, ...) {
    # are we able to return the cached inverse matrix?
    vCachedInverseMatrix <- pMatrix$getCache()
    if (!is.null(vCachedInverseMatrix)){
        ## YES!
        message("getting cached data")
        return(vCachedInverseMatrix)
    }
    
    # NO : so need to compute the inverse matrix
    vSourceMatrix <- pMatrix$get()
    vInverseMatrix <- solve(vSourceMatrix, ...)
    pMatrix$setCache(vInverseMatrix)
    vInverseMatrix
}
