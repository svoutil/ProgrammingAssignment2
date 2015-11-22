## A set of functions that implement a wrapped matrix that supports caching 
## the result of calling "solve" on the matrix.
## These functions are to be used in combination.
## makeCacheMatrix must be called first to create a wrapped matrix.
## cacheSolve is used in place of "solve" with a wrapped matrix and returns
## a prevously cached value, if available.

## makeCacheMatrix -- creates a wrapped version of a basic matrix that can
## cache the result of solving the matrix.
## The wrapped matrix is represented as a list of methods to get/set the 
## matrix itself as well as get/set the cached result for calling solve
## on the matrix.
##
## Parameters:
##  m : the initial matrix to wrap (defaults to an empty matrix)
##
## Returns: 
##  A wrapped matrix represented as a list of get/set methods for
##  the matrix and cached result
makeCacheMatrix <- function(m = matrix()) {
    ## the cached solution; initially NULL to indicate that a solution 
    ## has not yet been cached
    sol <- NULL
    
    ## store the original matrix
    set <- function(matrix) {
        ## Store the matrix in the scope of the makeCacheMatrix function
        ## (specifically in the parameter m passed to the function.)
        m <<- matrix
        
        ## Clear out any cached solution since it may no longer be valid
        ## for the new matrix
        sol <<- NULL
    }
    
    ## fetch the original matrix
    get <- function() m
    
    ## store the solution
    setsolution <- function(solution) sol <<- solution
    
    ## get the cached solution
    getsolution <- function() sol
    
    # a list of the above methods to represent the wrapped matrix
    list(set=set, get=get, 
         setsolution=setsolution, getsolution=getsolution)
}


## cacheSolve -- version of the "solve" function to be used with a wrapped
## matrix created by the makeCacheMatrix function.  Use a previously cached
## result if available.
##
## Parameters:
##  wrappedMatrix : the wrapped matrix which to solve
##  ... : additional arguments to pass to solve
##
## Returns:
##  The result of calling the "solve" function with the matrix stored in 
##  wrappedMatrix and any additional parameters passed to cacheSolve
cacheSolve <- function(wrappedMatrix, ...) {
    ## Get any cached solution; will return NULL if a cached solution 
    ## does not exist
    solution <- wrappedMatrix$getsolution()
    
    if (is.null(solution)) {
        ## A cached solution did not exist, solve it now
        solution <- solve(wrappedMatrix$get(), ...)
        ## Cache the solution
        wrappedMatrix$setsolution(solution)
    } else {
        message("using cached data")
    }
    
    ## return the solution
    solution
}
