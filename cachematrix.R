## This R Script is my solution for Coursera Course R Programming (RPROG-006) Programming Assignment 2: Lexical Scoping
##
## makeCacheMatrix & cacheSolve are two functions that work together in order to generate the inverse of a square matrix using a caching tecnique.

## Function call procedure is;
##   tempvariable <- makeCacheMatrix(matrixtoinvert)
##   inverse <- cacheSolve(tempvariable)

## Initial call will calculate the inverse, subsequent calls will use the cached answer and display an alert.


makeCacheMatrix <- function(x = matrix()) {
       
        ## Description
        ## This function performs the caching management for the function cacheSolve. It does this by creating methods (see below) for the passed matrix.
        ## The calling function, cacheSolve, determines which method to use.
        ##
        ## Usage
        ## Needs to be used in conjunction with the function cacheSolve
        ##
        ## Arguments
        ## x     A square matrix that is to be inverted
        ## inv   Cached version of the inverse of x, stored in the parent environment
        ##
        ## Methods / Calls
        ## get()    Returns the matrix that was last inverted
        ## set()    Initialises a special version of the matrix to be inverted
        ## getinv() Get the cached version of the inverted matrix
        ## setinv() Set the cached version of the inverted matrix to the passed variable
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        ##       
        ## Description
        ## Calculates the inverse of a square matrix but uses a caching technique to minimise calculation effort if a repeat call is made.
        ##
        ## Usage
        ## Needs to be used in conjunction with the function makeCacheMatrix
        ##
        ## Arguments
        ## x A square matrix that is to be inverted (assumed that inverse always exsits)
        
        ## Check if an inverse already exists in the cache.  If it does then return the stored inverse.
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        ## The cache does not contain an inverse so determine the inverse, store it in the cache and then return the inverse.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
