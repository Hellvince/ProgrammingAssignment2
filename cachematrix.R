################################################################################
## Title :      ProgrammingAssignment2
## Date  :      20/08/2014
##
## Create two functions :
##      - makeCacheMatrix: this function creates a "special matrix" object that 
##      can cache its inverse. The matrix itself can be fetched using get() or 
##      updated using set(), the inverse can be fetched using getsolve().
##
##      - cacheSolve: this function computes the inverse of a "special "matrix"
##      returned by makeCacheMatrix above. If the inverse has already been 
##      calculated (and the matrix has not changed), then cacheSolve will 
##      retrieve the inverse from the cache.
##
################################################################################


# Create a "special matrix" able to cache its inverse
makeCacheMatrix <- function(x = matrix()){
        ## Return a list object made of the attached functions
        ## The matrix content is accessible throught get() and set() only
        
        # Stored inverse value, NULL by default
        inv <- NULL
        
        # Print the matrix content
        get <- function(){
                x
        }
        
        # Update the matrix content and reset the cached inverse value
        set <- function(y){
                x <<- y
                inv <<- NULL        
        }
        
        # Print the cached inverse value 
        getsolve <- function(){
                inv
        }
        
        # Push the inverse value to cache it
        # To be called through cachesolve() ONLY
        setsolve <- function(inverse){
                inv <<- inverse
        }
        
        # Output the "special matrix" objet
        list(get = get,
             set = set,
             getsolve = getsolve, 
             setsolve = setsolve)
}



## Calculate or fetch the cached inverse of a "special matrix" object
cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        ## Assume 'x' is a "special matrix" object with an invertible matrix
        
        # Checking the cached value of 'inv'
        # If NULL, inverse was not cached : calculate it 
        if (is.null(x$getsolve())){
                
                message("Computing the inverse for the first time")
                
                # Fetch the matrix and calculate the inverse
                inverse <- solve(x$get(), ...)
                
                # Push it to the 'inv' value
                x$setsolve(inverse) 
                
                # Print the inverse
                inverse
                
        # If not NULL, inverse was cached : fetch it        
        } else {
                
                message("Getting cached data")
                
                # Fetch the cached 'inv' value from the special matrix
                x$getsolve()
        }
}