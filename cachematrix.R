## Utility functions to handle cached computation of matrices and
## their inverse. There are two functions:
## makeChacheMatrix: creates or take the original matrix and builds
##    a functional closure to cache there data
##
## cacheResolve: performes the solve operation within the function created
##    with makeCacheMatrix, additional parameters
##    for solve can be passed using the ... argument. The function
##    first tries to retriev the inverse from the closure built with
##    the makeCacheMatrix function. If not possible it performs the
##    solve operation and sets the inverse at the closure
## 

## This function builds a functional closure around the original
## matrix and its inverse, hence it's possible to chache these data.
##
## It returns a list containing the following
## elements:
## 1. set <- setter to set the original matrix
## 2. get <- getter to retriev the original matrix
## 3. set_inverse <- setter to the set the inversed matrix
## 4. get_inverse <- getter to retriev the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        # Define setter for original matrix
        set <- function(new_matrix) {
                x <<- new_matrix
                insverse <<- NULL
        }
        
        #define getter for original matrix
        get <- function() {
                return (x)
        }
        
        # define setter for inverse of matrix
        set_inverse <- function(new_inverse) {
                inverse <<- new_inverse
        }
        
        # define getter for inverse of matrix
        get_inverse <- function() {
                return (inverse)
        }
        
        # return a list of setter/getter (original matrix)
        # and setter/getter of inverse matrix
        return (list( 
                         set = set
                        ,get = get
                        ,set_inverse = set_inverse
                        ,get_inverse = get_inverse
                ))
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated, 
## then the cachesolve returns the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        # try to get inversed matrix from parameter x...
        i <- x$get_inverse()
        if(!is.null(i)) {
                message("getting cached data")
                return (i)
        }
        
        # ...no cache hit... compute inverse matrix and
        # use the set method of inversed data to make it
        # available for the next calls
        original_matrix <- x$get()
        i <- solve(original_matrix, ...)
        x$set_inverse(i)
        return (i)
}

