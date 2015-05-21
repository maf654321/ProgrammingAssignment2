## Using caching in the global environment, the recomputation of time-consuming results can be avoided.
## These two functions combine to make a cache-able matrix, and, if that matrix is invertible, caching that inverse

## makeCacheMatrix creates the cache-able matrix by a specialized list
##      x = invertible matrix
## This list will be used as input to cacheSolve


makeCacheMatrix <- function(x = matrix()) {
        # Sets 'result' to NULL as default state
        result <- NULL
        
        # Set value of matrix
        set <- function(y) {
                # use <<- to outupt to Global Env
                x <<- y
                
                # Sets 'result' to NULL in Global Env
                result <<- NULL
        }
        
        # Get value of matrix
        get <- function() x
        
        # Set value of the inverse of matrix
        setsolve <- function(solve) result <<- solve
        
        # Get value of the inverse of matrix
        getsolve <- function() result
        
        #Creating specialized list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve uses output of makeCacheMatrix to pull solved result from cache or solve a cache new result 
##      x = specialized list with cache-able Matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        result <- x$getsolve()
        
        # If inverse of 'x' has already been cached
        # Checks if 'result' is NOT default NULL state 
        if(!is.null(result)) {
                message("getting cached data")
                
                # Returning 'result'
                return(result)
        }
        
        # If inverse of 'x' has not been cached
        dat <- x$get()
        result <- solve(dat, ...)
        
        # Caching and returning 'result'
        x$setsolve(result)
        return(result)
}
