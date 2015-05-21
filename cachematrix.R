## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        result <- NULL
        set <- function(y) {
                x <<- y
                result <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) result <<- solve
        getsolve <- function() result
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        result <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(result)
        }
        dat <- x$get()
        result <- solve(dat, ...)
        x$setsolve(result)
        return(result)
}
