## Put comments here that give an overall description of what your
## functions do
## The functions create a list which contains other functions to store and
## retrieve a calculated result of an inverse of a matrix in a cache.
## In case the result of a specific matrix was not cached before, the inverse
## of this matrix will be calculated

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its reverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
    setsolve = setsolve,
    getsolve = getsolve)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix or retrieves
## is from the cache if already available in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
