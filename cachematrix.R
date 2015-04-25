## Put comments here that give an overall description of what your
## functions do 
# Using these 2 functions will allow for faster retieval of a previously 
# solved inverse matrix


## Write a short comment describing this function
# sets the value of the matrix and inverse of the matrix
# allows for  retrival of maxtrix and cached inverse of the matrix

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
# Checks if the inverse of the maxtrix has already been solved / cached
# if so inverse is returned
# if not inverse is calculated, cached, and returned

cacheSolve <- function(x, ...) {
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
