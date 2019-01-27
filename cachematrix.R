## Put comments here that give an overall description of what your
## functions do

#The function makeCacheMatrix creates an environment that contains the functions inverse and x within the set, get, setsolve, getsolve environment.
#The function cacheSolve creates a function that first checks if there is already an inverse in the makeCacheMatrix getsolve function
# if not, it computes the setsolve function.

## Write a short comment describing this function

#The function makeCacheMatrix creates an environment that contains the functions inverse and x within the set, get, setsolve, getsolve environment.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setsolve <- function(inverse) inverse <<- solve
        getsolve <- function() inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
#The cacheSolve function first checks if there is already an inverse available, if not, it computes the inverse within the get, set, getsolve, setsolve functions.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getsolve()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
}
