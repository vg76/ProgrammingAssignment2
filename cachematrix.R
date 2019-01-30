## Functions to cache inverse of matrix and calcuate it if not already cached

## Creates matrix object that can cache inverse of the matrix

makeCacheMatrix <- function(mtx = matrix()) {
        inverse <- NULL
        set <- function(x) {
                mtx <<- x
                inverse <<- NULL
        }
        get <- function() return(mtx)
        setsolve <- function(inv) inverse <<- inv
        getsolve <- function() return(inverse)
        return(list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve))     
}


## Calculate inverse of matrix if not currentl cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getsolve()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setsolve(inverse)
        inverse
}
