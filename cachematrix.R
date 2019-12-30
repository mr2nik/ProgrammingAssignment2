## Author: Nik Borer
## Date: 30.12.2019

## Put comments here that give an overall description of what your
## functions do
## if the inversion of a Matrix was already done and stored in the cache
## -> use the cache, else calculate the inversion


## Write a short comment describing this function
## this function caches the inversion of a matrix
## -> create the cache for Matrix inversion
makeCacheMatrix <- function(x = matrix()) {
        my_inv <- NULL
        set <- function(my_inverse){
                x <<- my_inverse
                my_inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) my_inv <<- solve
        getInverse <- function() my_inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## this function checks if the Matrix inversion is already done and return the data
## if not -> will call the makeCacheMatrix and create the inversion
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        my_inv <- x$getInverse()
        if(!is.null(x)) {
                message("getting cached inversion")
                return(my_inv)
        }
        data <- x$get()
        my_inv <- solve(data, ...)
        x$setInverse
        my_inv
}
}
