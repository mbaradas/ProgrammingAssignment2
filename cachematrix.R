## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## Use the <<- operator which can be used to assign a value to an object in an 
## environment that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is a square invertible matrix
        ## Return a list with these objects
        ## 1. set the matrix object
        ## 2. get the matrix object
        ## 3. set the inverse object
        ## 4. get the inverse object
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        set_inv = function(inverse) inv <<- inverse
        get_inv = function() inv
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## 'x' is the output of makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$get_inv()
        
        ## Check If the inverse has already been calculated (and the matrix has
        ## not changed), then the cachesolve should retrieve the inverse 
        ## from the cache.
        
        if(!is.null(inv)) {
                return(inv)
        }
        
        ## else, calculate the inverse
        mat_data <- x$get()
        inv <- solve(mat_data, ...)
        
        ## set the value of the inverse in the cache with set_inv 
        x$set_inv(inv)
        
        inv
}



