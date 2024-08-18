## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # store inverse value
    inverse <- NULL
    # set the original matrix and reset inverse
    set <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    # get the original matrix
    get <- function() matrix
    # set inverse value
    set_inverse <- function(inv) inverse <<- inv
    # get inverse value
    get_inverse <- function() inverse
    # Returns a list of the 4 functions, this list is the special "matrix"
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- special_matrix$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- special_matrix$get()
    inverse <- solve(data, ...)
    special_matrix$set_inverse(inverse)
    inverse
}
