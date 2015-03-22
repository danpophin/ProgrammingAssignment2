## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mat_cache <- NULL       ## initialize the empty cache matrix
        set_matrix <- function(new_value) {     ## initializae if recalled with new input
                x <<- new_value
                mat_cache <<- NULL
        }
        get_matrix <- function() x      ## retrieve the stored matrix
        set_inverse <- function(inverse) mat_cache <<- inverse  ## calculate the inverse of the matrix
        get_inverse <- function() mat_cache
        list(set_matrix = set_matrix, get_matrix = get_matrix, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_cache <- x$get_nverse()
        if(!is.null(mat_cache)) {
                message("retrieving cached data")
                return(mat_cache)
        }
        data <- x$get_matrix()
        mat_cache <- solve(data,...)
        x$set_inverse(mat_cache)
        mat_cache
}
