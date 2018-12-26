## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## There a pair of functions that cache the inverse of a matrix.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        
        get <- function() x
        
        set_inverse <- function(inverse_mat) {
                inverse_matrix <<- inverse_mat
        }
        
        get_inverse <- function() inverse_matrix
        
        list(set = set,
             get = get, 
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        
}


##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then the cachesolve should retrieve the
##  inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse_matrix <- x$get_inverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        
        data <- x$get()
        
        inverse_matrix <- solve(data, ...)
        x$set_inverse(inverse_matrix)
        
        inverse_matrix
}
