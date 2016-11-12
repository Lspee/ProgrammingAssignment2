
## This function accepts a matrix and creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL }
        get <- function() x
        set_inv <- function(inv) inv_mat <<- inv
        get_inv <- function() inv_mat
        list(set = set, get = get,
                set_inv = set_inv,
                get_inv = get_inv)
}

## This function simply recalculates OR retrieves the inverse of the 
## special "matrix" returned by `makeCacheMatrix` above. 

cacheSolve <- function(x, ...) {
        inv_mat <- x$get_inv()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data)
        x$set_inv(inv_mat)
        inv_mat
}
