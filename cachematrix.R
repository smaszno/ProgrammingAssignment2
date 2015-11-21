## makeCacheMatrix is a ripoff of makeVector example, i.e. it is a bunch of function to set/get value of matrix/inverted matrix 
## cacheSolve is a ripoff of cachemean, it returns a matrix that is inverse to x from cache (if it is there) or it calculates this inverse, stores it in cache and returns 

## makeCacheMatrix - allows operations on cache of matrices
## input: x - matrix
## output: list of 4 functions (set - to store a matrix in cache, get -  to get matrix from cache, setinvrs - to store inverse matrix in cache, getinvrs - to get inverse matrix from cache)

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinvrs <- function(inverted) invrs <<- inverted
        getinvrs <- function() invrs
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}


## cacheSolve - inverts a matrix
## input: matrix to be solved (inverted), plus additional parameters for solve function
## output: inverted matrix, taken from cache or calculated, stored in cache, and returned


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getinvrs()
        if(!is.null(invrs)) {
                message("getting cached inverse matrix")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinvrs(invrs)
        invrs

}