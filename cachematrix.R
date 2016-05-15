# Computing  and caching inverse of a matrix. 

# Creates list of functions which include methods to set and get values of matrix, 
# and set and get cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinv <- function(solve) {inv <<- solve}
    getinv <- function() {inv}
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


# Returns inverse of a matrix from cache. 
# If cache is empty calculates and returns inverse, and updates cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    matx <- x$get()
    inv <- solve(matx, ...)
    x$setinv(inv)
    inv
}


