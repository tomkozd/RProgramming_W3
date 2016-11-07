## The following two functions are designed to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

## Function makeCacheMatrix creates and returns a list of functions
## used by next function cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
    # inv stores the cached value
    inv <- NULL
    # matrix creation in the working environment
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    # gets the value of the matrix
    get <- function() x
    # inverts and caches the matrix
    setinvmatrix <- function(solve) inv <<- solve
    # gets the matrix from cache
    getinvmatrix <- function() inv
    # returns the function to the working environment
    list(set = set, get = get, setinvmatrix = setinvmatrix, 
         getinvmatrix = getinvmatrix)
}


## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
    # gets inverted matrix from cache
    inv <- x$getinvmatrix()
    # returns inverted matrix from cache if exists
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # creates matrix if does not exist
    matrix <- x$get()
    # inverts the matrix
    inv <- solve(matrix, ...)
    # sets inverted matrix in cache
    x$setinvmatrix(inv)
    # prints the inverted matrix
    inv
}

# Example:

# a <- diag(5,5)
# a
# CachedMarix <- makeCacheMatrix(a)
# cacheSolve(CachedMarix)