## Functions were created in similar way to the assignment example. First one (makeCacheMatrix) creates a matrix-kind object able to cache
## it's inverse. Second (cacheSolve) returns the inverse by computing or reading it from cache.

## makeCacheMatrix returns a list of functions similar to the assignment example:
## - set: set matrix
## - get: get matrix
## - set.inverted: set inverse
## - get.inverted: get inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set.inverted <- function(inverted) inv <<- inverted
    get.inverted <- function() inv
    list(set = set, get = get,
         set.inverted = set.inverted,
         get.inverted = get.inverted)
}


## cacheSolve returns the inverse by reading it from cache (if exists) or by computing.
## If the inverse comes from cache, comment will be printed.

cacheSolve <- function(x, ...) {
    inv <- x$get.inverted()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set.inverted(inv)
    inv
}
