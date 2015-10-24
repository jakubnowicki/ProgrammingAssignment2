## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
