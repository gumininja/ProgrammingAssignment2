## Calculating the inverse of matrixes can be time consuming thus
## caching the inverses can be favorable when it is calculated multiple times
## These functions help in caching the inverse matrix without polluting the
## environment with extra variables

## Create a cache for a single matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Returns the inverse matrix using the cache

cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
