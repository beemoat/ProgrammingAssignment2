## Functions to create a special object representing a matrix that caches the
## inverse and to obtain the inverse of the matrix.

## This function creates the cachematrix object, which is a list of functions to
## set the value of the matrix, get the value of the matrix, set the value of
## the inverse and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() {
        x
    }
    setInv <- function(inv) {
        xInv <<- inv
    }
    getInv <- function() {
        xInv
    }
    list(
        set = set,
        get = get,
        setInv = setInv,
        getInv = getInv
    )
}


## This function produces the inverse matrix of a cacheMatrix object. If the 
## inverse is cached, that is returned. Otherwise, the inverse is computed and 
## put into the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setInv(inv)
    inv
}

