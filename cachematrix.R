## These functions provide a way for storing a matrix, as well as the inverse
## of the matrix. When the inverse of the matrix is first calculated, the
## result will be stored, and will be looked up later if the answer is needed
## again. This saves on processing time.
## USAGE: create a matrix, and pass it to makeCacheMatrix(). Store the result
## in a variable, and pass that variable to cacheSolve(). The first time that
## this happens, the inverse of the matrix will be calculated. Subsequent calls
## will result in a cache lookup, and the user will be notified that the data
## is the cached data.

## makeCacheMatrix() takes a matrix as an argument, and will return the
## functions set(), get(), setInverse(), and getInverse() for later use.

makeCacheMatrix <- function(x = matrix()) {
    # the inverse
    inv <- NULL
    # set() sets the instance data (the matrix), and initializes the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get() returns the matrix
    get <- function() x
    ## setInverse() assigns the inverse
    setInverse <- function(inverse) inv <<- inverse
    ## getInverse() returns the inverse
    getInverse <- function() inv
    ## return the functions detailed above
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() takes the result of makeCacheMatrix, and uses it to solve
## for the inverse of the matrix in makeCacheMatrix(). If the inverse has
## already been calculated, it will return the cached data. If not, it will
## calculate the inverse, cache the result, and return the inverse.

cacheSolve <- function(x, ...) {
    ## get the existing inverse data
    inv <- x$getInverse()
    ## if the existing data is not null, return the existing data
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## otherwise, assign the matrix to data
    data <- x$get()
    ## solve for the inverse of the matrix
    inv <- solve(data, ...)
    ## set the cached inverse to the calculated inverse
    x$setInverse(inv)
    ## return the calculated inverse
    inv
}
