## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix: Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Check whether the arg is a matrix
        if(!is.matrix(x)) {
                message("The argument must be a matrix!")
                return(invisible(x))
        }
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: Computes the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then this function should retrieve the inverse from the
## cache. Check whether the matrix is invertible or not by using determinant.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## Check whether the matrix is invertible
        if(det(data) == 0) {
                message("The matrix is not invertible!")
                return(invisible(data))
        }
        inv <- solve(data, ...)
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}