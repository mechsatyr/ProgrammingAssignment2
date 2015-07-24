## makeCacheMatrix creates a special Matrix object that 
## cacheSolve uses to compute its inverse.
## If the inverse has already been calculated,
## then cacheSolve simply returns the inverse
## from the cache.

## makeCacheMatrix creates a special Matrix
## containing a function to:
## 1) set the value of the Matrix
## 2) get the value of the Matrix
## 3) set the value of the Inverse
## 4) get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverse_x <<- inverse
    getInverse <- function() inverse_x
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve calculate the Inverse of the special
## "Matrix" created with makeCacheMatrix.
## However, it firs checks to see if the Inverse
## has already been calculated. If so,
## it skips the computation and gets the Inverse
## from the cache.If not, it computes the inverse
## and sets the value in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("getting cahed dat")
        return(inverse_x)
    }
    data <- x$get()
    inverse_x <- inverse(data, ...)
    x$setInverse(inverse_x)
    inverse_x
}
