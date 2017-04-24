## This source file consists of two functions that implements a 
## inversed cache for a given matrix

## The first function,`makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix 
## 4.  get the value of the inverse matrix
##
## Argument x - a given matrix provided by the caller of the
##              function; default value is 1x1 null matrix
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() (x)
        setinv <- function(new_inv) inv <<- new_inv
        getinv <- function() (inv)
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The second function calculates the inverse of a special matrix
## created with the above function. It first checks to see 
## if the inversed matrix has already been calculated. If so, it 
## gets the inversed matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets 
## the value of the inversed matrix in the cache via the `setinv`
## function.
##
## Argument x - given special matrix vector created by makeCacheMatrix
##

cacheSolve <- function(x, ...) {
        ## get the cached inversed
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached inverse matrix")
                return(inverse)
        }
        ## fall through due to a null cache 
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
