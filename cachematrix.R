## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. So there are two functions serve that purpose (i.e. cache the inverse of a matrix)
## Usage:
##     x <- matrix(...)
##     specialMatrix <- makeCacheMatrix(x)
##     invertedMatrix <- cacheSolve(specialMatrix)


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mi <<- inverse
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
## Assumes that the matrix supplied is invertible.
## Computing the inverse of a square matrix is done by the solve() function.
##
## Parameters:
##     x -- special matrix created by makeCacheMatrix() method
##     ... -- any other parameters for solve() method

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cached <- x$getinverse()
    if(!is.null(cached)) {
        message("return cached value")
        return cached
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}

