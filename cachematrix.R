## Caching the inverse of a matrix to avoid the overhead of recomputing it for
## multiple use

## This function creates an object with functions to set (and store) a matrix
## as well as the inverse of the matrix. The input matrix x is considered to be 
## invertible, so no tests towards that are done.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function takes a makeCacheMatrix as input and returns the inverse of the 
## matrix. In the case the inverse was already computed it returns the cached
## inverse, otherwise it computes and caches the inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        mat <- x$get()
        x$setInverse(solve(mat))
        x$getInverse()
}
