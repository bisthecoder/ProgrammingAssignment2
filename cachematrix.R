##  pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## below function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invmt <- NULL
        set <- function(y) {
                x <<- y
                invmt <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invmt <<- inverse
        getInverse <- function() invmt
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## below function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmt <- x$getInverse()
        if (!is.null(invmt)) {
                message(" pull cached data")
                return(invmt)
        }
        mat1 <- x$get()
        invmt <- solve(mat1, ...)
        x$setInverse(invmt)
        invmt
}
