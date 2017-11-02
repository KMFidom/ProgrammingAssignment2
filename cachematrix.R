## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. These functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        cachematrix <- NULL
        set <- function(y) {
                x <<- y
                cachematrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachematrix <<- inverse
        getinverse <- function() cachematrix
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then the
## `cachesolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        cachematrix <- x$getinverse()
        if(!is.null(cachematrix)) {
                message("getting cached data")
                return(cachematrix)
        }
        data <- x$get()
        cachematrix <- solve(data, ...)
        x$setinverse(cachematrix)
        return(cachematrix)
}