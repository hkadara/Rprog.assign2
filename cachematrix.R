## Matrix inversion is computationally intensive
## It is beneficial to cache the inverse of the matrix

## Functions below are for caching the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Next function generates inverseof matrix assumed to be always invertible
## If matrix is already made, the matrix is obtained from the cache
## Else, it computes the inverse, and sets its value in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("acquiring cached data")
                return(inv)  
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
