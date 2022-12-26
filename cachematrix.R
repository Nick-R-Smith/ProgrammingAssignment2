## Writing 2 functions that will first create a matrix object that
## can cache its invers, then will compute

## Create matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- inverse
        getinverse <- function () m
        list(set = set, get = get, setinverse = setinverse
        getinverse = getinverse)
}


## Compute inverse of matrix and retreive value from cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m<- solve(data,...)
        x$setinverse(m)
        m
}
