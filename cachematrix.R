## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        d <- NULL
        set <- function(y) {
                x <<- y
                d <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) d <<- inverse
        getinverse <- function() d
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        d <- x$getinverse()
        if (!is.null(d)) {
                message("getting cached data")
                return(d)
        }
        data <- x$get()
        d <- solve(data, ...)
        x$setinverse(d)
        d
}
