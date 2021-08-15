## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # declare a null vector 
        # creates a special “matrix” object that can cache its inverse
        d <- NULL
        set <- function(y) {
                x <<- y
                d <<- NULL
        }
        # get and store x and inv in the enclosing environment of
        # the set, get, setInverse, getInverse functions. 
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
        # computes the inverse of the special “matrix” returned by makeCacheMatrix above
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
