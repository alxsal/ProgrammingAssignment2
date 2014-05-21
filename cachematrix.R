## Set of 2 functions to cache the inverse of a matrix
## The goal is to avoid computation on every call and 
## store the value on first computation, and reuse it 
## at later call

## makeCacheMatrix creates the functions to store the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
            xinv <- NULL
            set <- function(y) {
                    x <<- y
                    xinv <<- NULL
            }
            get <- function() x
            setinv <- function(inverse) xinv <<- inverse
            getinv <- function() xinv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## cacheSolve computes the inverse of the x matrix when first called
## but retrieves it on later calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            xinv <- x$getinv()
            if(!is.null(xinv)) {
                    message("getting cached data")
                    return(xinv)
            }
            data <- x$get()
            xinv <- solve(data)
            x$setinv(xinv)
            xinv
}
