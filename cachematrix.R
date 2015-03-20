## There are 2 functions within this script.  The overall capabilities
## of this script are to be able to compute the inverse of a matrix
## and then cache the result.  After cacheing the result the cached value
## can be retrieved without recomputing the inverse.

## The makeCacheMatrix function accepts a matrix as input and will either
## cache the object or rectieve the object from its cache.  This result
## depends on which sub functions are called on the object.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setcache <- function(cache) s <<- solve
        getcache <- function() s
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## The cacheSolve function calls the subfunctions of makeCacheMatrix to 
## either get the cached version of the inverted matrix or compute the 
## inverse and then set the cache.  If the object is cached, the message
## 'getting cached data' is displayed, otherwise a message is displayed
## stating that the inverse of the matrix has not been cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getcache()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        message("matrix inverse not cached, computing...")
        s <- solve(data, ...)
        x$setcache(s)
        s
}
