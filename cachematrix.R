The following pair of functions cache the inverse of a matrix. I must attribute the
wording here to Prof. Peng from his README.md file forked from GitHub.  Thanks, 
Prof. Peng

`makeCacheMatrix`: This function creates a special "matrix" object that can cache 
its inverse.  Very original, eh?

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

`cacheSolve`: This function computes the inverse of the special "matrix" returned 
by `makeCacheMatrix` above. If the inverse has already been calculated (and the 
                                                                        matrix has not changed), then `cacheSolve` retrieves the inverse from the cache.
Again, thanks, Prof. Peng.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}