## Creates & stores the inverse of a matrix
## calculates inverse the first time, returns the stored value later

## Assign to a variable to instantiate this special type of matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setCache <- function(solve) m <<- solve
        getCache <- function() m
        list(set = set, get = get, setCache = setCache, getCache = getCache)
}



## Invoke cacheSolve(your_variable) to return the inverse

cacheSolve <- function(x, ...) {
        m <- x$getCache()
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setCache(m)
        m
}
