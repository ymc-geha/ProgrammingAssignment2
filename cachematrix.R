## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse.

## This function creates a special "matrix", which is really a list of functions
## to get/set the matrix and to get/set the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function()
        x
    
    setInverse <- function(inverse)
        i <<- inverse
    
    getInverse <- function()
        i
    
    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## This function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the
# data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
  
    if (!is.null(i)) {
        message("Getting cached data.")
        return(i)
    }
    
    m <- x$get()
    i <- solve(m, ...)
    x$setInverse(i)
    
    i
}
