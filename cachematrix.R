## These functions allow calculate the inverse of a matrix caching the result

## This function is used to create the required structure to store the original
## matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
        getinverse = getinverse)
}


## These function is used to calculate el inverse of a matrix using a list
## created with the function makeCacheMatrix

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
