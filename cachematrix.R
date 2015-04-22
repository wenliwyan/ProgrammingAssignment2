## R functions to cache the potentially time-consuming computations to get
## the inverse of an invertible matrix, thus saving time when the value is
## needed again.

## makeCacheMatrix creates a special "matrix", as a list of functions to
##    1. set the matrix value
##    2. get the matrix value
##    3. set the matrix inverse
##    4. get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
	inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned above.
## If the inverse has been calculated, it just retrieves the value from cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
	return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
