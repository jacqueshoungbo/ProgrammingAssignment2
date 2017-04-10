## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    theinverse <- NULL
    set <- function (y){
        x <<- y
        theinverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) theinverse <<- solve
    getinverse <- function() theinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated
## (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    theinverse <- x$getinverse()
    if(!is.null(theinverse)) {
        message("getting cached data")
        return(theinverse)
    }
    data <- x$get()
    theinverse <- solve(data, ...)
    x$setinverse(theinverse)
    theinverse
}
