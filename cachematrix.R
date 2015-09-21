## These functions create a special "matrix" object and 
## find (and cache) it's inverse.
## Usage:
##      A <- matrix(c(2,0,0,4),nrow=2,ncol=2)
##      my_x <- makeCacheMatrix(A)
##      cacheSolve(my_x)
## (Note: subsequent calls just recall the stored inverse from cache)

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    CMinv <- NULL
    set <- function(mx) {
        x <<- mx
        CMinv <<- NULL
    }
    get <- function() x
    setinv <- function(mtxinv) CMinv <<- mtxinv
    getinv <- function() CMinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## computes the inverse of the special "matrix" returned by `makeCacheMatrix`
## If the inverse has already been calculated (and the matrix has not changed), 
## then `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mxinv <- x$getinv()
    if(!is.null(mxinv)) {
        message("getting cached data")
    } else {
    mx <- x$get()
    mxinv <- solve(mx, ...)
    x$setinv(mxinv)
    }
    mxinv
}
