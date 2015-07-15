## The functions makeCacheMatrix() and cacheSolve() are used to create an object
## that stores a matrix and chache's it's inverse.

## makeCacheMatrix(x = matrix()) is a function that creates a special "vector" 
## of functions storing, retreiving, and caching a matrix and its inverse.
## Input paramater: A square, invertible matrix.
## Returns: A list of functions for operating on the matrix.
makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y){
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) x_inv <<- inverse
    getinv <- function() x_inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## cacheSolve(x,...) is a function that checks to see if the matrix inverse has
## already been computed. If it hasn't, the inverse is computed and then cached;
## if it has, the inverse is retrieved from cache.
## Input parameter: List of functions returned by makeCacheMatrix()
## Returns: The matrix inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinv()
    if(!is.null(x_inv)) {
        message("Getting cached matrix inverse.")
        return(x_inv)
    }
    X <- x$get()
    x_inv <- solve(X)
    x$setinv(x_inv)
    x_inv
}
