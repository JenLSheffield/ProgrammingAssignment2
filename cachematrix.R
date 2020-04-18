## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Takes invertible matrix x, sets m to null.
# returns list of functions to get/set matrix x and inverse of matrix m in parent environment

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# Takes makeCacheMatrix object x
# Checks if x has cached inverse
# If not, retrieves matrix using x$get
# Calculates inverse of matrix, m, using solve()
# returns m to parent environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
