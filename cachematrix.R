## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    x_inv <- matrix(data = NA, nrow = ncol(x), ncol = nrow(x))
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) x_inv <<- inverse
    getinv <- function() x_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I = diag(dim(x$get())[1])
    x_inv <- x$getinv()
    if(!all(is.na(x_inv))) {
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data, I)
    x$setinv(x_inv)
    x_inv
}
