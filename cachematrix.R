## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    #Build NULL matrix based on input matrix
    x_inv <- matrix(data = NA, nrow = ncol(x), ncol = nrow(x))
    #Set function
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    #function to get values
    get <- function() x
    
    #Inverse assigning function
    setinv <- function(inverse) x_inv <<- inverse
    
    #Inverse retrieving function
    getinv <- function() x_inv
    
    #Object containing all functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #Create Identity matrix based on dimension of the input matrix
    I = diag(dim(x$get())[1])
    
    #Assign inverse
    x_inv <- x$getinv()
    
    #Check if inverse exists/already calculated and return inverse if available
    if(!all(is.na(x_inv))) {
        message("getting cached data")
        return(x_inv)
    }
    
    #If inverse not present,
    #Retrieve data to compute inverse
    data <- x$get()
    
    # Data * Inverse(Data) = I; Use solve function to calculate inverse
    x_inv <- solve(data, I)
    x$setinv(x_inv)
    
    #return inverse to prompt
    x_inv
}
