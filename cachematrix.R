## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse is already computed and
## the matrix hasn't changed it returns the result from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Returning the cached inverse")
        return(inv)
    }
    data <- x$get()
    nrows <- nrow(data)
    ncols <- ncol(data)
    ##Here we are doing a basic check for square matrix. Detailed checking
    ## whether the matrix is inversible is out of scope for this program
    ## and we let solve function handle it. 
    if(nrows == ncols)
    {
        inv <- solve(data, ...)
        x$setinverse(inv)
    }
    else
    {
        message("Non square matrix encountered")
    }
    inv
}
