## This set of functions ilustrate how R can cache values taking adventage of the scoping capabilities 
## 

## This function creates a list of creates a special "vector", which is really a list containing a function to
##      set the value of the Matrix
##      get the value of the Matrix
##      set the value of the inverse matrix
##      get the value of the inverse Matrix 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve) 
}


## The following function calculates the mean of the special "Matrix" created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the
## inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}