## An R function to cache potentially time-consuming computations for the inverse of a matrix (Assignment)

## Return a list of four elements, set the matrix, get the matrix, set the inverse matrix, and 
## get the inverse matrix. If a new matrix is the input, the inverse will be NULL

makeCacheMatrix <- function(x = matrix(), ...) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinvers <- function(invers) inv <<- invers
    getinvers <- function() inv
    list(set = set, get = get,
         setinvers = setinvers,
         getinvers = getinvers)
}


## Return the inverse of the matirx. Based on the function of makeCacheMatrix(), if the inv is not NULL,
## it will return the inverse matrix from the list and skip the calculation

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinvers()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinvers(inv)
    inv
}
