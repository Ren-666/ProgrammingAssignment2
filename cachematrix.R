## I am attempting to create an "matrix" R object that
## can then be used by a second function, via the first
## objects internal methods to get the matrix inverse by 
## processing or retrieving a cached answer if it already
## exists.

## This function creates the R object containing the 
## initial x matrix and the methods to calculate and cache
## its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function uses the methods of the makeCacheMatrix
## R object to calculate and return the inverse matrix of 
## the matrix embedded in that object instance, or to return
## a previously calculated inverse matrix without 
## recalculating it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)  
        x$setinv(m)             
        m
}
