## A pair of functions that retrieve the inverse value of a square
## from cache if it has already been calculated. Otherwise, calculate
## the inverse value and save it in cache.

## create a matrix object that cache its inverse
## input x of the function is the matrix to be inversed
makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        
        ## set the matrix x
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        ## get the matrix x
        get <- function() x
        
        ## set the inverse of the matrix
        setinverse <- function(inverse) n <<- inverse
        ## get the inverse of the matrix
        getinverse <- function() n
        
        ## return the list of the set and get functions
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## compute the inverse of matrix x returned by function makeCacheMatrix
## if inverse already calculated, retrieve value from cache
## input of this function is matrix x, and the list of functions returned
## by function makeCacheMatrix
## return the inverse of matrix x
cacheSolve <- function(x, ...) {
        n <- x$getinverse()
        
        ## if inverse already calculated, retrieve value from cache
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        
        ## otherwise calculate the inverse of matrix x
        data <- x$get()
        n <- calculateinverse(data, ...)
        
        ## set inverse of matrix in cache
        x$setinverse(n)
        n
}
