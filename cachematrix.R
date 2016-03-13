## Take the inverse of a square (n x n) matrix, first by creating
## a cached version of the matrix and then passing to a function
## to carry out the inversion.
## To run code and take the inverse, enter
## cacheSolve(makeCacheMatrix(MATRIX))
## where MATRIX is your matrix data.
##
## Note: provides same result as solve(MATRIX)
##


## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinverse()
    if(!is.null(inver)) {
        message("getting data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setinverse(inver)
    inver
}

## End program