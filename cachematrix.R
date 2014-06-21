## Peer Assessment / Programming Assignment 2: Lexical Scoping
## Moises C. M.

## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly. Here we have two function to achive
## this: one creates a special matrix that can hold its inverse, and a second one that calculates
## the inverse of the special matrix and caches it using the first one (if the special matrix
## has already the inverse cached, this function just return it.)
##
## Example:
##
## > m1 <- matrix(rnorm(25), 5, 5)
## > em <- makeCacheMatrix(m1)
## > emi <- cacheSolve(em)
## > em$getinverse()

## makeCacheMatrix() creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL               # private var. to hold the inverse matrix
    set <- function(y) {    
        x <<- y             # set (save) the original matrix to x (x is a private var.)
        m <<- NULL          # initializa the inverse matrix to NULL
    }
    get <- function() x     # return the original matrix 
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() computes the inverse of the special 'matrix' returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve()
##  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Assumption: x is an inversible matrix (square not singular)
    
    m <- x$getinverse()
    if (!is.null(m)) {
        message("Getting cached inversed matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
