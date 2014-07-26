## Utility fuctions to optimize the process of
## calculate and read the inverse of a matrix

## This function create a wrapper matrix that
## provide useful function to set and get a matrix object 
## and set and get its inverse. This is the core function
## behind the implementation of cacheSolve function.
## The function returns a list of the applicable functions 
## on the specified matrix.
## A simple usage :
## c <- rbind(c(1, -1/4), c(-1/4, 1))
## cached_matrix <- makeCacheMatrix(c)
## cached_c <- cached_matrix$get()
## cached_matrix$setInv(solve(cached_c))
## cached_inv_c <- cached_matrix$getInv()

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL
    set <- function(y) {
        x <<- y
        inv_m <<- NULL
    }
    get <- function() x
    setInv <- function(inv_x) inv_m <<- inv_x
    getInv <- function() inv_m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function evaluate the inverse of the matrix x 
## built with makeCacheMatrix, skipping the recalculation 
## if x has a precalculated inverse matrix.
## As precondtion we assume that the matrix 
## supplied is always invertible
## A simple usage :
## c <- rbind(c(1, -1/4), c(-1/4, 1))
## cached_matrix <- makeCacheMatrix(c)
## cacheSolve(cached_matrix)

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("reading cached data")
        return(m)
    }
    data <- x$get()
    ## we specify both a and b args on solve function
    ## to permit correct parameters forwarding
    m <- solve(data, diag(nrow(data)), ...)
    x$setInv(m)
    m
}
