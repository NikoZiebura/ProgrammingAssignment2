## Put comments here that give an overall description of what your
## functions do

## we have 2 functions here
## makeCacheMatrix is a functions which holds a matrix and can cache the inverse
## cacheSolve returns the inverse of a cachedMatrix by doing:
##   check if the inverse is already cached
##   if yes, return cached value
##   if not, compute it and store it for future access


## assume you have a marix x which is square invertible
## then you can make it to a CacheMatrix y by calling
## y <- makeCacheMatrix(x)
## you can now...
##     get the matrix via y$get()
##     change the matrix via y$set( a new matrix )
##     set the cached inverse via setInv( inverse matrix )
##     get the cached invese matrix via getInv()

makeCacheMatrix <- function(x = matrix()) {
            invX <- NULL
            set <- function(y) {
                    x <<- y
                    invX <<- NULL
            }
            get <- function() x
            setInv <- function(inv) invX <<- inv
            getInv <- function() invX
            list(set = set, get = get,
                 setInv = setInv,
                 getInv = getInv)
}


## Write a short comment describing this function
## should return the inverse matrix
## parameter is a cachedMatrix which is created by makeCacheMatrix
## if inverse is already computed, then return it
## if not, then compute it, store it in cache and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            invx <- x$getInv()
            if(!is.null(invx)) {
                    message("getting cached data")
                    return(invx)
            }
            data <- x$get()
            invx <- solve(data)
            x$setInv(invx)
            invx
}

