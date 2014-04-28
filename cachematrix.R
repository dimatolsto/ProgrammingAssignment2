## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.
## The first function creates a special "matrix" object that can cache its inverse.
## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

## It is assumed that the input matrix is always invertible.

## The makeCacheMatrix function creates a list containing a functions to
## set the value of the matrix to a cache
## get the value of the matrix from the cache
## set the value of the inverse matrix to the cache
## get the value of the inverse matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                     # First nitialize the inverse matrix with NULL
    set <- function(y) {                            # Set a cache: ...
        x <<- y                                     # ... matrix with the given value, ...
        inv <<- NULL                                # ... its inverse with NULL
    }
    get <- function() x                             # Return the matrix
    setInverse <- function(inverse) inv <<- inverse # Set the cache with the computed inverse matrix 
    getInverse <- function() inv                    # Return the inverse matrix from the cache
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the matrix created with makeCacheMatrix function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix and sets the matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()                  #query the x matrix's cache 
    if(!is.null(m)) {                    #if there is a cache 
        message("getting cached data")
        return(m)                        #return the cache, no computation
    }
    data <- x$get()                      #if there's no cache
    m <- solve(data)                     #compute the inverse matrix
    x$setInverse(m)                      #save the result to the cache
    m                                    #return the result
}
