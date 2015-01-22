## Caching the inverse of a matrix with following two functions: 
##      1. makeCacheMatrix () and 2. CacheSolve ()
## The function of makeCacheMatrix () creates a list of functions to 
##      1. set the value of a matrix (an invertible square Matrix)
##      2. get the value of a matrix
##      3. set the value of an inverse matrix
##      4. get the value of an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) v <<- inv
        getinverse <- function() v
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}        
        
## The function of cacheSolve () is used to compute the inverse of an invertible square matrix   
## or to skip the computation by just getting the inverse from the cache.
cacheSolve <- function(x, ...) {
        v <- x$getinverse()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data)
        x$setinverse(v)
## Return a matrix that is the inverse of 'x'
        v
}
