## makeCacheMatrix takes a matrix as input and creates a list with four functions.
## These four functions 
##     set: a function used to place matrix data into parent environment.
##     get: just gets the input matrix
##     setinverse:  puts the inverse matrix into the parent environment for 'permanent' storage
##     getinverse: retrieves the inverse matrix stored in parent environment
##     

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    cachedMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        cachedMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cachedMatrix <<- inverse
    getinverse <- function() cachedMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks to see if the inverse matrix is already cached.  If it is, it simply returns
##  the inverse.  If it's not cached, it computes it and stores it in the cache.. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setmean(inv)
    inv
}
