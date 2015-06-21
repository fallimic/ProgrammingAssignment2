## These functions are used to create matrices and compute their inverses.
## The inverse matrices are cached to reduce costly computation

## makeCacheMatrix takes in a matrix as an argument and returns a 'special matrix' (list object)
## containing functions to get and set the matrix as well as get and set it's inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes in a 'special matrix' (as returned by the makeCacheMatrix function)
## and will return the inverse of that matrix. If the inverse has already been looked up
## once through this function, a cached matrix will be returned and the inverse computation will not 
## be performed again.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
