makeCacheMatrix <- function(x = matrix) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve)  inv <<- solve
    getinv <- function()  inv
    list(setinv = setinv, getinv = getinv, get = get)
}


cacheSolve <- function(x, ...){
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(x$get())
    x$setinv(inv)
    inv
}

#m <- matrix(1:4, 2,2)
#x <- makeCacheMatrix(m)
#cacheSolve(x)