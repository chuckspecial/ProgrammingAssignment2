## create a matrix with cache-able inverse

## creates "matrix" list object with matrix get/set functions and cache-able inverse get/set funcs

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## caches or retrieves from cache inverse of matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        return(inv)
    }
    else{
        mat <- x$get()
        inv <- solve(mat,...)
        x$setinv(inv)
        return(inv)
    }
}
