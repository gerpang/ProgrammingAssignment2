## Assume matrix supplied is always invertible

## Makes matrix x and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solveMatrix) inv <<- solveMatrix
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Compute inverse of matrix returned by makeCacheMatrix()
## Else, retrieve inverse if already calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("retrieving from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
