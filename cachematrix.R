## Function to set a matrix
## get function to retrieve matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}

## Compute the inverse matrix created by makeCacheMatrix
## If the inverse has already been calculated then retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix inverse of x
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("fetch cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
