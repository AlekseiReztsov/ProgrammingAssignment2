## Functions below make cashe of potentially costly operations

## Create wrapper for matrix which cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL    
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Calculate inverse for matrix created with makeCacheMatrix if the inverse already has not calculated and the matrix has not changed. 
## Otherwise it returns cached value

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv))
    {
        print("Getting cached data")
        return(inv)
    }
    
    matr <- x$get()
    inv <- solve(matr)
    x$setInverse(inv)
    
    return(inv)
}

