## makeCacheMatrix creates a special matrix, sets the value of the matrix,
## gets the value of the matrix, sets the inverse of the matrix, and gets the 
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()){
        m  <- NULL
        set  <- function(y) {
                x <<- y
                m <<- NULL
        }
        get  <-  function()x
        setsolve  <-  function(solve) m <<- solve
        getsolve  <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve first checks if the inverse of the matrix created above has been
## computed (and the value has not cahnged) and gets the value (skipping computation)
## if it has been previously computed. If not, cacheSolve computes the inverse of the
## matrix and caches the value of the inverse of the matrix via the setsolve function.


cacheSolve  <- function(x, ...){
        m  <-  x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data  <- x$get()
        m  <- solve(data, ...)
        x$setsolve(m)
        m
}
