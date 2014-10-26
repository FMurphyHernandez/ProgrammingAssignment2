## A function to save computational time in calculating matrix inverses
## The functions create a new type of data that saves if the inverse of a matrix is already calculated. If it is already calculated it returns it from the mameory. If not, it calculates it.

## This function create a kind of new class of data, kind of matrix, that  also save if the inverse of the matrix is already calculated.

makeCacheMatrix <- function(x= matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the matrix that is in an object created with the last function has already evaluated by this function. If it's already evaluated, the function tell you that, and return you the inverse of the matrix from the memory. If is not, then in calculate the inverse of the matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
