## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Creates a cache list of the inverse of a given matrix
# cache the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(minverse) m <<- minverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## Returns the inverse of a matri. When called for the first time
## on a matrix, the inverse is calculated and stored in a cache 
## list and the result is returned. For subsequent calls for the
## same / identical matrix the value computed earlier is returned
## from the cache list
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
