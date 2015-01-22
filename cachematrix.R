## overall description of the functions defined below

## Set of  functions to accelerate computation of matrix inversion
## by computing the inverse of a matrix object the first time and then
## caching the result for use in subsequent calls to compute inverse

## Usage
## a <- matrix(sample(100), nrow = 10, ncol = 10)
## acache <- makeCacheMatrix(a)
## cacheSolve(acache) --> Will compute inverse and store it in cache
## cacheSolve(acache) --> Will get inverse from cache



## Short comment describing this function
## Creates a list to store a matrix and its inverse
## provides methods to set and get the original matrix and methods
## to set and get the inverse of the original matrix
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


## Short comment describing this function
## Returns the inverse of a matrix. When called for the first time
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
