## Put comments here that give an overall description of what your
## functions do

## Get a matrix, check to see if matrix is already cached, calculate inverse of matrix

makeCacheMatrix <- function(x = matrix()) { 
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


## checks to see if inverse of matrix already exists
## calculates inverse if it does not already exist
##retrieve if it exists
cacheSolve<- function(x, ...) {                 
    m <- x$getinverse()
    if(!is.null(m)) {                 
    message("getting Inverse of the matrix")
    return(m)
    }
    matrix <- x$get()                               
    m <- solve(matrix, ...)
    x$setinverse(m)
    m
}
