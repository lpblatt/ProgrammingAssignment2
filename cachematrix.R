## Programming Assignment 2 
## Create functions to cache the inverse of a matrix

## Function to create "matrix" object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, 
         get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
    
}

## Function computes the inverse of the matrix returned by 
## makeCacheMatrix function above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached invserse")
        return(i)
    }
    
    m1 <- x$get()
    i <- solve(m1, ...)
    x$setinverse(i)
    i
}
