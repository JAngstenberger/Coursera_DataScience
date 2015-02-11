## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix creates a list containing a function to
## 1. set the matrix value
## 2. get the matrix value
## 3. set the inverse matrix value
## 4. get the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## cacheSolve returns the inverse matrix. It first checks to see if
## the inverse has already been calculated. If so, it gets the result from the cache
## and skips the computation. Otherwise, it calculates the inverse and sets the 
## value in the cache via setinverse function.

## This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
