## Functions makeCacheMatrix and cacheSolve work together to create cache the inverse of a matrix
## and retrieve the inverse when called

## Description: creates a special matrix object that can cache its inverse
## Usage: makeCacheMatrix(x=matrix())
## Args: x, the matrix to be cached, defaults to empty matrix() if none are provided
## Returns: list of functions to be performed on cached matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) { ##set the matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x  ##retrieve matrix
    setinv <- function(inv) i <<- inv ##set the inverse of the matrix
    getinv <- function() i ##retrieve the inverse of the matrix
    list (set = set, 
          get = get, 
          setinv = setinv, 
          getinv = getinv)
}


## Description: computes the inverse of the special matrix object created by makeCacheMatrix()
## Usage: cacheSolve(x)
## Args: x, a cached Matrix object created by makeCacheMatrix(), required
## Returns: cached inverse of the special matrix object created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv() 
    if(!is.null(i)){ ##first test to see if the inverse is already cached; if it is, return it 
        message("getting cached inverse data")
        return(i)
    }
    data <- x$get() ##if the inverse is not cached, retrieve the cached matrix
    i <- solve(data,...) ##solve for the inverse and cache it
    x$setinv(i)
    i
}


##Code to help test the functions below, comment out when not running
##mymatrix<-matrix(data=c(2,4,-3,-7),nrow=2,ncol=2)
##mycachedmatrix <- makeCacheMatrix(mymatrix)
##myinversematrix <- cacheSolve(mycachedmatrix)
##mymatrix
#3myinversematrix

