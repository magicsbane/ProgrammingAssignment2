#Project 2 Assignment, R Programming, Build (R version 3.1.3, RStudio version 0.98.1091)
##Purpose: a pair of functions that cache the inverse of a matrix
###The function makeCacheMatrix creates a special "matrix" that can cache the inverse of the matrix
###The function cacheSolve computes the inverse of the special matrix or retrieves the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function (y) {
        x <<- y
        m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function () m
list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function takes a supplied matrix that is invertible and sets the value
## of the matrix (line 8), gets the value of the matrix (line 12), sets the
## value of the inverse of the matrix (line 13) and gets the value of the 
## inverse of the matrix (line 14).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data<- x$get()
        m<- solve(data,...)
        x$setinverse(m)
        m
## line 26 calls the getinverse from previous function and sets it to m
## If m is not NULL (line 27) the message is displayed and the cached data returned
## If m is NULL then the inverse of the matrix is calculated begining at line 31
}
