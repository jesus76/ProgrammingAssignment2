## Put comments here that give an overall description of what your
## functions do

## creates an object that contains the matrix and the 
## available space for the inversematrix. 
## As input requires an invertible matrix and returns (OUTPUT)
## and object with the corresponding methods 
## to modify the matrix: set and get 
## and the inverse matrix: setinverse and getinverse

## Example of use:
## The following is an example of matrix that should have inverse
##> x = matrix(c(1,0,1,2,4,0,3,5,6),3,3)
##> x
##     [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    4    5
##[3,]    1    0    6
##> xCached <- makeCacheMatrix(x)
makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) xinv <<- inverse
        getinverse <- function() xinv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## As INPUT the function requires an object of the type
## returned by makeCacheMatrix that contains the matrix
## and the inverse matrix data (if set)
## As a result (OUTPUT) computes (in case not done before) the inverse 
## matrix or retrieves from object (in case the inverse was already
## computed and cached)

## Example of use:
##> xCached <- makeCacheMatrix(x)
##> cacheSolve(xCached)
##           [,1]        [,2]        [,3]
##[1,]  1.0909091 -0.54545455 -0.09090909
##[2,]  0.2272727  0.13636364 -0.22727273
##[3,] -0.1818182  0.09090909  0.18181818
##> cacheSolve(xCached)
##getting cached data
##           [,1]        [,2]        [,3]
##[1,]  1.0909091 -0.54545455 -0.09090909
##[2,]  0.2272727  0.13636364 -0.22727273
##[3,] -0.1818182  0.09090909  0.18181818
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	xinv <- x$getinverse()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinverse(xinv)
        xinv

}
