## These functions allows for the calculation of the inverse 
## of an invertible square matrix using caching of the inverse
## of a matrix rather than computing it repeatedly.

## Usage: define a square matrix A whose inverse is sought
## .e.g for the solution of linear equation with different 
## righthand sides (A.x1=y1, A.x2=y2 etc). 
## The first step is to initialize that cache-matrix functionality
##        makeC <-makeCacheMatrix(matrix(data=c(2,0,0,2),nrow=2,ncol=2))
## Now, calculate the inverse if it not already exists, say, 
## for the use with different right hand sides
##        cacheSolve(makeC)%*%c(1,1),  cacheSolve(makeC)%*%c(0,1) .... etc.
##  e.g.
## > cacheSolve(makeC)%*%c(1,1)
## [,1]
## [1,]  0.5
## [2,]  0.5
## > cacheSolve(makeC)%*%c(1,0)
## getting cached data
## [,1]
## [1,]  0.5
## [2,]  0.0

## makeCacheMatrix(x) stores the matrix x to the default environment 
## (e.g. Global Environment) when first called. Subsequent call retrieves 
## the function from storage. Thus, this function mimics the effect 
## of a R-Environment based cache.

    makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
    }


## cacheSolve finds the inverse of a square matrix using the 
## cache-functionality introduced in makeCacheMatrix(x). The 
## cacheSolve-function takes a single square-matrix x stored 
## in the makeCacheMatrix(x) format and calculates the inverse 
## of x if this is not already in cache (i.e. the actual working
## environment) and otherwise retrieves it from the cache. This 
## version checks if the matrix is actually a square matrix, 
## but other error-handling is left to the solve-function.

    cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        if (dim(data)[1]!=dim(data)[2]){
            stop("error:only square matrices are allowed")
        }
        m <- solve(data,...)
        x$setinverse(m)
        m
    }
