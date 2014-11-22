## The combination of following functions help us to save time
## in computation of inverse matrix. Here we compute the inverse 
## matrix of a given matrix only for one time and save it. 
## Whenever we need the inverse matric, we just used the saved result.

## makeCacheMatrix produces a list of fucntion to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) Inv <<- solve
    getsolve <- function() Inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

##  cacheSolve return the inverse of matrix x.
## If the inverse matrix has been already calculated,
## the function returns the saved inverse matrix.
## Otherwise, it computed the inverse matrix and save
## it for later times.
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

## Example:
## > matrix1<-matrix(1:4,2,2)
## > matrix1
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > a<-makeCacheMatrix(matrix1)
## > class(a)
## [1] "list"
## > cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## The above matrix is the inverse of matrix1
