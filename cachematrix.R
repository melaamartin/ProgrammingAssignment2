##melaamartin Coursera data science R programming week 3 assignment
## Function to cache the inverse of a matrix

## 1. Create a matrix object that can cache its inverse

makeCacheMatrix <-function(A = matrix()){ 
    s = NULL #inverse function passed to solve
    set <- function(Y) {
        A <<- Y ##
        s <<- NULL #
    }
    get <- function() A 
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get, 
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## Compute the inverse of the matrix above. If inverse has already been calculated, cacheSolve will retrieve inverse from cache

cacheSolve <- function(A, ...) {
    s <- A$getsolve()
    if(!is.null(s)) { 
        message("getting cached data")
        return(s)
    }
    data <- A$get()
    s <- solve(data, ...)
    A$setsolve(s)
    s
}

##test (need to specify a square, nonsingular matrix)
B <- matrix(rnorm(9), nrow = 3, ncol = 3)##random 9 numbers in a 3 x 3 matrix to avoid a non-singular matrix
C <- makeCacheMatrix(B)
C$get()               # retrieve the value of A
C$getsolve()           # retrieve the value of s, which should be NULL, have to run cacheSolve()
cacheSolve(C)          # should retrun inverse
C$getsolve()           # now you can call it directly
