## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This fucntion below creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" which is created by
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

## ====================================================================
## Testing the function
my_matrix <- makeCacheMatrix(matrix(rnorm(9,3), 3, 3))
> my_matrix$get()
         [,1]     [,2]     [,3]
[1,] 1.409887 2.473466 2.472283
[2,] 4.380639 3.644130 2.920300
[3,] 2.944259 3.215404 3.447679
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
           [,1]       [,2]       [,3]
[1,] -0.9567562  0.1743369  0.5384075
[2,]  1.9609072  0.7289681 -2.0236002
[3,] -1.0117443 -0.8287372  1.7175265
> cacheSolve(my_matrix)
getting cached data
           [,1]       [,2]       [,3]
[1,] -0.9567562  0.1743369  0.5384075
[2,]  1.9609072  0.7289681 -2.0236002
[3,] -1.0117443 -0.8287372  1.7175265
> my_matrix$getInverse()
           [,1]       [,2]       [,3]
[1,] -0.9567562  0.1743369  0.5384075
[2,]  1.9609072  0.7289681 -2.0236002
[3,] -1.0117443 -0.8287372  1.7175265

> my_matrix$set(matrix(c(2, 2, 3, 1, 1, 4, 3, 6, 9), 3, 3))
> my_matrix$get()
     [,1] [,2] [,3]
[1,]    2    1    3
[2,]    2    1    6
[3,]    3    4    9
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
              [,1]       [,2] [,3]
[1,]  1.000000e+00 -0.2000000 -0.2
[2,] -6.661338e-17 -0.6000000  0.4
[3,] -3.333333e-01  0.3333333  0.0
> cacheSolve(my_matrix)
getting cached data
              [,1]       [,2] [,3]
[1,]  1.000000e+00 -0.2000000 -0.2
[2,] -6.661338e-17 -0.6000000  0.4
[3,] -3.333333e-01  0.3333333  0.0
> my_matrix$getInverse()
              [,1]       [,2] [,3]
[1,]  1.000000e+00 -0.2000000 -0.2
[2,] -6.661338e-17 -0.6000000  0.4
[3,] -3.333333e-01  0.3333333  0.0
