# Matrix inversion is computationally intensive, to address this challenge, it would be 
# good to caching the inverse of a matrix to prevent calculating repeatedly.
# The following functions helps achieve the same.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The below function returns the inverse of the matrix. The function is expected to first checks if
# the inverse has already created. If so, it gets the results & skips the calculation.
# If not, it calculates the inverse, sets the value in the cache via setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample run:
## > x = rbind(c(1, -1/8), c(-1/8, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]   [,2]
## [1,]  1.000 -0.125
## [2,] -0.125  1.000
## > cacheSolve(m)
##          [,1]      [,2]
## [1,] 1.0158730 0.1269841
## [2,] 0.1269841 1.0158730
## > 
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0158730 0.1269841
## [2,] 0.1269841 1.0158730