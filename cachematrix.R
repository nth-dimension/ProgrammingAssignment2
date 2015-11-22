# The following two functions in this file will cache a matrix and its 
# inverse.

# This function creates a special 'matrix' which actually is a list containing
# functions that
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y = matrix()) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
# This function calculates the inverse of the special matrix that is created
# using the 'makeCacheMatrix' function
# It first checks if the inverse has already been calculated, if so, it skips
# the calculation. Otherwise the inverse of the matrix is computed and the 
# value is set using the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
