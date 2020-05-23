## This code will cache the inverse of a matrix

## The follwing function will create a special 'matrix' to store its inverse
## The objective of the first function is to create a list containing a function to set and get the matrix and set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                y <<- x
                inv <<- NULL
        }
        get <- function() x 
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set=set,get=get,
             setsolve=setsolve,
             getsolve=getsolve)

}


## This function compute the inverse of the special matrix precedently created. If the inverse has already 
## been cached (without having the matrix changed),then the function will show a message and return the value from the cache.
## Note: matrix row by column product can be performed with the %*% operator

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)){
                message('getting cached data')
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setsolve(inv)
        inv
}

## Control can be performed considering that a matrix multiplied for its inverse will return the identity matrix
## I = A%*%(A^-1)