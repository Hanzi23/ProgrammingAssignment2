## This file consists of 2 function, one function stores the information of
## the matrix and another calculates the inverse of the matrix,
## in case the inverse had already been calculated, returns that previous
## calculated


##This function makes a list of functions for the variable x, 
##set allows to set the value of the matrix, 
##get gives the current matrix stored, 
##setinv allows to set the inverse of the matrix 
##getinv returns the inverse matrix in case it had been already set

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function calculates the inverse of the matrix through the "solve"
## function and sets the inverse matrix in x; in case it had already been
## calculated, returns that stored value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached inverse matrix...")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
