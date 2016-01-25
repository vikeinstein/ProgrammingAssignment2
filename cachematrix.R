# R script written by Victor N. Emenike
# for the Courera R programming course
# offered by Johns Hopkins University (JHU)
# written on 25.01.2016 by 07:51
# The script contains a pair of functions that 
# caches the inverse of a matrix 
# with the first function
# creating and storing a speacial "matrix" object, 
# while the second function computes inverse of the matrix
# returned by the first function
# 
# --------------------------------------

# The first function makeCacheMatrix 
# creates a special "matrix" object (which is a list of functions) 
# and caches the inverse of the object 
#
makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    setmatrix <- function(y){
        x <<- y
        inv <<- NULL #restores the inverse to NULL since a new input has been declared
    }
    getmatrix <- function() x
    # the next line solves the inverse of the matrix and allocates it to "inv"
    setinverse <- function(solve) inv <<- solve 
    # the next line returns the stored inverse of the matrix
    getinverse <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The second function cacheSource computes the
# inverse of a special "matrix" returned by 
# the first function makeCacheMatrix.
# In a situation where the inverse has already
# been calculated and the matrix has not changed
# the function, cacheSolve simply retrieves the 
# inverse from the cache

cacheSource <- function(x,...){
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$getmatrix()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}