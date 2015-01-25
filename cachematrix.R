## File: cachematrix.R
## Description: Contains functions to compute and cache the inverse of a matrix

## Function: makeCacheMatrix  
## Description: creates a special "matrix", which is really a list containing a function to perform the following:
## 1. set the value of the inverse matrix
## 2. get the value of the inverse matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
## Inputs: an invertible matrix to "specialize"
## Outputs: specialized matrix with cache inverse functionality

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL

    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Function: cacheSolve
## Description: computes the inverse of the special "matrix" returned by makeCacheMatrix
##              if inverse has already been calculated (and the matrix has not changed via 'set')
##              then will inverse from the cache.
## Inputs: specially formatted "matrix" via makeCacheMatrix
## Output: inverse of matrix provided

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
  
    if(!is.null(m)) {
        message("getting cached inverse matrix data")
        return(m)
    }
    data <- x$get()
    #compare source matrix with provided  
    m <- solve(data, ...)
    x$setinverse(m)    
    m        
        
}
