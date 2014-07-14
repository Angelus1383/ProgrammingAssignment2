## The script file implements two functions. 
## The first builds a spacial "matrix" able to cache its inverse. 
## The second computes the inverse of the spcial matrix if this isn't cached, 
## otherwise returns the precalculated inverse matrix.

## The function builds a special "matrix" able to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    ## Sets the matrix to invert initializing the cached invertedMatrix to NULL.
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    ## Returns the matrix to invert.
    get <- function(){
        x
    }
    ## Sets the corresponding inverted matrix.
    setInverse <- function(inv){
        inverse <<- inv
    }
    ## Returns the corresponding inverted matrix if cached, NULL otherwise.
    getInverse <- function(){
        inverse
    }
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Computes the inverse of the special "matrix" if it's not cached or is changed
## and chache it, otherwise returns the cached inverse matrix.
## The function assumes that the input matrix is invertible.
cacheSolve <- function(x, ...) {
    ## Retrieve the inverted matrix value from the cachableMatrix object
    invM <- x$getInverse()
    ## If the inverted matrix it's not in the cache, 
    ## then computes it and adds it to the chache.
    if(is.null(invM)){
        mToInvert <- x$get()
        ##Compute the inverted matrix
        invM <- solve(mToInvert)
        x$setInverse(invM)
    }else{
        message("getting cached data")
    }
    ## Return a matrix that is the inverse of 'x'
    invM
}