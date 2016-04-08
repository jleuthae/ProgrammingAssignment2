## These are 2 functions that can calculate
## the inverted matrix of a given matrix.
## The cache allows you to store the 
## inverted matrix so it isn't calculated
## every time.

## Makes a list of functions to get and set 
## the matrix and inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvert <- function(invertedmatrix){
        im <<- invertedmatrix
    }
    getinvert <- function() im
    list(set = set, get = get, 
         setinvert = setinvert,
         getinvert = getinvert)
}


## Checks if the inverted matrix is stored, 
## and if not calculates it.  Either way, 
## returns the inverted matrix

cacheSolve <- function(x, ...) {
    im <- x$getinvert()
    if(!is.null(im)){
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data)
    x$setinvert(im)
    im
}
