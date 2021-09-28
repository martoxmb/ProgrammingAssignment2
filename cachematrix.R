## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The functions below calculate inverse matrix and store it in to cache to be extracted later

## The function set the value of the matrix, get the value of the matrix, set the value of the inverse
## matrix, get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
             x <<- y
             inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

## The following function calculates the inverse of the matrix created with the function above.
## It first checks if the inverse has already been calculated. If so, it gets the inverse matrix from
## the cache and skips the computation. Otherwise, it calculates the inverse matrix and sets the
## value of this new matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!is.null(inv)) {
            message('getting cached data')
            return(inv)
        }
        mm <- x$get()
        inv <- solve(mm, ...)
        x$setinv(inv)
        inv
}
