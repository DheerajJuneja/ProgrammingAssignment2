## Caching the Inverse of a Matrix:
## Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
## attributes -->
## x --> matrix
## inv --> inverse of the matrix(set to NULL initially)
## functions -->
## set() --> to set x
## get() --> to get x
## setInv()--> to set inverse
## getInv() --> to get inverse


makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() 
		{ inv }
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function calculates the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data !")
                return(inv)
        }
        matData <- x$get()
        inv <- solve(matData, ...)
        x$setInv(inv)
        inv
}
## Steps to EXECUTE the funtion(Example)
##my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##my_matrix$get()
##cacheSolve(my_matrix)
##my_matrix$getInv()