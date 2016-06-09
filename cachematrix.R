##
## The intent of the functions is to make repeated computation
## of a matrix faster by caching the computed inverse values.
##
## If the inverse of a matrix has been computed earlier then 
## from second request onwards the cached value of the inverse
## will be returned
##


## Create a function creates a fuction that acts like an object
## with 4 functions - set, get, setInverse, getInverse

makeCacheMatrix <- function(x = matrix()) {
        
        mInverse <- NULL
        
        set <- function(y){
                x <<- y
                mInverse <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(computedInverse) mInverse <<- computedInverse
        
        getInverse <- function() mInverse
        
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
        
}


## This function looks checks whether the inverse of the matrix has
## has been calculated earlier. If so, it returns the cached value
## otherwise, it computes the inverse, sets it in cache and returns
## the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getInverse()
        if(!is.null(i)){
                message("Getting cached inverse")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
