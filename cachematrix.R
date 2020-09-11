## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse} 
        getinverse <- function() {inv}
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        
}

 
## cacheSolve calculates the mean of the special "matrix" created 
#  with the above function. However, it first checks to see if the 
#  inverse matrix has already been calculated. If so, it gets the inverse matrix
#  from the cache and skips the computation

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cache data")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}


