#returns a matrix including all functions for getting and setting inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        
        
        inv = NULL
        set = function(y) {
                
                x <<- y
                i <<- NULL
        }
        get = function() x
        setinv = function(inv) i <<- inv 
        getinv = function() i
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#checks for inverse of matrix and gets from cache or solves it from beginning
cacheSolve <- function(x, ...) {
        
        i = x$getinv()
        
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        mat.data = x$get()
        i = solve(mat.data, ...)
        
        x$setinv(i)
        
        return(i)
}