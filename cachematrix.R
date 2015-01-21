## Below a pair of functions caches the inverse of a matrix 

## The function 'makeCacheMatrix' makes the list which has functions to assign values of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y, r, c) {
                x <<- matrix(y, r, c)
                i <<- NULL
        } #assign the value of the matrix which has 'y' data, 'r' rows, 'c' columns to 'x'
        
        get <- function() x
        #get the value of the matrix 'x'
        
        setinverse <- function(inverse) i <<- inverse 
        #assign the value of the inverse of the matrix 'x' to 'i' 
        
        getinverse <- function() i
        
        #get the value of the inverse 'i'
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        #make the list which has components that are 'set', 'get', 'setinverse' and 'getinverse' functions
}



## The function 'cacheSolve' caches the inverse of the matrix created with the above function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        #assign the value of the inverse of the matrix created with the above function to 'i'
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        } # if the inverse of the matrix created with the above function has already been calculated, return 'i'
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        # if the inverse of the matrix created with the above function has not already been calculated,
        # calculate the inverse 'i' and set the value of 'i'
}
