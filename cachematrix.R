## Below a pair of functions caches the inverse of a matrix 

## The function 'makeCacheMatrix' creates the list of functions that assign values of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        #creat Null value to be assigned the value of the inverse
        
        set <- function(y, r, c) {
                x <<- matrix(y, r, c)
                i <<- NULL
        } #assign the value of the matrix which has 'y' data, 'r' rows, 'c' columns to the matrix 'x'
          #assing NULL to 'i' to refresh the value of 'i'
        
        get <- function() x
        #get the value of the matrix 'x'
        
        setinverse <- function(inverse) i <<- inverse 
        #assign the value of the inverse of the matrix 'x' to 'i' 
        
        getinverse <- function() i
        
        #get the value of the inverse 'i'
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        #creat the list of 'set', 'get', 'setinverse' and 'getinverse' functions
}



## The function 'cacheSolve' caches the inverse of the matrix created with the above function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        #assign the value of the inverse of the matrix created with the above function to 'i'
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        } # if the inverse of the matrix has already been calculated, return 'i'
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        # if the inverse of the matrix has not already been calculated,
        # calculate the inverse 'i' and set the value of 'i'
}
