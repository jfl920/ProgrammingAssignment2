# These functions give the inverse of a matrix in a time-consuming computation manner - 
# i.e. if the inverse of a matrix has been calculated and the contents of a matrix did not change, 
# it will produce the cached inverse matrix, otherwise it will calculate the inverse and cache it

# This function creates a special "vector", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL # i is the inverse matrix, it is set to NULL everytime makeCacheMatrix is called
    
    set <- function(y) { # set the value of the vector
        x <<- y
        i <<- NULL # update variable m
    }
    get <- function() {x} # Returns the value of the original matrix
    setinverse <- function(solve) {i <<- solve} # Sets value of the inverse of the matrix
    getinverse <- function() {i} # Gets the value of the inverse matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse() # Accesses object i and gets the inverse matrix
    if (!is.null(i)) { # If i is already cached
        
        message("getting inverse matrix") # ... send this message to console
        return(i) # ... return the inverse matrix   
    }
    data <- x$get() # We reach this code only if x$getinverse() is returned NULL
    i <- solve(data, ...) # If i is NULL then calculate the inverse matrix
    x$setinverse(i) # Store the calculated inverse matrix in x (see setinverse() in makeCacheMatrix)
    i # Return the inverse matrix to the code that called this function
    
}
