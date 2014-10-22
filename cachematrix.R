#-- Function for calculating and caching the inverse of a given matrix.
#   To set a matrix:
#       x <- matrix(1:4,2,2)
#       xx <- makeCacheMatrix(x)
#   To get the original matrix back:
#       xx$get()
#   To calculate the inverse OR return from memory if available:
#       i <- cacheSolve(xx)
#   To change the input matrix, and erase the inverse in memory:
#       y <- matrix(c(1,2,4,5),2,2)
#       xx$set(y)
#
#-- Creating the cache matrix with an input matrix x. Returned is a list with
#   functions to save and retrieve the original matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # Set the inverse to NULL
        i <- NULL
        
        # Function for saving an input matrix, and resetting the inverse to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # Function for returning the original matrix
        get <- function() x
        
        # Functions for saving and returning the inverse
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        # Return a list with the four above functions 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#-- Solving the inverse of the cache matrix and return, or retrieve from 
#   memory if previously calculated
cacheSolve <- function(x, ...) {
        # Check if inverse is already calculated. Return if yes.
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Retrieving inverse from cache.")
                return(i)
        }
        
        # Inverse is not yet calculated. Retrieve the original matrix and solve
        data <- x$get()
        i <- solve(data, ...)
        
        # Save the inverse in memory, and return it.
        x$setinverse(i)
        i
}
