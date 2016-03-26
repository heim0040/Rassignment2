## The makeCacheMatrix function creates a matrix that can cache its inverse.
## cacheSolve computes and stores the inverse of the vector created by makeCacheMatrix

## The makeCacheMatrix function creates a vector containing a function that 1) sets the value of the matrix 'x' in the 
## containing environment using the <<- assignment operator, 2) gets the value of the matrix, 3) sets the value of the 
## inverse of the matrix, and 4) gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y           
                m <<- NULL        
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse.
             getinverse = getinverse)
}

# cacheSolve computes the inverse of the vector created in makeCacheMatrix. 
# It first checks to see if the inverse has already been calculcated in makeCacheMatrix.
# If the inverse has been calculated it gets the inverse from the cache.
# If not it calculates the inverse of the matrix and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}