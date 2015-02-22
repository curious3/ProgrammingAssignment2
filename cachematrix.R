## Functions to calculate and cache a matrix and its inverse
## Usage:
## a1 <- makeCacheMatrix(matrix(rnorm(16, mean=3), 4, 4))
## a2 <- cacheSolve(a1)
## a3 <- cacheSolve(a1) # returns cached inverse
## a2 %*% a1$get() # verify
## a3 %*% a1$get() # result is same as with a2

## Initializes the function with a matrix
## Matrix must be square (nrow == ncol)
makeCacheMatrix <- function(x = matrix()) {
        # Set the matrix
        # stops if matrix is not square
        set <- function(y) {
                if (nrow(y) != ncol(y)) stop("Not a square matrix")
                x <<- y
                # If matrix is set, must clear any cached inverse
                inv <<- NULL
        }
        
        # get the specified matrix
        get <- function() x
        
        # get/set inverse
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv

        # constructor, initialize
        set(x)
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        # get the inverse
        inv <- x$getinverse()
        
        # if inverse already exists i.e. not null
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # else calculate inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

