## If the contents of a matrix are not changing,
## it makes sense to cache inverse of the matrix
## so that when the inverse is needed again,
## it can be looked up in the cache rather than recomputed.

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

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
             setinverse = setinverse,
             getinverse = getinverse)        
}


## The following function calculates the inverse
## of the special "matrix" created with the above function.
## However, it first checks to see if the inverse of the matrix
## has already been calculated.
## If so, it gets the inverse of the matrix from the cache
## and skips the computation.
## Otherwise, it calculates the inverse and stores it in the cache
## via the setinverse function.

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
