## The purpose of makeCacheMatrix and cacheSolve functions is to set up a caching environment
## for an invertable matrix and its inverse for use in subsequent functions and calculations.
## This avoids the overhead of repeated matrix inversions. 
## makeCacheMatrix creates the cache environment for a specific invertable matrix.
## The matrix is then inverted once and cached for subsequent calls via cacheSolve.

## makeCacheMatrix takes an invertable matrix as input and generates a vector of 4 functions. 
## It saves the input matrix into a variable in the parent environment and 
## initializes the variable to hold the inverse of that matrix in the parent environment.
## 
## The 4 functions are named objects in the output list.
## $set - calls the set function, which stores the input matrix and initalizes the 
##              variable to hold the inverted matrix.
## $get - calls the get function, which retrieves the input matrix.
## $setinverse - calls the setinv function, which stores the inverted matrix.
## $getinverse - calls the getinv function, which retrieves the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                     # initalize the inverse matrix variable
    set <- function(y) {                          # set matrix 
      x <<- y                                     # cache the input matrix in parent env
      m <<- NULL                                  # clear the inverse matrix in parent env
    }
    get <- function() x                           # get matrix  
                                                  # return input matrix from parent env
    setinv <- function(inverse) m <<- inverse     # set inverse  
                                                  # cache matrix inverse in parent env
    getinv <- function() m                        # get inverse 
                                                  # return matrix inverse from parent env
    list(set = set, get = get,                    # associate functions to names in list
         setinverse = setinv,                     # that is assigned to hold results of 
         getinverse = getinv)                     # makeCacheMatrix function call
}


## cacheSolve takes an invertable matrix as input.
## makeCacheMatrix must be called before execution of cacheSolve for each matrix used.
## After the initial call of makeCasheMatrix, cacheSolve may be called any number of times 
## to retrieve the matrix inverse.
## If the matrix inverse has already been cached in a prior call of cacheSolve
## the function returns the cached inverted matrix.
## Otherwise, the matrix is inverted and cached for subsequent calls, and returned.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()                          # retrieve the inverse matrix from cache
    if(!is.null(m)) {                            # if inverse matrix is not null
         message("getting cached matrix")
         return(m)                               # return inverse matrix retrieved from cache
    }
    mxdata <- x$get()                            # else retrieve input matrix from cache
    m <- solve(mxdata, ...)                      # call solve function to invert matrix
    x$setinverse(m)                              # cache inverted matrix 
    m                                            # return inverted matrix
}
