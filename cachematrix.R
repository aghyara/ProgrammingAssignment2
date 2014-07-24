## A pair of functions that compute and cache the inverse of a matrix

## Function to create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initializing the inverse variable to NULL
    invMatrix <- NULL
    
    ## Function to set the value of the matrix
    set <- function (m)
    {
        matrix <<- m
        invMatrix <<- NULL
    }
    
    ## Function to get the value of matrix by returning 'matrix' variable
    get <- function() matrix
    
    ## Function to set the inverse value of the matrix
    setInverseMatrix <- function(inverseMatrix)
    {
        invMatrix <<- inverseMatrix        
    }
    
    ## Function to get value of inverse Matrix, return 'invMatrix' variable
    getInverseMatrix <<- function() invMatrix
    
    ## Return the list of the methods
    list(set = set, get = get, 
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
    
}


## Purpose of this function, to computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## It checks if the inverse has already been calculated (and the matrix has not changed)
## If not changed, retrives the inverse matrix from the cache 
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInverseMatrix()
    
    ## Check if variable not null
    if(!is.null(invMatrix))
    {
        ## Variable already calculated/set, hence get data form cache
        message("getting cached data")
        ## Return the cached value
        return(invMatrix)
    }
    
    ## Else variable null, hence compute the inverse
    
    ## Get the matrix object
    data <- x$getMatrix()
    
    ## Calculate the inverse of the matrix
    invMatrix <- solve(data) %*% data
    
    ## Assign/Set the inverse value to the object x
    x$setInverseMatrix(invMatrix)
    
    ## Return the inverse matrix
    invMatrix 
    
}
