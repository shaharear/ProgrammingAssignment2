## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## It is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the invers matrix
## get the value of the invers matrix


makeCacheMatrix <- function(x = matrix()) {
        
        ## inversMatrix will store the invers of given matrix 
        inversMatrix <- NULL
        
        ## setter for the matrix
        set <- function(y) {
                x <<- y
                inversMatrix <<- NULL
        }
        
        ## getter for the matrix
        get <- function() x
        
        ## setter for invers
        setmatrix <- function(invmtx) inversMatrix <<- invmtx
        
        ## setter for invers
        getmatrix <- function() inversMatrix
        
        ## list of functions
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmtx <- x$getmatrix()
        
        ## If the inverse is already calculated then return it
        if(!is.null(invmtx)) {
                message("getting cached data")
                return(invmtx)
        }
        
        ## calculate invers matrix
        data <- x$get()
        invmtx <- solve(data, ...)
        
        ## cashed the invers matrix
        x$setmatrix(invmtx)
        
        ## return the invers matrix
        invmtx
}

##How to Test
## create a 2 by 2 matrix x
## x <- matrix(c(4,3,3,2), 2, 2)
## See the matrix x
##x
## Pass the matrix to convert it a special matrix object
## cx <- makeCacheMatrix(x) 
## get the special matrix object
## cx$get() 
## Calculate the matrix invers 1st time
## cacheSolve(cx) 
## Calculate the matrix invers again. As the invers is already cashed, it return the result from cache.
##cacheSolve(cx) 
