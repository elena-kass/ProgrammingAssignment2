## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse. 
## The "matrix" is a list that 1) sets the value of the matrix, 2) gets the value of the matrix
## 3) sets the value of the inverse 4) gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL            #create an empty matrix that will hold the inverse
       
        #this function will set the value of the matrix x      
        set <- function (y) {   
                x <<- y
                invx <<- NULL
        }
        
        #this function will get the value of the matrix x
        get <- function() x     
        
        #this function will set the inverse of matrix
        setinverse <- function(inverse) invx <<- inverse 
        
        #this function will get the inverse of matrix
        getinverse <- function () invx
        
        #output is a list with 4 parameters
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates th inverse of the matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated, the "cachesolve"  retrieves the inverse
## from the cache with no further computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <-x$getinverse()
        
        ## If the inverse is stored in the cache, it will be retrieved with no calculations
        if(!is.null(invx)) {
                message("retrieving cached data")
                return(invx)
        }
        
        ## If the inverse is not stored, then the origina matrix is retrieved from the list
        ## and the inverse of calculated
        data <- x$get()
        invx <- solve(data, ...)
        x$setinverse(invx)
        invx
}


