## Calculating the inverse of a matrix is a timeconsuming operation and these
## functions attempt to provide constucts that store the result of a inverse 
## operation on a matrix into a cache and read off the cache for subsequent 
## inverse requests.

## The makeCacheMatrix function returns a matrix object wrapped in a list which 
## functions to get and set the matrix along with functions to store/cache the
## inverse and retrieve it
## Returns a list with names:
## set - function to set the value of the matrix
## get - function to get the value of the matrix
## setinverse - set the value of the matrix inverse
## getinverse - get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
#Initialize matrix variable as NULL each time
m_inv <- NULL

## setter function for matrix inside list
set <- function(y){
        x <<- y
        m_inv <<- NULL
}

## getter function for matrix inside list
get <- function() x

## function to set inverse
setinverse <- function(inverse) m_inv <<- inverse

## function to retrieve inverse
getinverse <- function() m_inv

## Create and return list
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Function to accept a matrix as input and return the inverse from cache if
## exists or use solve to get the inverse and store in cache before returning
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat<-x$getinverse()
        ##Check if inverse has already been cached
        if(!is.null(mat)){
                message("returning inverse from cache")
                ##Return inverse from cache
                return(mat)
        }
        ##If not cached get the matrix from list and solve for inverse
        data <- x$get()
        mat <- solve(data,...)
        x$setinverse(mat)
        
        ##Return inverse
        mat
}
