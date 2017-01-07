## Calculating the inverse of a matrix is a timeconsuming operation and these
## functions attempt to provide constucts that store the result of a inverse 
## operation on a matrix into a cache and read off the cache for subsequent 
## inverse requests.

## The makeCacheMatrix function returns a matrix object wrapped in a list which 
## functions to get and set the matrix along with functions to store/cache the
## inverse and retrieve it

makeCacheMatrix <- function(x = matrix()) {
m_mat <- NULL
set <- function(y_mat){
        x <<- y
        m_mat <<- NULL
}

get <- function() x
setinverse <- function(inverse) m_mat <<- inverse
getinverse <- function() m_mat
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat<-x$getinverse()
        if(!is.null(mat)){
                message("returning inverse from cache")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data,...)
        x$setinverse(mat)
        mat
}
