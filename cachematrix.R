## Two functions that will be used to cache the inverse of a matrix

## makeCacheMatrix:Creates a cache matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function (y){
                x<<-y
                inverse<<-NULL
        }
        
        get <- function() x
        setinverse <- function(arg) inverse<<- arg
        getinverse<- function() inverse
        
        list(
        set=set,
        get=get,
        setinverse=setinverse,
        getinverse=getinverse
        )
}

## cacheSolve: Sets the inverse of matrix

cacheSolve <- function(x, ...) {
        inverse<- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        m<-x$get()
        inverse<-solve(m,...)
        x$setinverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
