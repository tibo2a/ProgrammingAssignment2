## In MakeCaMatrix we defined 4 fucntions:
#the first one  is set, which sets the matrix,
## the second is get, which gets the matrix,
## the third sets the inverse of the matrix
## the fourth gives you the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        getInverse<- function() inverse
        setInverse<-function(y) inverse <<- y
        list(set = set, get = get,
             getInverse=getInverse,
             setInverse=setInverse
        )
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
       
        
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
