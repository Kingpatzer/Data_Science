## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix = function(x) {
        inverse = NULL
        
        set = function () {
                x <<- x
                inverse <<- inverse
        }
        
        get = function () {
                x
        }
        
        setinverse = function (x){
                inverse <<- x
        }
        
        getinverse = function (){
                inverse
        }
       
        list(set = set, get=get, setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {

        
        inverse = x$getinverse()
        
        # if the inverse has already been calculated
        if (!is.null(inverse)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inverse)
        }
        
        # otherwise, calculates the inverse 
        matrix.data = x$get()
        inverse = solve(matrix.data, ...)
        
        # sets the value of the inverse in the cache via the setinverse function.
        x$setinverse(inverse)
        
        return(inverse)
}


