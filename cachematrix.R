##the makeCacheMatrix and cacheSolve has been written to check whether
##the inverse of the given matrix is being computed or not,if the the inverse 
##has computed then this cache function will gives the already stored result
##if not then it will solve the matrix and return the inverse by computing
                    

## the makeCacheMatrix will calculate the inverse of the matrix 
##it will set the value of matrix ,get the value of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL           ##null objects
        set <- function(y){   ##set the value of the matrix
          x <<- y
          inv <<- NULL
        }
        get <- function(){x}     ##get the value of the matrix
        setInverse <- function(inverse) {inv <<- inverse }     ##set the value of the inverse
        getInverse <- function() {inv}      ##get the value of the inverse
        list(set = set, get=get, setinverse = setInverse, getInverse = getInverse)  ##
          
}


## the following function will check if we have computed the inverse
##of the matrix or not,if computed get the cached data and if not then solve 
##the given matrix and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()     ##inverse of x asssigned to inv
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)     ##inverse will be returned
        }
        mat <- x$get()
        inv <- solve(mat, ...)     ##to getting the inverse of mat
        x$setInverse(inv)          
        inv
}
