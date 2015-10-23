# There are two ways how to formulate makeMatrix function: a) you put matrix as a function argument; b) you put vector as a function argument, 
# and later you transform your vector into matrix. In this this script I'm using option a) so you need to put matrix, otherwise the script will stop.
# makeMatrix function returns the lists of functions, that will be used by the second function cacheinverse to calculate cache, and print the inverse
#matrix.
makeCacheMatrix <- function(x = matrix()) {
            if(!is.matrix(x)) stop("x must be a matrix")
            m <- NULL
            #function set is not used unless it is explicitedly called from console.
            set <- function(y) {
                        x <<- y
                        m <<- NULL
            }
            # This is the main part of function. It defines 3 main functions that a) gets the matrix and simply returns it; b) takes the inverse from outside 
            #(as argument)  and sets its value in the parent enviroment (makeCacheMatrix evironment)  c) the set inverse value can be taken and used by 
            #the third function.
            
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}
# This functions acts as follows - it calls subfunctions set by my makeCacheMatrix functions to get and send back the required data and calculated inverse
cacheSolve <- function(x, ...) {
            # subfunction getinverse brings back the cashed inverse value if it was calculated before, otherwise it continues to calculate it.             
            m <- x$getinverse()
            if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
            }
            # the matrix value is taken from makeCacheMatrix environment its inverse is calculated and sent back to makeMatrix enviroment. Additionaly the inverse
            # value is printed in the console.
            data <- x$get()
            m <-  solve(data, ...)
            x$setinverse(m)
            m
}