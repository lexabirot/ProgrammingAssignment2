## This function creates a list with 4 functions that make possible to retrieve for a matrix, its inverse.
## Each time this function is executed, it prints "new matrix"
## The result must be affected to t

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        print("new matrix")
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s   
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function takes in entry the result of the function makeCacheMatrix u and the matrix being analysed v
## If v has changed then makeCacheMatrix is executed before getting or computing the inverse of the matrix
## Then t in global environment is updated

cacheSolve <- function(u,v,...) {
        if(!identical(u$get(),v)){
                t<<-makeCacheMatrix(v)
        }
        s <- t$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- t$get()
        s <- solve(data,...)
        t$setsolve(s)
        s
}
