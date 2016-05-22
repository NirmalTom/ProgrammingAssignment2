## cache the matrix inverse


## invertible square matrices as input

makeCacheMatrix <- function(n = matrix()) {
                 n_inv <- NULL
                 set <- function(y) {
                        n <<- y
                        n_inv <<- NULL
                 }
                 get <- function() n
                 setinv <- function(inv) n_inv <<- inv
                 getinv <- function() n_inv
                 list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## returns inverse of the matrix 
##if matrix is not changed, returns cached inverse matrix
##if matrix is new, it returns and caches calculated inverse


cacheSolve <- function(n, ...) {
            n_inv <- n$getinv()
            if(!is.null(n_inv)) {
                message("Getting cached data")
                 return(n_inv)
            }
            n <- n_inv$get()
            n_inv <- solve(n, ...)
            n$setinv(n_inv)
            n_inv
}
        ## Return  inverse of 'n', which is a matrix
        
}
