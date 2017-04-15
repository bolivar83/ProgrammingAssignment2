## This function create a cache marix object that can be used to
## at varius times solve the Inverse of the marix, but only
## calculates the Inverse once.
##
## Example use:
##  Mtx <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, ncol=3)
##  cacheMatrix <- makeCacheMatrix(Mtx)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(Mtx)      # Change the matrix being cached.
##  Mtx <- cacheMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInv(solve(data, ...)) #  function for cached inverse of matrix 
##  cacheMatrix$getInv()                 #  function used to get the cached inverse of matrix x
#######################################################################################################
makeCacheMatrix <- function(x = matrix()) {
               ## Create a cacheMatrix object for a matrix DET(matrix)!=0
               cachedInv <- NULL
               set <- function(y) {
                              x <<- y
                              cachedInv <<- NULL
               }
               get <- function() x
               setInv <- function(Inv) cachedInv <<- Inv
               getInv <- function() cachedInv
               list(set = set, get = get,setInv = setInv,getInv = getInv)
}

## Return the Inv of an cacheMatrix object

cacheSolve <- function(x, ...) {
               ## Return a matrix that is the Inverse of 'x'
               invFunc <- x$getInv()
               if(!is.null(invFunc)) {
                              message("getting cached data")
                              return(invFunc)
               }
               data <- x$get()
               invFunc <- solve(data, ...)
               x$setInv(invFunc)
               invFunc
}
