## These functions calculate the inverse of a matrix, if not available in the cache
## or retrieve the result of the calculation from the cache, if it has been calculated
## previously

## Create a list containing the functions "set" and "get" to set/get the matrix associated with 
## this instance of the list and "setinverse" / "getinverse" to set/get the inverse of the matrix
## As per lexical scoping rules, the variables "mtrx" and "invMtrx" are become part of the
## instance of the list created here
makeCacheMatrix <- function(mtrx = matrix()) {
   invMtrx <- NULL
   
   ## Function "set" to store the input matrix and initiate the resultimatrix 
   set <- function(inp) {
      mtrx <<- inp
      invMtrx <<- NULL
   }

   ## Function "get" to retrieve the input matrix 
   get <- function() mtrx

   ## Function "setmatrix" to store the inverted matrix in the cache 
   setmatrix <- function(inv) invMtrx <<- inv

   ## Function "getmatrix" to retrieve the inverted matrix in the cache 
   getmatrix <- function() invMtrx
   list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## Return the inverse of a matrix either from the cache (if available) or 
## by calculating it, accept the cached matrix as a parameter
cacheSolve <- function(cacheMtrx, ...) {

   ## Try to get the inverted matrix from the cache
   invMtrx <- cacheMtrx$getmatrix()

   ## If it is available, return the cached result 
   if(!is.null(invMtrx)) {
      message("getting cached data")

   ## If it is not available, calculate hte inverse and store the result in the cache 
   } else {
      mtrx <- cacheMtrx$get()
      invMtrx <- solve(mtrx, ...)
      cacheMtrx$setmatrix(invMtrx)
   }
   invMtrx 
}
