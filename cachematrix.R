
## makeCacheMatrix() returns a list of 4 functions to cache a matrix and its inverse:
#  setMtx - set value of cached matrix.
#  getMtx - get value of cached matrix.
#  setInversedMtx - set value of inversed cached matrix.
#  getInversedMtx - get value of inversed cached matrix.
#  ------------------------------------------------------------------------------
#  makeCacheMatrix() parameters:mtx - matrix from which inverse should be cached.

makeCacheMatrix <- function(mtx = matrix()) {
    #inversed matrix will be saved here:
    mtxInv <- NULL
    #function to set matrix in cache:
    setMtx <- function(m) {
        mtx <<- m 
        mtxInv <<- NULL}
    #function to get matrix from cache:
    getMtx <- function() mtx
    #set inversed matrix in cache
    setInversedMtx <- function(inversedMtx) mtxInv <<- inversedMtx
    #get inversed matrix from cache
    getInversedMtx <- function() mtxInv
    list(setMtx = setMtx, getMtx = getMtx,setInversedMtx = setInversedMtx,getInversedMtx = getInversedMtx)
}


## cacheSolve() will use makeCacheMatrix() to cache and return (from cache) inversed matrix of one specific matrix.
#  ----------------------------------------------------------------------------------------------------------------
#  cacheSolve() parameters:     
#  x - functions list returned by makeCacheMatrix().
#  m - matrix we want to get its inverse from cache.
cacheSolve <- function(x,m) {
    # if this is the first time we are calling cacheSolve - cache m:
    if(is.na(x$getMtx())) x$setMtx(m)
    #get inversed matrix from cache (if exists):
    mInv <- x$getInversedMtx()
    #assert inversed matrix exists: 
    if(!is.null(mInv)) {
        #assert inversed matrix is of m:
        if (identical(m,x$getMtx())) {
            message("getting inversed matrix")
            return(mInv)
        }
        return(Null)
    }
    mtxCache <- x$getMtx() #return m from cache
    mInv <- Solve(m) # calculate inversed matrix of m
    x$setInversedMtx(mInv) # cache inversed matrix
    mInv # returne inversed matrix
}
