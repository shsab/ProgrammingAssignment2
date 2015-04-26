## Cretae a special matrix that contains the matrix and 
## Caches its invert.

## Assume matrix provided is invertable
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(Y){
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInvert <- function(xInvert) xInv <- xInvert
  getInvert <- function() xInv
  list(set = set, get = get,
       setInvert = setInvert,
       getInvert = getInvert)
}


## Check if invert exists in chache return it, otherwise calculate

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInvert()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  MAT <- x$get()
  tol <- Null
  EXP <- -1
  #MAT <- as.matrix(MAT)
  matdim <- dim(MAT)
  if(is.null(tol)){
    tol=min(1e-7, .Machine$double.eps*max(matdim)*max(MAT))
  }
  if(matdim[1]>=matdim[2]){ 
    svd1 <- svd(MAT)
    keep <- which(svd1$d > tol)
    xInv <- t(svd1$u[,keep]%*%diag(svd1$d[keep]^EXP, nrow=length(keep))%*%t(svd1$v[,keep]))
  }
  if(matdim[1]<matdim[2]){ 
    svd1 <- svd(t(MAT))
    keep <- which(svd1$d > tol)
    xInv <- svd1$u[,keep]%*%diag(svd1$d[keep]^EXP, nrow=length(keep))%*%t(svd1$v[,keep])
  }
  x$setInvert(xInv)
  xInv
}
