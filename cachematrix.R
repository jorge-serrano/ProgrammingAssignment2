#creates a special matrix with the supplied matrix
MakeMatrix <- function(mtx=matrix()) {
  iv <- NULL
  set <- function(val) {
    mtx <<- val
    iv <<- NULL
  }
  get <-function()
  {
    mtx
  }
  setInverse <- function(inverse) {
    iv <<- inverse
  }
  getInverse <- function() {
    iv
  }
  list(set = set, get=get,setInverse = setInverse, getInverse = getInverse )
}
## Return a matrix that is the inverse of 'mtx'
#example of use:
#myMatrix <- matrix(1:6,2,3)
#cacheSolve(MakeMatrix(myMatrix))
cacheSolve <- function(mtr, ...) {
  
  library(MASS)
  iv <- mtr$getInverse()
  if(!is.null(iv)) {
    #print("getting cached matrix")
    message("getting cached matrix")
    return(iv)
  }
  data <- mtr$get()
  iv <- ginv(data)
  mtr$setInverse(iv)
  iv
}