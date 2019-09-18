## converts matrix so that calculations are cacheable
## returns a list of functions to set the matrix, get the matrix, set the inverse
##allows you to check whether an inverse has already been computed

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setinv <- function(inv) i<<-inv
  getinv <- function() i
  list (set=set,get=get,setinv=setinv,getinv=getinv)

}


##checks if inverse for that input has already been calculated (by seeing if it is still NULL)
##if already calculated, prints message and returns the cached inverse
##otherwise it retrieves the matrix, solves the inverse, sets the inverse value for that particular input, and returns it

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)){
    message("retrieving cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data,...)
  x$setinv(i)
  i
}
