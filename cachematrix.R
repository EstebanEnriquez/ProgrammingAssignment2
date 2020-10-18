## The first function called makeCacheMatrix just create a special matrix given
## the argument in the function, here you can also subset the matrix with the
## child functions within makeCacheMatrix, in order to set a new matrix.
## The second function called cacheSolved first checks if "i" has already been
## calculated. If so, it gets the inverse from the cache. Otherwise, it
## calculates the inverse.

## This function makes a list with four child functions(get(),set(),setInv(),getInv())
## It doesn't make any computation, just creates a matrix with a given argument
## and a list where you can set a new matrix or store the calculated inverse
## in the another function

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setInv<-function(inverse) {
    i<<-inverse
  }
  getInv<-function() i
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## First it checks if the inverse has already been calculated, if so, it gives
## you a message and get the inverse from the cache. If not, it calculates the
## inverse, then stores it in setInv() and finally shows you the result. 

cacheSolve <- function(x, ...) {
  i<-x$getInv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setInv(i)
  i
}
