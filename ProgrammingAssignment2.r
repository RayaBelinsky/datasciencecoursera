# this function creates a list 
# that contains 4 functions: set, get, setmatrix and getmatrix.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL # this is where the result of inversion is stored
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x # return the input matrix
  setmatrix<-function(solve) m<<- solve # set the inversed matrix
  getmatrix<-function() m # return the inversed matrix
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix() # get the inversed matrix from object x
  if(!is.null(m)){ # if the inversion result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  matrix<-x$get() # if not, we do x$get to get the matrix object
  m<-solve(matrix, ...) # solve it
  x$setmatrix(m) # set it to the object
  m # return the solved result
}
