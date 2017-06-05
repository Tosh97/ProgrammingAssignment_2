## Generates the inverse from a matrix

makeCacheMatrix <- function(m = matrix()) {
  ##Begins inverse property
  j<- NULL
  
  ##Setting matrix with user method
  set<-function(matrix){
    m<<-matrix()
    j<<-NULL
  }
  
  ##Getting the Matrix
  get<-function() {
    m
  }
  
  ##Setting inverse with user method
  setInverse<-function(inverse) {
    j<<-inverse
  }
  
  ##Getting inverse with user method
  getInverse<-function() {
    j
  }
  
  #Returnning list of methods
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Computes inverse based off of matrix returned by "makeCacheMatrix"
## If already calculated, will retrieve inverse from cache
cacheSolve <- function(x, ...) {
  ##Returns inverse of x as a matrix
  m<-x$getInverse()
  
  ## If already set, returns inverse
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ##Get the matrix from object
  data<-x$get()
  
  ##Calculate the inverse with matrix multiplication
  m<-solve(data, ...)
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ##Return the matrix
  m
}
