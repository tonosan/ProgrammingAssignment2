## This two function store in a different environment a matrix 
##and a list of functions and then apply functions to the matrix
##and display the results


## Creates a closure an enclosing for de parent function and 
## allow access to all its variables

makeCacheMatrix <- function(x=matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
   }
  get<-function() x
  setmat<-function(solve) m<<-solve
  getmat<-function() m
  list(set = set,get = get,
       setmat = setmat,
       getmat = getmat)
}


## This parent environment of the closure is the execution 
## environment

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getmat()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setmat(m)
  m
}
