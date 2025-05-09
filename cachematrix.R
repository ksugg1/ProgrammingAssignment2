## Here I am creating a matrix, and caching it. We are setting the value of the
##matrix, getting the value of the matrix, setting the value of the inverse, 
##and then getting the value of the inverse matrix. Because all the information 
##is cached, the idea is that it will not need to be recalculated each time.




## We are setting a function that will take any matrix we give it and cache certain
##elements to be called more easily and with less effort on R's part

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) s<<-solve
  getsolve<-function() s
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Here we are returning a matrix that is the inverse of 'x', but it it has
##already been calculated, we are retrieving the answer from memory rather than
##re-running it

cacheSolve <- function(x, ...) {
       s<-x$getsolve()
       if(!is.null(s)){
          message("getting cached data")
         return(s)
       }
       data<-x$get()
       s.calculated<-solve(data, ...)
       x$setsolve(s.calculated)
       s.calculated #return the inverse matrix
}

