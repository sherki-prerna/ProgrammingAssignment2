## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
                  x<<-y
                  inv<<-NULL
                   }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function() {
                      inver<-ginv(x)
                      inver%*%x
                      }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
#this is used for getting the cache data
cacheSolve <- function(x, ...)  ##gets cache data
        
{
  inv<-x$getinv()
  if(!is.null(inv)){     #checking whether inverse is null
                      message("getting cached data!")
                      return(inv) 
  }
  data<-x$get()
  inv<-solve(data,...) ##calculates inverse value
  x$setinv(inv)
  inv  ## Return a matrix that is the inverse of 'x'
}
  
