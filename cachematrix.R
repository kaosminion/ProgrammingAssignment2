## The below functions will calculate the inverse of a matrix, 
## however if inverse is already in cache it will return the cache

## makeCacheMatrix creates a special matrix object
makeCacheMatrix <- function(x = matrix()) {
  in_x<-NULL ## sets the value of in_x to null initially
  set<-function(y){ #set the value of the matrix
    x<<-y
    in_x<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) in_x<<- solve
  getmatrix<-function() in_x
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## cacheSolve will return the inverse of a matrix, 
## however if the inverse already exists in cache, it will return the cache

cacheSolve <- function(x=matrix(), ...) {
  in_x<-x$getmatrix() # gets the inverse if it has already been calculated
  if(!is.null(in_x)){ # check to see if the matrix was in cache
    message("retrieving cached inverse matrix")
    return(in_x)
  }
  in_x<-solve(x$get(), ...)
  x$setmatrix(in_x)
  in_x
}