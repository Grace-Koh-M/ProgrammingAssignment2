## Put comments here that give an overall description of what your
## functions do

## The two functions in this assignment are: (i) makeCachematrix (ii) cacheSolve
## makeCachematrix includes set, get, setinv, getinv
## inverse for squared and non-squared matrices are calculated using MASS package

library(MASS)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL        #initializing inverse as NULL
    set <- function(y){
                      x<<-y
                      inv <<- NULL
                      }
    get <- function()x      #function to get matrix x
    setinv <- function(inverse)inv <<- inverse
    getinv <<- function(){
                          inver<-ginv(x)
                          inver%*%x       #function to obtain inverse of the matrix
                          }
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## Write a short comment describing this function
## The following is used to get the cache data

cacheSolve <- function(x, ...)  #get cache data
    {
    inv <- x$getinv()
    if(!is.null(inv)){               #check whether inverse in NULL
                      message("getting cached data!")
                      return(inv)             #returns inverse value
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv     ## Return a matrix that is the inverse of 'x'
}
