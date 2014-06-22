## Put comments here that give an overall description of what your
## functions do

##Hete the function makeCacheMatrix contains a list of functions to set 
##      and get the matrix as well .
## The funtion cacheSolve() is used to compute and return the inverse of matrix
## The getinverse() function gets the inverse from cacheSolve 
##and the setinverse() cunctions sets the value of matrix inverse


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      
      m <- NULL
      set <- function(y) {   #to set the value of the matrix
            x <<- y
            m <<- NULL
      }
      get <- function() x     #to get the value of the matrix
      
      setinverse <- function(mean){    #to set the value of inverse of the matrix
            m <<- mean
      }
      
      getinverse <- function() {       # to get the value of the inverse of the matrix
            m
      }
      list(set = set, get = get, setinverse=setinverse,    #return the list of functions
           
           getinverse = getinverse)
      
      

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse() #query the x vector's cache 
      if(!is.null(m)) {     #if there is a cache then
            message("getting cached data")
            return(m)       #just return the cache,no computation required
      }
      data <- x$get()      #if no cache is found
      m <- solve(data, ...)    #compute here for inverse
      x$setinverse(m)         #return the result
      m
      
      
}

