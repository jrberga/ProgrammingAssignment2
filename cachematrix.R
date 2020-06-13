# Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:

# The first function, makeCacheVector creates a special list, which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function (y){
    x <<- y
    inv<<- NULL
  }
  
  get <-function()x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function()inv
  
  list(
    set=set,
    get=get, 
    setinverse=setinverse,
    getinverse=getinverse)
}


#the function above first checks if there is a inverse matrix saved in the cache

#if it exists, so the function gets the result saved in the cache and show it up. 

#But, if that doesnt exist, so the inverse is calculated.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)){
    print("getting cached matrix...")
    return(inv)
  }
  
  mi <- x$get()
  inv <- solve(mi,...)
  
  x$setinverse(inv)
  inv
  
}
## Return a matrix that is the inverse of 'x'

#This is a exemple: 

x1 = rbind(c(1,2), c(0,3))

m = makeCacheMatrix(x1)

m$get()

cacheSolve(m)
cacheSolve(m)