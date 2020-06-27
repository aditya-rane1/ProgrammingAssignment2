## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse and creates a list of functions.

makeCacheMatrix<-function(x=matrix())
{
   i<<-NULL
   
   setmatrix<-function(v)
  {
    x<-v
    i<<-NULL
  }
  
   getmatrix<-function()
  {
    x
  }

   setinverse<-function(inverse)
  {
    i<<-inverse
  }
  
   getinverse<-function()
  {
    i
  }
  list(setmatrix=setmatrix,getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)

} 
## Function below calculates the inverse of a matrix object not having inverse and stores in its cache or retrieves the inverse from cache of matrix object

cacheSolve<-function(x)
{
  if(is.null(x$getinverse()))
  {
    m<-x$getmatrix()
    i<-solve(m)
    x$setinverse(i)
    i
  }
  else
  {
    print("Retrieving inverse from Cache")
    return(x$getinverse())
  }
}
