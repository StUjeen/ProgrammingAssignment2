## Put comments here that give an overall description of what your
## functions do:
## The first function "makeCacheMatrix" creates a matrix defined as incoming parameter
## and a list of functions which allow to manipulate with the matrix 
## you defined and also it stores cached value of
## an inverse matrix in case it is applicable
## The second one "cacheSolve" takes the income matrix and does eihter calculation of inverse or
## call of the function from the list to get and turn back cached inverse 

## This function creates the list of functions

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  setmtrx <- function (mtrx) {
    x <<- mtrx
    inv <<- NULL
  }
  
  getmtrx <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function () inv
  list(setmtrx = setmtrx
       ,getmtrx = getmtrx
       ,setinv = setinv
       ,getinv = getinv)
  #inv
}


## this function turns back an inverse of matrix created using first function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  ## check if invertible matrix exists in cache if so turns back inverse from cache
  if (!is.null(inv)) {
    message("cached inverse matrix:")
    return(inv)
  }
  ## check if new matrix is invertible if no then turns back message otherwise
  ## calculates the inverse
  else {
    mat <- x$getmtrx() 
    inv <- solve(mat, ...)  
    x$setinv(inv)
    inv
    }
  }






