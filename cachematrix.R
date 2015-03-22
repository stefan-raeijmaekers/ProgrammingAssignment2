## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Global information. The functions 'cacheSolve' and 'makeCacheMatrix' will make it possible to calculate the inverse of a  +  
## given matrix and 'cache' it so it will not be necessary to calculate it again if the inverse of the same matrix is needed +
## again (thus saving computation time). For more detailed explanations, cfr. below.                                         +
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## The functon 'makeCacheMatrix' take a matrix x as its input argument and return a list containing
## four functions. The functions will make it possible to provide a new non-inverted matrix to replace the original one,
## to get the matrix currently stored in the variable x, to set the value of the variable 'inv' which either holds the
## value 'NULL' or the inverted matrix once this has been calculated and get the value of the variable 'inv'. 

makeCacheMatrix <- function(x = matrix()) {
  
  ## This variable will be used to store the inverted matrix once it has been calculated. Initially it is given the value 'NULL'.
  
  inv <- NULL
  
  ## This function will be used to set the value of x in the parent environment of setMatrix (i.e. makeCacheMatrix). It will
  ## require a new matrix as its input argument. The first line of the function will assign the variable newMatrix to x 
  ## (x being an existing variable in the parent environment). Subsequently the variable 'inv' (also part of the parent 
  ## environment of setMatrix) will be set to 'null' since the inverse of the newly provided matrix has not yet been calculated. 
  
  setMatrix <- function(newMatrix) {
    x <<- newMatrix
    inv <<- NULL
  }
  
  ## This function will be used to return the value of x (i.e. the original (non-inverted) matrix). Since x is not defined in the
  ## function getMatrix itself it will first be looked up in the parent environment where this function is defined (makeCacheMatrix).
  
  getMatrix <- function() {
    return(x)
  }
  
  ## The following function will be used to set the value of the variable 'inv' (which is also part of the parent environment of
  ## setInverse). The value that is assigned to inv is to be given as an input argument (here named 'inverse') of the function. 
  
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## The following function will return the value of inv. Analogous to getMatrix, the variable inv is part of the parent environment
  ## of getInverse.
  
  getInverse <- function() {
    return(inv)
  }
  
  ## This list contains the four functions mentioned above. Since it is the last line of the function makeCacheMatrix, the list will
  ## be the return value of this function. 
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)

}

## The function 'cacheSolve' requires the list genreated by makeCacheMatrix as its input argument. Depending on whether the inverse of the
## matrix in the parent environment of x has already been calculated it will either print a message and return the existing inverted matrix
## or calculate and return a newly calculated inverted matrix. 

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  ## The if condition will check whether or not the value of the variable inv is equal to NULL. If this is not the case (the ! operates
  ## as a logical NOT) the code in the if-statement will be executed (i.e. the message will be printed and the value of inv will be returned). 
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If the value of inv is not equal to "NULL", the code in the else-statement will be executed. First, the function 'getMatrix' (an 
  ## element of the list x) is called. This function will retrieve the original matrix from its parent environment and assign it to 
  ## the variable currentMarix. Subsequently the function solve is called to compute the inverse of the original martrix and assign 
  ## it to the variable inv. The result of this computation is passed as an argument to the function setInverse (again an element of 
  ## list x) which will set the value of "inv" in its parent environment to the computed inverted matrix. In this way, once we call 
  ## the function cacheSolve on the same list again, the statement in the if-condition will evaluate to TRUE and the code in the 
  ## if-statement will be executed (and the inverse of the matrix will not have to be computed again). Finally, the value of the 
  ## variable inv is returned. 
  
  else {
    currentMatrix <- x$getMatrix()
    inv <- solve(currentMatrix, ...)
    x$setInverse(inv)
    return(inv)
  }
}
