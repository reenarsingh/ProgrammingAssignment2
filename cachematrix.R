#R Programming - Week 3 Assignment to calculate, cache and retreive the inverse of a matrix

#function to cache the matrix and the inverse of the matrix
makeCacheMatrix <- function(origMatrix = matrix()) {
  origgMatrix <- NULL
  inverseMatrix <- NULL
  
  #set the MAtrix
  setMatrix <- function(matrx) {
    origgMatrix <<- matrx
  }
  
  #get the matrix
  getMatrix <- function()origMatrix
  
  #setInverseMatrix
  setInverseMatrix <- function(invMatrx) {
    inverseMatrix <<- invMatrx
  }
  
  setMatrix(origMatrix)
  #get the inverse MAtrix
  getInverseMatrix <- function()inverseMatrix
  
  #return a list of all the functions
  
  list(setMatrix = setMatrix,  getMatrix =  getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

#function to retrieve the invers of the matrix dromthe cache if it exists, calculate it otherwise
cacheSolve <- function(functionObject,...){
  inverseMatrix <- functionObject$getInverseMatrix()
  origMatrix <- functionObject$getMatrix()
  if(!is.null(inverseMatrix)){
    print("Retreiving Cached Inverse for Matrix")
    return(inverseMatrix)
  }
  else
  {
    print("calculating new Cached Inverse for Matrix")
    functionObject$setInverseMatrix(solve(origMatrix,...))
    return(functionObject$getInverseMatrix())
  }
}


