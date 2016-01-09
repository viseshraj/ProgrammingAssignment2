#Function to create spl matrix object to enable environmental place holders
#Description ::
#The functions fun1, fun2 & fun3 hold the pointers to the environment where caching of the inverse matrix is done.
#This logic is based on Lexical Scoping
makeCacheMatrix <- function(squarematrix){
  inverseCache <- NULL
  fun1 <- function() inverseCache     
  fun2 <- function() squarematrix
  fun3 <- function(inverse) {
    inverseCache <<- inverse
  }
  list(fun1=fun1,
       fun2=fun2,
       fun3=fun3)
}

#Function to provide inverse of matrix:
#Description ::
#Check cache for inverse matrix. 
#If not available create inverse matrix and update cache.
#If available in cache, return inverse matrix and exit.
cacheSolve <- function(splmat) {
  #Handling non square matrix
  if (nrow(splmat$fun2()) != ncol(splmat$fun2())) {
    return("The provided matrix is not a square matrix. Hence not invertable.")
  }
  #Handling non invertable matrices
  if (det(splmat$fun2())==0){
    return("The provided matrix is not invertable. Hence out of scope to this function.")
  }
  #Check if inverse is available in cache
  if(is.null(splmat$fun1())){
    print("Inverse Matrix not in Cache. Hence Computing")
    #Compute inverse of matrix
    inverse <- solve(splmat$fun2())
    splmat$fun3(inverse)
    return(inverse)
  }
  #Return inverse matrix
  print("Inverse Matrix from Cache: ")
  splmat$fun1()
}
