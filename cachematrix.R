##Jonathan Sessom

##This function Function makeCacheMatrix gets a matrix as an input, sets the value of the matrix,
#gets value of the matrix, sets the inverse Matrix and gets the inverse Matrix. The matrix object
#can cache its own object. 

##take the matrix as an input

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
    set <- function(y) 
      {
        x <<- y
        inv <<- NULL
      }
      get <- function() x
    setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)

}


## The function cacheSolve takes the output from above matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) to see if it has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# sets the invertible matrix by using solve function.
# If inverse matrix from makeCacheMatrix((matrix) has some value in it (will always work
#after running the code one time), it returns a message  "cached data" 
#and the cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
    if (!is.null(inv)) {
        message(" cached data")
        return(inv)
       }
     mat <- x$get()
     inv <- solve(mat, ...)
     x$setInverse(inv)
     inv
}



mymatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
 
    mymatrix$get()
    
mymatrix$getInverse()

cacheSolve(my_matrix)

mymatrix$getInverse()

mymatrix$set(matrix(c(2, 2, 1, 4), 2, 2))

mymatrix$get()

mymatrix$getInverse()

cacheSolve(my_matrix)

mymatrix$getInverse()

mymatrix$get()

