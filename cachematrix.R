## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# we have to create a special matrix object that can cache its inverse
# The following function will compute the inverse of Matrix - x


makeCacheMatrix <- function(x = matrix()) 
{
  # Here we start the sub parts in order to find inverse
  
  Findinverse <- NULL
  set <- function(y) 
  {
    x <<- y
    Findinverse <<- NULL
  }
  get <- function() x
  # as per the code we now set and get inverse from the provided finction
  
  # we want the function to accept the matrix and find its inverse
  
  
  setInverse <- function(inverse) Findinverse <<- inverse
  getInverse <- function() Findinverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# looking at the previous code, once the function has already calculated the inverse of the matrix,
# this function will indentify it and will retrieve it. This is from the given Cache

cacheSolve <- function(x, ...)
  
{
  
  Findinverse <- x$getInverse()
  # here we will have to provide an ‘if’ statement to consider the missing NA values and compute
  # the function as per the requirement 
  
  if (!is.null(Findinverse)) 
  {
    message("getting cached data")
    return(Findinverse)
  }
  
  
  F_Matrix <- x$get()
  Findinverse <- solve(F_Matrix, ...)
  x$setInverse(Findinverse)
  Findinverse
  # last part will print out the inverse. 
# in order to make or store values for a matrix
# is the command my_matrix
# in order to get the matrix 
# use the command my_matrix$get()
# in order to get inverse 
# use the command my_matrix$getInverse()
# in order to cachesolve
# use the command cacheSolve(my_matrix)
}

