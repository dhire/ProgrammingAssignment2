## author: Don Hire
## filename: c:\coursera\cachematrix.R 
##
## creates two functions makeCacheMatrix and cacheSolve
## used to return the inverse of a matrix
## logic to use a cached solution (inverse of a matrix) is provided
## in order to increase efficiency 
##
##
##   function makeCacheMatrix accepts a matrix argument and creates a list containing 4 functions 
##    the list created has the follwing 4 elements
##          set_matrix - function that assigns a matrix argument to global environment element in_matrix
##					and assigns NULL to the global environment matrix matrix_inverse
##		get_matrix - function to display matrix element in_matrix
##		set_inverse - function that assigns the matrix argument in_matrix_inverse to global environment element matrix_inverse
##		get_inverse - function to display matrix element in_matrix_inverse
##

makeCacheMatrix <- function(in_matrix = matrix()) {
matrix_inverse <- NULL
        set_matrix <- function(y) {
                in_matrix <<- y
                matrix_inverse <<- NULL
        }
        get_matrix <- function() in_matrix
        set_inverse <- function(in_matrix_inverse) matrix_inverse <<- in_matrix_inverse
        get_inverse <- function() matrix_inverse
        list(set_matrix = set_matrix, 
             get_matrix = get_matrix,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## function cacheSolve returns inverse of matrix
##  required argument x is list element with items get_invervse and get_matrix 
##  (see makeCacheMatrix function)
##  
##  if the matrix inverse does not exist then solve is called to create 
##  the inverse matrix which is cached prior to being returned
##  
##  if the matrix inverse exists the cache value is returned
##

cacheSolve <- function(x, ...) {

matrix_inverse <- x$get_inverse()

        if(!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        data <- x$get_matrix()
	  matrix_inverse <- solve(data, ...)
        x$set_inverse(matrix_inverse)
        return(matrix_inverse)
  
}
