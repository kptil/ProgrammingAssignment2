## Programming Assignment 2: Lexical Scoping
## Author: kpt12@georgetown.edu
## Date: 8 March 2019

## The below functions utilize R's Lexical Scoping rules to cache the value of
## a matrix and its inverse within the parent environment. This allows subsequent 
## functions to use the the inverse without having to recalculate it each time, 
## which can take a lot of time if the inverse calculation is complex. The first
## function, makeCacheMatrix, takes your original matrix as its argument and sets
## up a structure of functions and variables that cacheSolve then uses to calculate
## and store the inverse in the parent environment. cacheSolve does the actual
## inverse calculation. This works because when R encounters an undefined variable
## or function within a function it is running, it looks up that variable or 
## function within the parent environment, according to the rules of Lexical Scoping.

## Because in R every function includes a pointer to the parent environment in
## which is was defined, you can nest functions and free (undefined) variables
## within another function, and R will pull their definitions from the parent
## environment. If you have variables that are defined within nested functions,
## and so would not be automatically saved to the parent environment and made 
## accessible to subsequent functions, you can save them to the parent environment 
## using the <<- operator. 

## The makeCacheMatrix function takes a matrix as it's argument and creates an empty
## variable called i to be filled later with the matrix's inverse. The set function
## then saves the matrix (x) and the i variable to the parent environment using the
## <<- operator. This ensures subsequent functions can pull the values stored in x 
## and i from the parent environment without having them defined in the subsequent
## function. It also allows you to reset the x matrix by inputting a new matrix as
## the function's y argument. The get function returns x, the original matrix. The
## setinverse function sets i equal to 'inverse', the function argument, which you
## will define as the inverse of matrix x in the cacheSolve function. This is the
## part that fills the empty i variable with calculated inverse and stores it in 
## the parent environment, but the actual calculation happens in the cacheSolve
## function. The getinverse function returns i. Lastly, makeCacheMatrix combines
## all these functions into a list and gives them names. Because it returns a list,
## you can save the makeCacheMatrix result into a variable and then call any of the
## four functions defined wihtin it using the $ operator and regular subsetting rules.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve's first argument is the input from the makeCacheMatrix function,
## which you can save as any variable, here represented by x. Again, x is a list
## with the four functions saved as named elements, so you can use the $ operator
## to call the value of the getinverse function and save it to the local variable
## i. The getinverse function just returns the value of is as it was saved to the
## parent environment in the makeCacheMatrix function. This should be NULL, unless
## you have already run the cacheSolve function and have a matrix's inverse saved
## in i. The following if statement returns that saved value of i if i is not NULL.
## If i is NULL, cacheSolve calculates the inverse of the original matrix by first
## retrieving it using x$get(), storing the matrix in local variable data, and then
## running the solve function on data, which gives you the inverse of the matrix. 
## cacheSolve then stores the inverse as i in the parent environment by passing the
## local variable i to the set inverse function as defined in makeCacheMatrix. The
## function then returns i.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
