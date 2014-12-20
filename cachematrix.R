#    R Functions to Find Inverse of a Square Matrix
# 
#
#  Programming Assignment 2 for
#
#  Coursera Data Scientist's Toolbox specialization program
#
#  R Programming Course
#
#  Dec. 20, 2014
#
#  Submitted by Tom Merklein   tmerk@sonic.net
#
#
#  Comments below are tutorial in nature and would not be used
#  in production code.
#
#
#
#  Inverse Matrix - Simple Definition
#  ----------------------------------
#
#  The inverse of a matrix is one such that, if you mulitply 
#  the original matrix by its inverse, you get the identity
#  matrix.
#
#  Example:
#
#     Square        Inverse of      Identity
#     Matrix         Matrix          Matrix
#
#      --  --       --      --       --  --
#     | 4  7 |  x  |  .6  -.7 |  =  | 1  0 |
#     | 2  6 |     | -.2   .4 |     | 0  1 |
#      -- ---       --      --       --  --
#
#
#  Code Overview
#  -------------
#  makeCacheMatrix
#
#     This function is used to define and store the input matrix
#     and later, the computed inverse matrix.
#
#     It consists of (and returns) 4 functions that are used by 
#     the R Studio user and the other function, cacheSolve:
#
#        set         - Can be used to define the input matrix
#                      (may not necessarily be used; see examples below).
#
#                      Uses the <<- assignment operator to make 
#                      the input matrix accessible to the other
#                      functions in makeCacheMatrix.
#
#        get         - Returns either the original input matrix
#                      or the computed inverse matrix
#                      (depending on whether the inverse computation
#                      has been made)
#
#        getinverse  - Does *not* compute the inverse matrix.
#
#                      It returns the value of the matrix stored
#                      in makeCacheMatrix.
#
#                      getinverse is called by cacheSolve *before*
#                      cacheSolve computes the inverse matrix.
#
#                      At the first call to cacheSolve, 
#                      the makeCacheMatrix matrix is NULL,
#                      and this triggers cacheSolve to compute the 
#                      inverse matrix.
#
#        setinverse  - Does *not* compute the inverse matrix.
#
#                      setinverse is called by cacheSolve *after* 
#                      cacheSolve has computed the inverse matrix.
#
#                      setinverse uses the <<- assignment operator to
#                      make the inverse matrix accessible to the other
#                      functions in makeCacheMatrix.
#
#
#  cacheSolve
#
#    This function computes the inverse matrix.
#
#    It calls makeCacheMatrix/getinverse which returns makeCacheMatrix's
#    copy of the inverse matrix.
#
#    On the first call, this inverse matrix will be NULL, so cacheSolve 
#    uses the R 'solve' function to compute the inverse matrix.
#
#    It then calls makeCacheMatrix/setinverse to store the inverse
#    matrix.
#
#    On subsequent calls to cacheSolve, the call to makeCacheMatrix/getinverse
#    will *not* return a NULL matrix -- it will return the stored
#    inverse matrix.
#
#    cacheSolve then just returns the inverse matrix (vs. recomputing).
#
#
#
#  Sample Useage 1:
#  ---------------------------
# 
#  > source("cachematrix.R")
# 
#  > amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#
#  > amatrix$get()
#
#  > cacheSolve(amatrix)
#
#  > amatrix$getinverse()
#
#  > cacheSolve(amatrix)    # Returns cached matrix inverse using 
#                           # previously computed 
#                           # matrix inverse
#
#
#  Sample Useage 2:
#  ---------------------------
#  > amatrix = makeCacheMatrix()
#  > amatrix$set(matrix(c(1,2,3,4), nrow=2, ncol=2))
#
#  > amatrix$get            # You *must* use the parantheses
#                           #  See comments at bottom
#  > amatrix$get()
#  > cacheSolve(amatrix)
#  > cacheSolve(amatrix)    # Returns previously-computed matrix
#
#
#
#   More detailed sample output is at the bottom of this file.



makeCacheMatrix <- function(x = matrix()) 
{
    #  Initialize 'm' to NULL on the first call to makeCacheMatrix.  
 
    #  'm' is local to makeCacheMatrix; also see '<<-' comments below. 
    
    m <- NULL
    
    
    #  The next 4 functions are not executed until called by the
    #  the R Studio user or cacheSolve below.


    #  The set function can be used to input the original matrix; 
    #  however, it does not have to be called from the R studio
    #  prompt -- the makeCacheMatrix function can used to set 
    #  the input matrix (see examples above and below).

    set <- function(y)
    {
      #  Use the <<- operator to make 'x' and 'm' accessible 
      #  to the other makeCacheMatrix functions
     
      #  Set 'x' to be the input matrix.
 
      x <<- y
      m <<- NULL   # Return value
    }

 
    #  The get function *always* returns the original input matrix
    #  (done by set function above).
 
    get <- function() 
    {
        x      # Return value
    }

    #  Called from the bottom of cacheSolve function.
    #  This is what saves the inverse matrix values
    #  in 'm' which, via the  <<-  operator, is 
    #  accessible by the other makeCacheMatrix functions.
    
    setinverse <- function(inverse) 
    {
        m <<- inverse   # Return value
    } 

    #  Called by cacheSolve.
    #  'm' is not passed into this function because
    #  it's already accessible by both functions.
    #
    #  The first time it's called, 'm' is NULL
    
    getinverse <- function() 
    {
        m     # Return value
    }
 
    
    #  makeCacheMatrix returns a list of functions.
 
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



#  cacheSolve returns a matrix that is the inverse of the 'x' matrix.

cacheSolve <- function(x, ...) 
{
    #  The first time cacheSolve is called, 'm' is NULL.
    #  That's because the initial call to makeCacheMatrix sets
    #  it to NULL.
    #  That's what gets returned by makeCacheMatrix/getinverse.
    #
    #  On subsequent calls to cacheSolve, 'm' has already been
    #  inverted, so in that case, m is not NULL.
    
    m <- x$getinverse()

    #  If the inverse matrix has already been calculated, 
    #  just return the inverse matrix values.
    
    if (!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }

    #  If the inverse matrix needs to be calculated,
    #  copy the 'm' matrix to the 'data' matrix.
    
    data <- x$get()
    
    #  The "solve" function computes the inverse of the 'data' matrix.
    #  Save in 'm' which is local to cacheSolve.
    
    m <- solve(data, ...)
 
    #  Save the inverse matrix in the makeCache's 'm' matrix.
    
    x$setinverse(m)
    
    return (m)

}

# _______________________________________________________________________
#
#  Sample runs to show actual output of these functions
#
#
#  Sample run #1:
#  ---------------------------
#
#  > source("cachematrix.R")
# 
#  > amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#
#  > amatrix$get()         # Returns original matrix
#        [,1] [,2]
#  [1,]    1    3
#  [2,]    2    4
# 
#   > cacheSolve(amatrix)  # Computes, caches, and returns matrix inverse
#        [,1] [,2]
#  [1,]   -2  1.5
#  [2,]    1 -0.5
#
#  > amatrix$getinverse()  # Returns matrix inverse
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
#  > cacheSolve(amatrix)   # Returns cached matrix inverse using 
#                          # previously computed 
#                          # matrix inverse
#  getting cached data
#        [,1] [,2]
#  [1,]   -2  1.5
#  [2,]    1 -0.5
#
#
#  Sample run #2:
#  ---------------------------
#
#  > bmatrix = makeCacheMatrix()
#
#  > bmatrix$get                  # You *must* use the parantheses
#  function() 
#      {
#          x      # Return value
#      }
#  <environment: 0x42db7e8>
#
#  > bmatrix$get()                # bmatrix has not been set, yet
#       [,1]
#  [1,]   NA
#
#  > bmatrix$set(matrix(c(1,2,3,4), nrow=2, ncol=2))
#
#
#  > bmatrix$get()                # Return original, input matrix
#       [,1] [,2]
#  [1,]    1    3
#  [2,]    2    4
#
#  > cacheSolve(bmatrix)          # First call computes inverse matrix
#       [,1] [,2]
#  [1,]   -2  1.5
#  [2,]    1 -0.5
#
#  > cacheSolve(bmatrix)
#  getting cached data            # Subsequent calls return the 
#       [,1] [,2]                 # previously-computed inverse
#  [1,]   -2  1.5                 # matrix
#  [2,]    1 -0.5
#  > 
#
#  Sample run #3:
#  ---------------------------
#
#  > amatrix = makeCacheMatrix(matrix(c(3, 2, 0, 0, 0, 1, 2, -2, 1), nrow=3, ncol=3))
#
#  > amatrix$get()
#  [,1] [,2] [,3]
#  [1,]    3    0    2
#  [2,]    2    0   -2
#  [3,]    0    1    1
# 
#  > cacheSolve(amatrix)
#  [,1] [,2] [,3]
#  [1,]  0.2  0.2    0
#  [2,] -0.2  0.3    1
#  [3,]  0.2 -0.3    0

