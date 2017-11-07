## Put comments here that give an overall description of what your
## functions do
## -----------------------------------------------------------------------------------------
## The function 'makeCacheMatrix' works along with 'cacheSolve'.
## To test the functions first a matrix needs to be created like the example below:
## Ej. a<-matrix(data=c(2,3,4,5,6,7,8,0,1),nrow=3,ncol=3,byrow=TRUE)
## Then another function is created (Ej, b) which is a parent function of 'makeCacheMatrix'
## and using the  matrix (a) as an argument. This function has the same attributes as
## 'makeCacheMatrix'. Ej. b<-makeCacheMatrix(a)
## Then the function 'cacheSolve' can be executed using the new function (b) 
## as argument and returning the inverse.
## If this function (b) is called again to re-calculate the inverse, then the 
## value of the inverse will be get from cached data through the function 'getinv'.
## As the variables set, get, setinv and getinv are defined as a list at the end of
## makeCacheMatrix, then these can be called using $ to either obtain the matrix, the inverse or
## set a new matrix.

## Write a short comment describing this function
## -------------------------------------------------------------------------------------------
## 'makeCacheMatrix' is used to obtain the matrix which inverse will be calculated.
## This function also allowed to input a new matrix using 'set' function. The 'set' function is
## called from 'cacheSolve' to set a matrix and then using 'setinv' and 'getinv' to cache 
## the inverse if the inverse has not been previously calculated. 
## The functions 'set', 'setinv' and 'getinv' works with 'x' and 'im' setup in a different
## environment which is achieved using <<- and leaving the argument outside the functions 'get',
## 'setinv' and 'getinv'. This way the value of 'x' and 'im' used by these functions are the ones 
## of this second environment and not the main one.
## It is noticed the inverse of the matrix which is obtained through 'im' is initialized as 
## NULL and changed through the function 'setinv'.

makeCacheMatrix <- function(x = matrix()) {
    im<-NULL #Initialize the inverse matrix as NULL
        set<-function(y){ #Obtain a new entry for the matrix
        x<<-y
        im<<-NULL
    }
    get<-function() x 
    setinv<-function(solve) im<<-solve
    getinv<-function() im
    list(set = set, get = get,setinv = setinv,getinv = getinv)
}

## Write a short comment describing this function
## The function 'cacheSolve' can be executed using a parent function of 'makeCacheMatrix' as
## an argument (Ej, matrix b). Then the inverse 'im' is returned calling 'getinv.
## 'cacheSolve' then verified if im is NULL or not, getting 'im' from 'getinv'. If NULL, then the
## inverse matrix 'im' is calculated using 'solve' function and cached using the function 'setinv', 
## otherwise im is directly obtained from 'getinv' as cached and printing the message
## "getting cache data". 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im<-x$getinv()
        if(!is.null(im)){
        message("getting cached data")
        return(im)
        }
    data<-x$get()
    im<-solve(data,...)
    x$setinv(im)
    im
}
