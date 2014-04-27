## Put comments here that give an overall description of what your
## functions do
## Below R function is a demostration of how to calculate the inverse of matrix and use the cache to reduce the recursive computing time by using the inverse from the cache value assuming that the matrix remains the same.

## Write a short comment describing this function

##Function makeCacheMatrix()  creates a special vector, which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {


	# initialize value of inverse matrix
        inversematrix = NULL
        
        # set value of matrix from user input y
        set <- function( y ) {
                
                # sets to global x value of matrix input y 
                x <<- y
                
                # clears inversematrix value if it exists
                inversematrix <<- NULL
        }
        
        # get value of matrix if it has been set
        get <- function() x
        
        # set value of the inverse of matrix
        setinverse <- function(solve) inversematrix <<- solve
        
        # get value of the inversematrix
        getinverse <- function() inversematrix

        list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function
## Function cachesolve calculates the inverse of the special matrix created using the above function. It first checks if the inverse is already calculated.If yes, then it would use and get  inverse from the cache else would calculate the inverse and then set the value of the inverse using setinverse.	  


cacheSolve <- function(x, ...) {

	## get value of inverse
        inversematrix <- x$getinverse()
        
        ##check to see if inverse is already computed
        if( !is.null( inversematrix ) ) {
                message("getting cached data")
                
               ##if inverse is cached, get the cached value and return
                return(inversematrix)
                
        }
        
        ##otherwise calculates inverse of data
        invertiblematrix <- x$get()
        inversematrix <- solve(invertiblematrix, ...)
        
        ##set mean to cached value
        x$setinverse(inversematrix)
        
        ## Return a matrix that is the inverse of 'x'
        return(inversematrix)

}	
