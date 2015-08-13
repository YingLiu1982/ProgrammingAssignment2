## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set function set x to the input matrix, and m to the inverse matrix
## get function returns the input matrix
## setInv set the values of Inv to the inverse matrix m
## getInv get the cached inverse matrix m 

makeCacheMatrix <- function(x = matrix()) 
{
		m <- NULL
		set <- function(y) 
		{
			x <<- y
			m <<- NULL
		}
		
		get <- function() x
		
		setInv <- function(Inv) m <<- Inv
		
		getInv <- function() m
		
		list(
			set = set, 
			get = get,
            setInv = setInv,
            getInv = getInv
			)
}


## Write a short comment describing this function
## firstly retrieve the cached inverse matrix 
## if the results have been cached, retrieve the values and print a message of ("getting cached data")
## if the results are not calculated, calculate the inverse matrix and assign it to m
## cache the inverse matrix using setInv
## return the inverse matrix

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
		m <- x$getInv()
        
		if(!is.null(m)) 
		{
			message("getting cached data")
			return(m)
		}
		
		data <- x$get()
		
		m <- solve(data, ...)
		
		x$setInv(m)
		
		m
}
