# I Anju Kamra wrote this code

AnjuCacheMatrix <- function(x = matrix()) {
        i <- NULL ## creating a blank matrix to store Inverse
        set <- function(y) {
                x <<- y  ## Assigning new matrix to x using set
                i <<- NULL
        }
        get <- function() x
        ##Storing the inverse in i after it is calculated
        setsolve <- function(solve) i <<- solve 
        
        ##Caching value from cache if available
        getsolve <- function() i 
        
        ##Store the values in a list and return the list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function calculates the inverse of a matrix. If cached value of inverse is available it gets
## it from there else it calculates
cacheSolve <- function(x, ...) {
        i <- x$getsolve() #Check if matrix x is cached
        if(!is.null(i)) {
                message("getting cached data") #If the matrix x is cached than get the cached value
                return(i)
        }
        data <- x$get() # We get it If matrix is not cached and caclculate the inverse
        i <- solve(data, ...) 
        x$setsolve(i) #Caching the Inverse of the matrix that was created above
        i
}

##Test Cases
x <- makeCacheMatrix(matrix(1:4,2))
z<-x$get() ##Get the matrix created
z

cacheSolve(x) ## Getting the inverse
cacheSolve(x) ## Checking if cached inverse is returned

#Testing if inverse is correct
y<-x$getsolve()
y %*% z ## Should z be identity matrix