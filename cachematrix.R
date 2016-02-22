## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The following function creates a special "matrix" object
#that can cache its inverse. First a empty dummy matrix is created. Then the set function for the original 2x2 matrix is implemeted, followed by the simple print/return function get.
# The setMatrix function assigns the incoming matrix "inmatrix" to the cachematrix variable in the functions environment. getInverse is the according read-out function.

makeCacheMatrix <- function(x = matrix()) {
        cachematrix <- NULL
        
        set <- function(y) {
                x <<- y
                cachematrix <<- NULL
        }
        
        get <- function() x

        setMatrix <- function(inmatrix) cachematrix <<- inmatrix

        getInverse <- function() cachematrix
        
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}


## Write a short comment describing this function
# This function checks if the matrix is invertible (tryCatch) and throes an error if not or a warning. It first tries to load the inverse of the loaded matrix 
# with the getInverse() function defined in makeCacheMatrix. If the inverse already exists, it is not computed again but just read. Otherwise the invere is computed.
cacheSolve <- function(x, ...) {
        ## attempt to get the inverse of the matrix stored in cache
        cacheInverse <- x$getInverse()
        
        # return inverted matrix from cache if it exists
        # else create the matrix in working environment
        if (!is.null(cacheInverse)) {
                return(cacheInverse)
        }
        
        # create matrix since it does not exist
        matrix <- x$get()
        
        # make sure matrix is square and invertible
        # if not, handle exception cleanly
        tryCatch( {
                # set and return inverse of matrix
                cacheInverse <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)
                
                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)
                
                return(NA)
        },
        finally = {
                # set inverted matrix in cache
                x$setMatrix(cacheInverse)
        } )
        return (cacheInverse)
        }
