## Programming assignment no. 2 for R programming course on Coursera

## The below function creates the list object that contains the original matrix and it's inverse (if previously calculated)

makeCacheMatrix <- function(x = matrix()) {
        inverse_Matrix <- NULL   ## Initialize inverse value for checking in cacheSolve
        set <- function(y) {     ## This "set" attribute isn't really used in this example
                x <<- y          ## ...but is a useful attribute if you want to send a new matrix to the function
                inverse_Matrix <<- NULL
        }        
        get <- function() x      ## Set the "get" attribute equal to the input matrix
        set_inv <- function(solved_matrix) inverse_Matrix <<- solved_matrix  ## used to set the inverse matrix value to a supplied value
        get_inv <- function() inverse_Matrix    ## set the "get_inv" attribute equal to the invert_Matrix object
        list(   set = set,                      ## Set the list attributes equal to the functions mentioned above
                get = get,
                set_inv = set_inv,
                get_inv = get_inv)
}


## The below function checks if the supplied object (list created above) has a cached inverse, and calculates it if not

cacheSolve <- function(x=matrix(), ...) {
        inverse_Matrix <- x$get_inv()   ## Get whatever is stored as the inverse_Matrix in makeCacheMatrix function
        if(!is.null(inverse_Matrix)){   ## if the "get_inv" attribute is not empty (null)
        message("getting cached data")  ## ... Then let the user know that the inverse is cached
        return(inverse_Matrix)          ## ... and return the cached inverse matrix
        }
        input_matrix <- x$get()                 ## If the get_inv value was null instead, 
        invMatrix <- solve(input_matrix, ...)   ## ... invert the matrix stored under the "get" attribute
        x$set_inv(invMatrix)                    ## and store this value in the "set_inv" attribute
        invMatrix                               ## Return the inverted matrix
}
