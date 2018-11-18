# Programming assignment week 3: Caching the Inverse of a Matrix - November 18, 2018

#Step 1: Write function to create a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {               #Line 5 defines type of data input. Here matrix
    inver <- NULL                                         #Line 6 sets default value of inver as NULL = undefined. Will hold value of inversed Matrix
    set <- function(y) {                                  #Line 7-8 define set function to assign value of Matrix
        x <<- y
        inver <<- NULL                                    #Line 9 resets inver to NULL when a new Matrix is entered
    }
    get <- function() x                                   #Line 11 define get function to return value of Matrix 
    setinverse <- function(inverse) inver <<- inverse     #Line 12 define setinverse function to assign value of inver
    getinverse <- function() inver                        #Line 13 define getinverse function to return value of inver
    list(set = set, get = get,                            #Line 14-16 make defined functions callable by $ operator
         setinverse = setinverse, 
         getinverse = getinverse)
}

#Step 2: Write function to compute the inverse of the object cached by makeCacheMatrix

cacheSolve <- function(x, ...) {                          #Line 21 defines type of data input. Here object from makeCacheMatrix
    inver <- x$getinverse()                               #Line 22 assign inver to use getinverse function on object from makeCacheMatrix
    if(!is.null(inver)) {                                 #Line 22-23 return the value of inver if inver is not NULL
        return(inver)
    }
    data <- x$get()                                       #Line 26 assign data tu use value of Matrix in object from makeCacheMatrix
    inver <- solve(data, ...)                             #Line 27 assign inver to solve Matrix from object makeCacheMatrix by getting its inverse value
    x$setinverse(inver)                                   #Line 28 define value of setinverse for object from makeCacheMatrix as inver calculated in Line 27
    inver                                                 #Line 29 print inver
}
