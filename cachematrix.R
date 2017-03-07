## makeCacheMatrix will return a named list with the four functions 
## and 2 initiated variables, x (a matrix) and m (x's inverse)

makeCacheMatrix<- function(x=matrix()){
    m<-NULL
    set<- function(y){
        x<<- y
        m<<- NULL
    }
    get<-function()x
    setinverse<- function(inverse) m<<- inverse
    getinverse<- function () m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve takes the special list from makeCacheMatrix
## that included the four functions and to variables needed to 
## execute the solving of the matrix inverse and the caching of that inverse
## cacheSolve uses a logic argument to check if an inverese matrix
## has already been calculated, in which case it'd return that value
## otherwise it runs the operation solve() on the matrix from makeCacheMatrix

cacheSolve<- function(x, ...){
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<- solve(data,...)
    x$setinverse(m)
    m
}

## cacheSolve sets the solved matrix to setinverse 
## returns the inverse matrix m
