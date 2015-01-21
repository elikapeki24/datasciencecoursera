## These functions will compute the inverse of a square invertible 
##matrix x

## 1st function creates special "matrix" object that can cache its 
##inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    setinv<-function(inverse) m<<-inverse
    getinv<-function()m
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## 2nd function calcs the inverse of the special matrix created above
## If inverse previously calculated, inverse retrieved from cache

cacheSolve <- function(x, ...) {
    m<-x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setinv(m)
    m
}
