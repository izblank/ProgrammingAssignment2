## Two functions to illustrate the concept 
## of lexical scoping and how it can be used to implement caching in R
##  Sample usage is:
##
##  m<- matrix(1:4,2,2)
##  cm<- makeCacheMatrix(m)  #creates an environment with a copy of "m"
##  cacheSolve(cm, demomode=TRUE)  #calculates inverse of matrix "m" 
##  cacheSolve(cm, demomode=TRUE)  #retrives cached copy of inverse matrix




## 
##  The makeCacheMarix caches in its environment two objects:  
##  1.  The matrix object passed through input parameter "x"
##  2.  Lazy-populated cached value "lazyCache"
##
##  The input parameter must be a square matrix with elements of numeric, 
##  integer, or complex type
##  The code itself is pretty generic and could be used to store any 
##  type of "lazyCache", so it is up to companion functions such as cacheSolve
##  to decide what to store in "lazyCache" and how to populate the cached value 
##  
##  The ouput is a list of four set/get functions plus a "createdby" element 
##  indicating the name of the function that created the output
##  This can be used by companion function to make sure the input 
##  is of a proper type

makeCacheMatrix <- function(x = matrix()) {
    #some input parameter checking
    # first, make sure x is a matrix
    # second, only numeric, integers, and complex numbers are allowed
    stopifnot(
        class(x) == "matrix",
        any(class(as.vector(x))==c("numeric","integer",complex) )
    )
    # third, both dimensions should be the same
    if (dim(x)[1] != dim(x)[2]) { 
        stop("Matrix is not square: ", paste(dim(x),collapse=","))
    }
    
    #Now to work: set the cached value to NULL and define 
    #all the get/set functions 
    lazyCache<-NULL
    set<- function (y=NULL) {
        x <<- y
        lazyCache <<- NULL
    }
    get<- function() {x}
    setcache<- function(y) {lazyCache<<-y}
    getcache<- function() {lazyCache}
    list(set=set, get=get, setcache=setcache, getcache=getcache, 
         createdby="makeCacheMatrix")
    
}


## cacheSolve uses an object created by makeCacheMatrix 
## to cache claculations of an inverse matrix
## if cached value is null, a call to solve() is used to populate cache,
## otherwise cached value is retruned right away
## call with demomode=TRUE if you want to know whether cached or 
## newly calculated value was returned

cacheSolve <- function(x, demomode=FALSE,...) {
        ## Return a matrix that is the inverse of 'x'
    ##a simple quick-n-dirty check to make sure "x" is an object likely created by makeCacheMatrix 
    nameToCheck<- "createdby"
    creatorsToCheck<- c("makeCacheMatrix","freepass")  ##add more names if necessary
    xnames<-names(x)
    if (!any(xnames=="createdby")) stop("Could not verify input object creator")
    creator<-x[xnames==nameToCheck]
    if (!any(creatorsToCheck==creator)) stop("Wrong creator: ",creator )

    
    solved<-x$getcache()
    if (!is.null(solved)) {
        if (demomode) message("Using cached value")
        return(solved)
    }
    if (demomode) { 
        message("Calculating the inverse...")
        Sys.sleep(2)
    }
    solved<-solve(x$get(),...)
    x$setcache(solved)
    solved
}
