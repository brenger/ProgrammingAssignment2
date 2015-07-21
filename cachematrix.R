## Calculate and put inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
		m<-NULL
		set<-function(y){
				x<<-y
				m<<-NULL
		}
		get<-function() x
		setinv<-function(solve) m<<- solve
		getinv<-function() m
		list(set=set, get=get,
				setinv=setinv,
				getinv=getinv)
}

## Retrieve inverted matrix from cache if available, otherwise calculate it

cacheSolve <- function(x=matrix(), ...) {
		m<-x$getinv()
		if(!is.null(m)){
				message("getting cached data")
				return(m)
		}
		matrix<-x$get()
		m<-solve(matrix, ...)
		x$setinv(m)
		m
}
