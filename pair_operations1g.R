#Functions that operate on a list of pair objects
#The input list of pairs describes a single (ggobi) view
                                        #Sanity Functions
is.view.exhaustive <- function(lst,dat)
  {
    all.ids <- sapply(lst,FUN=function(x) x$get.data.ids())
    all(1:nrow(get(dat)) %in% do.call('c',all.ids))
  }

is.view.mutually.exclusive <- function(lst)
  {
    ids <- sapply(lst,FUN=function(x) x$get.data.ids())
    for(i in 1:(length(ids)-1))
      {
        for(j in (i+1):length(ids))
          {
            if(any(ids[[i]] %in% ids[[j]])) return(FALSE)
          }
      }
    TRUE
  }

is.view.sane <- function(lst,dat)
  {
    if(is.view.exhaustive(lst,dat) &
       is.view.mutually.exclusive(lst) &
       all(sapply(lst,FUN=function(x) x$sanity())))
      return(TRUE)
    else if(is.view.exhaustive())
      warning("View is not Exhaustive")
    else if(is.view.mutually.exclusive())
      warning("View is not Mutually Exhaustive")
    else
      warning("Some pair has gone bad")
    return(FALSE)
  }

better.view <- function(lst,start,bases,n,max.tries=10)
  {
    p <- vector('list',bases + 1)
    index <- function(mat) sum(sapply(lst,FUN=function(x) x$index(mat)))
    cat("Initial Value",index(start),'\n')
    i <- 1
    while (i <= bases)
      {
        if(i==1)
          {
            p[[i]]<- search_geodesic(current=start,index=index,n=n,max.tries=max.tries)
          }
        else
          {
            p[[i]]<- search_geodesic(current=p[[i-1]],index=index,n=n,max.tries=max.tries)
          }
        if(is.null(p[[i]])) return(p[1:(i-1)])
        i <- i + 1
      }
    return(p)
  }
