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
    p <- vector('list',bases)
    i <- 1
    index <- function(mat) sum(sapply(lst,FUN=function(x) x$index(mat)))
    cat("Initial Value",index(start),'\n')
    p[[1]] <- start
    while (i <= bases)
      {
        p[[i+1]]<- search_geodesic(current=p[[i]],index=index,n=n[i],max.tries=max.tries)
        if((length(p[[i+1]]) == 1)) return(p)
        i <- i + 1
      }
    return(p)
  }
