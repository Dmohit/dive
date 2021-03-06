#Functions that operate on a list of pair objects
#The input list of pairs describes a single (ggobi) view
is.view.exhaustive <- function(lst,dat)
  {
    dimensions <- dim(get(dat))
    all.ids <- sapply(lst,FUN=function(x) x$get.data.ids)
    all(1:dimensions[2] %in% all.ids)
  }

is.view.mutually.exclusive <- function(lst)
  {
    ids <- lapply(lst,FUN=function(x) x$get.data.ids)
    for(i in length(ids):1)
      {
        for(j in 1:(i-1))
          {
            if(any(ids[[i]] %in% ids[[j]])) return(FALSE)
          }
      }
    return(TRUE)
  }

is.view.sane <- function(lst)
  {
    if(is.view.exhaustive() & is.view.mutually.exclusive() & all(sapply(lst,FUN=function(x) x$sanity))) return(TRUE)
    else if(is.view.exhaustive())
      warning("View is not Exhaustive")
    else if(is.view.mutually.exclusive())
      warning("View is not Mutually Exhaustive")
    return(FALSE)
  }

create.new.unfixed.pair <- function(lst,GGobiHandle,PairHandle,color)
  {
    ids <- GGobiHandle$get.color.group(color)
    np <- PairHandle$new.pair(rids=ids,type="unfixed")
    sapply(lst,FUN=function(x) x$rem.points,ids=ids)
    stopifnot(is.view.sane(list(lst,np)))
    return(list(lst,np))
  }

create.new.fixed.pair <- function(lst,GGobiHandle,PairHandle,color)
  {
    ids <- GGobiHandle$get.color.group(color)
    np <- PairHandle$new.pair(rids=ids,type="fixed")
    sapply(lst,FUN=function(x) x$rem.points,ids=ids)
    stopifnot(is.view.sane(list(lst,np)))
    return(list(lst,np))
  }

GGobi.brush.to.pair <- function(GGobiHandle,lst,color)
  {
    ids <- GGobiHandle$get.color.group(color)
    for(i in 1:length(lst))
      {
        if(all(ids %in% lst[[i]]$get.ids)) return(i)
        else if(any(ids %in% lst[[i]]$get.ids)) stop("More than 1 pair corresponds to a brush group.")
      }
  }

## create.new.target.pair <- function(lst,GGobiHandle,PairHandle,color)
##   {
##     ids <- GGobiHandle$get.color.group(color)
##     ##See if points moved
##     reald <- PairHandle$get.data()
##     faked <- GGobiHandle$get.display.dataset()
##     if(all.equal(
##     np <- PairHandle$new.pair(rids=ids,type="fixed")
##     sapply(lst,FUN=function(x) x$rem.points,ids=ids)
##     stopifnot(is.view.sane(list(lst,np)))
##     return(list(lst,np))
##   }

remove.pair <- function(lst)
  {
  }

obj.fun <- function(lst,wts,mat)
  {
    vals <- sapply(lst,FUN=function(x) x$index(mat))
    return(wts * vals)
  }

better.view <- function(lst,opthist,bases,n,max.tries=10)
  {
    p <- vector('list',bases+1)
    i <- 1
    cat("Initial Value",index(start),'\n')
    p[[1]] <- start
    while (i <= bases)
      {
        p[[i+1]]<- search_geodesic(current=p[[i]],index=index,n=n[i],max.tries=max.tries)
        if((length(p[[i+1]]) == 1)) return(p)
        i	<- i + 1
      }
    return(p)
  }
