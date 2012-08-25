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
    new.pair <- PairHandle(rids=ids,type="unfixed")
    for(i in 1:length(lst))
      {
        lst[[i]]$rem.data.ids(ids)
        lst[[i]]$construct.o.from.d()
      }
    stopifnot(is.view.sane(list(lst,new.pair)))
    return(list(lst,new.pair))
  }

create.new.fixed.pair <- function(lst,GGobiHandle,PairHandle,color)
  {
    
  }

create.new.target.pair <- function(lst,GGobiHandle,PairHandle,color)
  {
  }

remove.pair <- function(lst)
  {
  }

build.obj.fun <- function(lst)
  {
  }

better.view <- function(lst,opthist)
  {
  }
