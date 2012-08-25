#Functions that operate on a list of pair objects
#The input list of pairs describes a single (ggobi) view
is.view.exhaustive <- function(list,dat)
  {
    dimensions <- dim(get(dat))
    all.ids <- sapply(list,FUN=function(x) x$get.data.ids)
    all(1:dimensions[2] %in% all.ids)
  }

is.view.mutually.exclusive <- function(list)
  {
  }

is.view.sane <- function(list)
  {
    if(is.view.exhaustive() & is.view.mutually.exclusive()) TRUE
    else FALSE
  }

create.new.pair <- function(list)
  {
  }

remove.pair <- function(list)
  {
  }

build.obj.fun <- function(list)
  {
  }

better.view <- function(list,opthist)
  {
  }
