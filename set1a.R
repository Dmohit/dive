set <- function(type)
  {
    row.ids <- numeric(len)
    if(!(type %in% c('false','fixed','target','unfixed'))) stop("Invalid type argument passed")
    type <- type
    get.type <- function()
      return(type)
    set.ids <- function(ids)
      row.ids <- ids
    add.ids <- function(ids)
      row.ids <- c(row.ids,ids)
    rem.ids <- function(ids)
      {
        tmp <- row.ids[which(!(row.ids %in% ids))]
        row.ids <- tmp
      }
    get.ids <- function()
      return(row.ids)
    return(list(get.type=get.type,set.ids=set.ids,add.ids=add.ids,rem.ids=rem.ids,get.ids=get.ids))
  }
