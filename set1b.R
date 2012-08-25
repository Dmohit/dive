dset <- function(rids,projection=NULL)
  {
    row.ids <- rids
    projection <- projection
    set.ids <- function(ids) row.ids <- ids
    add.ids <- function(ids) row.ids <- c(row.ids,ids)
    rem.ids <- function(ids)
      {
        tmp <- row.ids[which(!(row.ids %in% ids))]
        row.ids <- tmp
      }
    get.ids <- function() return(row.ids)
    set.projection <- function(mat) projection <<- mat
    get.projection <- function(mat) return(projection)
    return(list(set.ids=set.ids,add.ids=add.ids,rem.ids=rem.ids,get.ids=get.ids,
                set.projection=set.projection,get.projection=get.projection))
  }
oset <- function(oth)
  {
    oth <- oth
    get.oth <- function() return(oth)
    set.oth <- function(oth) oth <<- oth
  }
