dset <- function(rids)
  {
    row.ids <- rids
    set.ids <- function(ids) row.ids <<- ids
    add.ids <- function(ids) row.ids <<- c(row.ids,ids)
    rem.ids <- function(ids)
      {
        tmp <- row.ids[which(!(row.ids %in% ids))]
        row.ids <<- tmp
      }
    get.ids <- function() row.ids
    number <- function() length(row.ids)
    return(list(set.ids=set.ids,add.ids=add.ids,rem.ids=rem.ids,get.ids=get.ids))
  }
oset <- function(oth)
  {
    oth <- oth
    get.oth <- function() return(oth)
    set.oth <- function(oth) oth <<- oth
    number <- function() nrow(oth)
    columns <- function() ncol(oth)
  }
