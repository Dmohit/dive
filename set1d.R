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
    return(list(set.ids=set.ids,add.ids=add.ids,rem.ids=rem.ids,get.ids=get.ids,number=number))
  }

oset <- function(oth,mat=NULL)
  {
    if(is.null(mat))
      {
        projected <- FALSE
        mat <- NULL
        base <- oth
        oth <- oth
      }
    else
      {
        projected <- TRUE
        mat <- mat
        base <- oth
        oth <- base %*% mat
      }
    get.oth <- function() oth
    get.mat <- function() if(projected==TRUE) mat else FALSE
    is.projected <- function() projected
    set.mat <- function(mat)
      {
        if(is.null(mat))
          {
            projected <<- TRUE
            mat <<- mat
            base <<- oth
            oth <<- base %*% mat
          }
        else FALSE
      }
    set.oth <- function(oth)
      {
        if(projected == FALSE)
          {
            base <<- oth
            oth <<- oth
          }
        if(projected == TRUE)
          {
            base <<- oth
            oth <<- base %*% mat
          }
      }
    unset.projected <- function()
    {
      projected <<- FALSE
      oth <<- base
    }
    number <- function() nrow(oth)
    columns <- function() ncol(oth)
    return(list(get.oth=get.oth,set.oth=set.oth,
                number=number,columns=columns,
                get.mat=get.mat,set.mat=set.mat,
                is.projected=is.projected,unset.projected=unset.projected))
  }
