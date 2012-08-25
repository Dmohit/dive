pair <- function(ID)
  {
    if(length(ID) != 2 | class(ID) != character) stop("Invalid ID passed to intiate pair")
    ID <- ID
    data <- data
    oth <- oth
    if(type == 'false') stop("To initialize a pair object, type argument cannot be false")
    setd <- set(type)
    setf <- set('false')
    get.ID <- function() return(ID)
    set.ids <- function(a,ids) a$set.ids(ids)
    add.ids <- function(a,ids) a$add.ids(ids)
    rem.ids <- function(a,ids) a$rem.ids(ids)
    get.ids <- function(a)     a$get.ids()
    set.data.ids <- function(ids) set.ids(setd,ids)
    set.false.ids <- function(ids) set.ids(setf,ids)
    add.data.ids <- function(ids) add.ids(setd,ids)
    add.false.ids <- function(ids) add.ids(setf,ids)
    rem.data.ids <- function(ids) rem.ids(setd,ids)
    rem.false.ids <- function(ids) rem.ids(setf,ids)
    get.data.ids <- function() get.ids(setd)
    get.false.ids <- function() get.ids(setf)
    copy.ids.from.false <- function()
      {
        ids <- get.ids(setf)
        set.ids(setd)
      }
    copy.ids.from.data <- function()
      {
        ids <- get.ids(setd)
        set.ids(setf)        
      }
    get.data <- function()
      return(data[get.data.ids(),])
    get.oth <- function()
      return(oth[get.false.ids(),])
    return(list(get.ID=get.ID,
                set.data.ids=set.data.ids,
                set.false.ids=set.false.ids,
                add.data.ids=add.data.ids,
                add.false.ids=add.false.ids,
                get.data.ids=get.data.ids,
                get.false.ids=get.false.ids,
                rem.data.ids=rem.data.ids,
                rem.false.ids=rem.false.ids,
                copy.ids.from.data=copy.ids.from.data,
                copy.ids.from.false=copy.ids.from.false,
                get.data=get.data,
                get.oth=get.oth))
  }
