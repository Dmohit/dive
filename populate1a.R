ggobi.brush.to.cpair <- function(ggobi.handle,color,type=NULL,original.data)
  {
    if(type!=NULL) stop("I don't know what type of data structure you want!")
    rids <- ggobi.handle$get.color.group(color)
    newpair <- pair(type)
    
    newset$set.ids(rids)
  }

ggobi.brush.to.pair <- function()
  {
  }
