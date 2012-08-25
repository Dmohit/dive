view <- function(data,oth,projection)
  {
    pairs <- vector('list',1)
    wts <- numeric(1)
    pairs[[1]] <- pair('unfixed')
    pairs[[1]]$set.data.ids(1:nrow(data))
    pairs[[1]]$set.false.ids(1:nrow(oth))
    wts[1] <- 1
    objfun <- cpair(n=50,k=2,pair=pairs[[1]],dat=data,other=oth,start)
    new.view.from.ggobi <- function(ggobi.id)
      {
        
      }
    
  }
