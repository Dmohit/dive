proj <- function() {
  v <- vector('list',length=10)
  names(v) <- c('status','proj','ID','fixed','target','wtf','wtt','wtr','olds','current')
  class(v) <- 'proj'
  return(v)
}

init.proj <- function(v) {
  
