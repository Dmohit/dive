load('SANN Candidates.RData')
make.frame <- function(data,objlist) {
  #objlist is a list of projecton matrices
  projdim <- dim(objlist[[1]])[2]
  fac <- nrow(data)
  a <- sapply(objlist,FUN=function(x) data %*% x)
  b <- numeric(length(a))
  dim(b) <- c(fac,length(objlist)*projdim)
  for(i in 1:ncol(a)) {
    tmp <- a[,i]
    dim(tmp) <- c(fac,projdim)
    b[,(1+(i-1)*projdim):(i*projdim)] <- tmp
  }
  colnames(b) <- 1:dim(b)[2]
  colnames(b)[seq(1,dim(b)[2],2)] <- paste('x',1:(dim(b)[2]/2),sep='')
  colnames(b)[seq(2,dim(b)[2],2)] <- paste('y',1:(dim(b)[2]/2),sep='')
  return(b)
}

init.ggobi <- function(data,projs,slots,name='cepp') {
  #projs is a list of projection matrices
  #slots is the number of empty slots requested
  frame <- make.frame(objlist=projs)
  )

g['proj2'] <- frame[,3:4]
