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
  return(b)
}

init.ggobi <- function(data,projs,slots,name='cepp') {
  #projs is a list of projection matrices
  #slots is the number of empty slots requested
  require(rggobi)
  projdim <- dim(projs[[1]])[2]
  frame <- make.frame(data=data,objlist=projs)
  empty.template <- matrix(0,nrow=nrow(frame),ncol=projdim)
  for(i in 1:slots) {
    frame <- cbind(frame,empty.template)
  }
  xnames <- paste('x',1:(dim(frame)[2]/2),sep='')
  ynames <- paste('y',1:(dim(frame)[2]/2),sep='')
  colnames(frame)[seq(1,dim(frame)[2],2)] <- paste('x',1:(dim(frame)[2]/2),sep='')
  colnames(frame)[seq(2,dim(frame)[2],2)] <- paste('y',1:(dim(frame)[2]/2),sep='')
  g <- ggobi(frame,name=name)
  for(i in 1:length(projs)) {
    display(g[1],vars=list(X=xnames[i],Y=ynames[i]))
  }
  return(g)
}


g <- init.ggobi(OlivesT,list(o1$par,o2$par,o3$par),slots=3)
