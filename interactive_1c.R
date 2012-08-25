load('SANN Candidates.RData')
backend <- function(data,objlist,slots) {
  #Shared Variable definitions
  xnames <- paste('x',1:(length(objlist) + slots),sep='')
  ynames <- paste('y',1:(length(objlist) + slots),sep='')
  l <- vector('list',length=length(objlist)+slots)
  #Initializations
  for(i in 1:length(l)) {
    l[[i]] <- new.structure()
    l[[i]]$ID <- c(xnames[i],ynames[i])
    l[[i]]$fixed <- NULL
    l[[i]]$target <- NULL
    l[[i]]$wtf <- 0
    l[[i]]$wtt <- 0
    l[[i]]$wtr <- 1
    l[[i]]$olds <- NULL
    if(i <= length(objlist) {
      l[[i]]$status <- 1
      l[[i]]$proj <- objlist[[i]]
    }
    else {
      l[[i]]$status <- 0
      l[[i]]$current <- data %*% l[[i]]$proj
    }
}
  #Functions go here
  new.structure <- function() {
    v <- vector('list',length=10)
    names(v) <- c('status','proj','ID','fixed','target','wtf','wtt','wtr','olds','current')
    return(v)
  }
  return(list())
     }
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
}

