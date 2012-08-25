lstObjects <- function(envir= .GlobalEnv, target.class)
  {
    objlist <- ls(envir=envir)
    objclass <- sapply(objlist, function(objName) class(get(objName, envir=envir))[1])
    data.frame(Name = I(objlist)[objclass==target.class], Class = I(objclass)[objclass==target.class])
  }

make.frame <- function(dat,objs,slots)
{
  data <- get(dat)
  projs <- lapply(objs,get)
  projdim <- dim(projs[[1]])[2]
  fac <- nrow(data)
  a <- sapply(projs,FUN=function(x) data %*% x)
  b <- numeric(length(a))
  dim(b) <- c(fac,length(objs)*projdim)
  for(i in 1:ncol(a))
    {
      tmp <- a[,i]
      dim(tmp) <- c(fac,projdim)
      b[,(1+(i-1)*projdim):(i*projdim)] <- tmp
    }
  empty.template <- matrix(0,nrow=fac,ncol=projdim)
  for(i in 1:slots)
    {
      b <- cbind(b,empty.template)
    }
  colnames(b) <- 1:ncol(b)
  xnames <- paste('x',1:(dim(b)[2]/2),sep='')
  ynames <- paste('y',1:(dim(b)[2]/2),sep='')
  colnames(b)[seq(1,dim(b)[2],2)] <- paste('x',1:(dim(b)[2]/2),sep='')
  colnames(b)[seq(2,dim(b)[2],2)] <- paste('y',1:(dim(b)[2]/2),sep='')
  return(b)
}
