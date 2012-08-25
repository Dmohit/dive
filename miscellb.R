lstObjects <- function(envir= .GlobalEnv, target.class)
  {
    objlist <- ls(envir=envir)
    objclass <- sapply(objlist, function(objName) class(get(objName, envir=envir))[1])
    data.frame(Name = I(objlist)[objclass==target.class], Class = I(objclass)[objclass==target.class])
  }

make.frame <- function(dat,objs)
{
  data <- get(dat)
  projs <- lapply(objs,get)
  projdim <- dim(projs[[1]])[2]
  fac <- nrow(data)
  a <- sapply(projs,FUN=function(x) data %*% x)
  b1 <- numeric(length(a))
  dim(b1) <- c(fac,length(objs)*projdim)
  for(i in 1:ncol(a))
    {
      tmp <- a[,i]
      dim(tmp) <- c(fac,projdim)
      b1[,(1+(i-1)*projdim):(i*projdim)] <- tmp
    }
  colnames(b1) <- 1:ncol(b1)
  b2 <- b1
  xnames <- paste('x',1:(ncol(b1)),sep='')
  ynames <- paste('y',1:(ncol(b1)),sep='')
  colnames(b1)[seq(1,ncol(b1),2)] <- xnames[1:(ncol(b1)/2)]
  colnames(b1)[seq(2,ncol(b1),2)] <- ynames[1:(ncol(b1)/2)]
  colnames(b2)[seq(1,ncol(b1),2)] <- xnames[(1+ncol(b1)/2):ncol(b1)]
  colnames(b2)[seq(2,ncol(b1),2)] <- ynames[(1+ncol(b1)/2):ncol(b1)]
  b <- cbind(b1,b2)
  return(b)
}
