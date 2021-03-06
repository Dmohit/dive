require(rggobi)
load('SANN Candidates.RData')

new.cepp <- function(data,objlist,slots,name='cepp') {
  #Shared Variable definitions
  xnames <- paste('x',1:(length(objlist) + slots),sep='')
  ynames <- paste('y',1:(length(objlist) + slots),sep='')
  projdim <- dim(objlist[[1]])[2]
  nr <- nrow(data)
  nc <- nrow(data)
  l <- vector('list',length=length(objlist)+slots)
  g <- 0
  #Initializations
  for(i in 1:length(l)) {
    l[[i]] <- projection()
    l[[i]]$ID <- c(xnames[i],ynames[i])
    l[[i]]$fixed <- NULL
    l[[i]]$target <- NULL
    l[[i]]$wtf <- 0
    l[[i]]$wtt <- 0
    l[[i]]$wtr <- 1
    l[[i]]$olds <- NULL
    if(i <= length(objlist)) {
      l[[i]]$status <- 1
      l[[i]]$proj <- objlist[[i]]
    }
    else {
      l[[i]]$status <- 0
      l[[i]]$proj <- matrix(0,nrow=nr,ncol=projdim)
    }
    l[[i]]$current <- data %*% l[[i]]$proj
  }
  #Functions go here
  projection <- function() {
    v <- vector('list',length=10)
    names(v) <- c('status','proj','ID','fixed','target','wtf','wtt','wtr','olds','current')
    return(v)
  }

  selection <- function() {
    v <- vector('list',length=2)
    names(v) <- c('colour','glyph')
    return(v)
  }
  
  make.frame <- function() {
    #Called only during Initialization
    frame <- do.call(cbind,lapply(l,FUN=function(x) x$current))
    colnames(frame) <- 1:dim(b)[2]
    colnames(frame)[seq(1,dim(frame)[2],2)] <- paste('x',1:(dim(frame)[2]/2),sep='')
    colnames(frame)[seq(2,dim(frame)[2],2)] <- paste('y',1:(dim(frame)[2]/2),sep='')
    return(frame)
  }

  init.ggobi <- function(data,projs,slots,name='cepp') {
    #projs is a list of projection matrices
    #slots is the number of empty slots requested
    frame <- make.frame()
    g <<- ggobi(frame,name=name)
    lapply(l,FUN = display,x=g[1],vars=list(X=xnames[i],Y=ynames[i]))
  }

  ggobi.id.to.string <- function(i) names(c(variables(displays(g)[[i]])$X,variables(displays(g)[[i]])$Y))

  get.glyph.group <- function() {}

  get.color.group <- function() {}

  information(id) {
    i <- which(sapply(l,FUN=function(x) x$ID) == id)
    if(l[[i]]$status == 0) return("This slot is inactive")
    else
  }
}
