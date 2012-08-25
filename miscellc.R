lstObjects <- function(envir= .GlobalEnv, target.class)
  {
    objlist <- ls(envir=envir)
    objclass <- sapply(objlist, function(objName) class(get(objName, envir=envir))[1])
    data.frame(Name = I(objlist)[objclass==target.class], Class = I(objclass)[objclass==target.class])
  }
