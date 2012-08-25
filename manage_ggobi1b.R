ggobi.manager <- function(frame,name='cepp')
  {
    cols <- c('purple','pink','red','blue','green','orange','brown','grey','yellow')
    g <- ggobi(frame,name=name)
    nc <- ncol(frame)
    xnames <- colnames(frame)[seq(1,nc,2)]
    ynames <- colnames(frame)[seq(2,nc,2)]
    for(i in 1:(nc/2))
      {
        d <- display(g[1],vars=list(X=xnames[i],Y=ynames[i]))
        imode(d) <- "Brush"
      }
    
    get.scatter.displays <- function()
      {
        l <- displays(g)[sapply(displays(g),FUN=function(x) class(x)[1] == "GGobiScatterplotDisplay")]
        return(l)
      }
    
    get.display.dataset <- function()
      {
        l <- get.scatter.displays()
        return(dataset(l[[1]]))
      }
    
    get.current.id <- function()
      {
        l <- get.scatter.displays()
        xname <- variables(l[[1]])$X
        yname <- variables(l[[1]])$Y
        return(c(xname,yname))
      }
    
    is.valid <- function(id)
    {
      l <- get.scatter.displays()
      xname <- variables(l[[1]])$X
      yname <- variables(l[[1]])$Y
      if(id[1] != xname | id[2] != yname) return(FALSE)
      else return(TRUE)
    }
    
    redraw <- function(id,var,values)
      {
        cur.id <- get.current.id()
        if(!(var %in% cur.id)) return(FALSE)
        else
          {
            d <- get.display.dataset()
            d[,var] <- values
            return(TRUE)
          }
      }
    
    get.color.group <- function(id,color)
      {
        if(is.valid(id))
           {
             colint <- which(color %in% cols)
             return(as.integer(which(glyph_color(g[1])== colint)))
           }
        return(FALSE)
      }
    return(list(get.display.dataset=get.display.dataset,
                get.current.id=get.current.id,
                redraw=redraw,
                get.color.group))
  }
