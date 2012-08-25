ggobi.manager <- function(frame,name='cepp')
  {
    require(rggobi)
    cols <- c('Purple','Pink','Red','Blue','Green','Orange','Brown','Grey','Yellow')
    g <- ggobi(frame,name=name)
    nc <- ncol(frame)
    xnames <- colnames(frame)[seq(1,nc,2)]
    ynames <- colnames(frame)[seq(2,nc,2)]

    new.display <- function(names)
      {        
        d <- display(g[1],vars=list(X=names[1],Y=names[2]))
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

    get.display.variable <- function(var)
      {
        d <- get.display.dataset()
        return(d[,var])
      }
    
    get.current.id <- function()
      {
        l <- get.scatter.displays()
        xname <- variables(l[[1]])$X
        yname <- variables(l[[1]])$Y
        return(names(c(xname,yname)))
      }
    
    is.valid <- function(id)
    {
      l <- get.scatter.displays()
      xname <- variables(l[[1]])$X
      yname <- variables(l[[1]])$Y
      if(id[1] != xname | id[2] != yname) return(FALSE)
      else return(TRUE)
    }
    
    redraw <- function(var,values)
      {
        d <- get.display.dataset()
        d[,var] <- values
      }
    
    get.color.ids <- function(color)
      {
        which(cols[glyph_color(g[1])] %in% color)
      }
    
    get.color.data <- function(color)
      {
        get.display.dataset()[get.color.ids(color),]
      }

    get.color.vars <- function(color,vars)
      {
        get.color.data(color)[,vars]
      }

    get.number.brushes <- function()
      {
        length(unique(glyph_color(g[1])))
      }

    get.brush.colors <- function()
      {
        cols[unique(glyph_color(g[1]))]
      }

    get.all.cols <- function()
      return(cols)

    exists.color <- function(col)
      any(col %in% get.brush.colors())
    
    return(list(new.display=new.display,get.display.dataset=get.display.dataset,get.display.variable=get.display.variable,
                get.current.id=get.current.id,
                redraw=redraw,
                get.number.brushes=get.number.brushes,get.brush.colors=get.brush.colors,
                get.all.cols=get.all.cols,
                get.color.ids=get.color.ids,get.color.data=get.color.data,get.color.vars=get.color.vars,exists.color=exists.color))
  }
