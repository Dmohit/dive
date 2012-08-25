widget.env <- function()
  {
    require(gWidgets)
    options("guiToolkit"="RGtk2")
    ###State Variables
    data <- NULL
    projs <- NULL
    slots <- 0
    GGobiHandle <- NULL
    PairsHandle <- NULL
    views <- NULL
    opthistory <- NULL
    numviews <- 0
    ###Updating Functions for base
    update.base <- function()
      {
        if(is.null(data) & is.null(projs))
          {

            enabled(choose.matrices) <- FALSE
            enabled(choose.set) <- TRUE
            enabled(choose.matrices) <- FALSE
          }
        else if(!is.null(data) & is.null(projs))
          {
            enabled(choose.set) <- TRUE
            enabled(choose.matrices) <- TRUE
            enabled(spawn.ggobi) <- FALSE
          }
        else if(!is.null(data) & !is.null(projs))
          {
            enabled(choose.set) <- TRUE
            enabled(choose.matrices) <- TRUE
            enabled(spawn.ggobi) <- TRUE
          }
        visible(base) <- TRUE
      }
    ###Basic GUI functions
    chooseData <- function(h,...)
      {
        listOfObjects <- lstObjects(target.class="matrix")
        win <- gwindow("Choose Data Frame...", visible=TRUE)
        group0 <- ggroup(horizontal = FALSE, container=win)
        tab <- gtable(listOfObjects,container = group0,expand=TRUE,
                      handler=function(h,...)
                      {
                        data <<- svalue(h$obj)
                        dispose(win)
                        update.base()
                      })
        button.group <- ggroup(horizontal = TRUE, container=group0)
        addSpring(button.group)
        addSpace(button.group,5)
        no <- gbutton("Cancel",container=button.group,handler = function(h,...) dispose(win))
        yes <- gbutton("OK",container=button.group,
                       handler=function(h,...)
                       {
                         data <<- svalue(tab)
                         dispose(win)
                         update.base()
                       })
      }
    chooseProjections <- function(h,...)
      {
        listOfObjects <- lstObjects(target.class="matrix")
        win <- gwindow("Choose Projection Matrices...", visible=TRUE)
        group0 <- ggroup(horizontal = FALSE, container=win)
        tab <- gtable(listOfObjects,container = group0,multiple=TRUE,expand=TRUE)
        if(!is.null(get('projs'))) svalue(tab) <- projs
        slot.group <- ggroup(horizontal = TRUE, container=group0)
        addSpring(slot.group)
        glabel("Add Empty Slots :",container=slot.group)
        numslots <- gspinbutton(from=0,to = 15,by =1,value=5,container=slot.group)
        button.group <- ggroup(horizontal = TRUE, container=group0)
        addSpring(button.group)
        addSpace(button.group,5)
        no <- gbutton("Cancel",container=button.group,handler = function(h,...) dispose(win))
        yes <- gbutton("OK",
                       container=button.group,
                       handler=function(h,...)
                       {
                         if(is.null(projs)) projs <<- svalue(tab)
                         else projs <<- unique(c(projs,svalue(tab)))
                         slots <<- svalue(numslots)
                         dispose(win)
                         update.base()
                       })
      }
    ###GUI functions for Views Window
    createTab <- function(NoteBookHandle,GGobiHandle,TabName)
      {
        newpage <- ggroup(horizontal = TRUE, container=NoteBookHandle, label=TabName)
        numbrushes <- GGobiHandle$get.number.brushes()
        brushes <- GGobiHandle$get.brush.colors()
        brushes.labels <- gframe(horizontal = FALSE, container=newpage,expand=TRUE)
        brushes.radio <- gframe(horizontal=FALSE,container=newpage,expand=TRUE)
        brushes.spin <- gframe(horizontal=FALSE,container=newpage,expand=TRUE)
        for(i in 1:numbrushes)
          {
            glabel(paste("Brush",i,"Color",brushes[i]), container = brushes.labels)
            set <- gradio(c("Unfixed set","Fixed Set","Target Set"), container=brushes.radio,selected=1,horizontal=TRUE)
            spin <- gspinbutton(from=-1,to=1,by=0.1,value=0,container=brushes.spin)
          }
        ## glabel("Information", container =group.nb)
        ## obj5 <- gtext("This current view has moved points",container=group.nb)
        ## obj6 <- gbutton("Refresh Information",container=group.nb,handler = function(h,...) gmessage("world"))
        ## glabel("Selection Information",container=group0)
        ## obj7 <- gtable(dd,container=group0,expand=TRUE)
      }
    
    ###Diagnostics
    get.data <- function() data
    get.proj <- function() projs
    get.slots <- function() slots
    
    ###Getting things going
    Spawn <- function(h,...)
      {
        PairsHandle <<- pair.env(get(data))
        numviews <<- length(projs)
        view <<- vector('list',numviews)
        opthistory <<- vector('list',numviews)
        frame <- make.frame(data,projs,slots)
        GGobiHandle <<- ggobi.manager(frame,data)
        for(i in 1:numviews)
          {
            opthistory[[i]] <<- get(projs[i])
            view[[i]] <<- PairsHandle$new.pair()
            createTab(nb,GGobiHandle,as.character(i))
          }

        
        #spwan view windows()
      }
    
    ###Construct Base Window
    base <- gwindow('cepp Engine',visible=TRUE)
    choose.set <- gbutton("Choose Data Set",container=base,handler=chooseData)
    choose.matrices <- gbutton("Choose Projection Matrices",container=base,handler=chooseProjections)
    spawn.ggobi <- gbutton("Spawn GGobi",container=base,handler=Spawn)
    enabled(choose.set) <- TRUE
    enabled(choose.matrices) <- FALSE
    enabled(spawn.ggobi) <- FALSE
    ###Construct Views Window
    views.win <- gwindow("Views Window", visible=TRUE)
    group0 <- ggroup(horizontal = FALSE, container=views.win)
    nb <- gnotebook(container = group0, expand=TRUE)
    ###Kick-Off
    update.base()
    ###returns
    return(list(get.data=get.data,get.proj=get.proj,get.slots=get.slots))
  }
