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
    tabs <- NULL
    wts <- NULL
    numviews <- 0
    
    ###diagnostics
    get.data <- function() return(data)
    get.proj <- function() return(projs)
    get.slots <- function() return(slots)
    get.GGobiHandle <- function() return(GGobiHandle)
    get.PairsHandle <- function() return(PairsHandle)
    get.views <- function() return(views)
    get.opthistory <- function() return(opthistory)
    get.tabs <- function() return(tabs)
    get.wts <- function() return(wts)
    get.numviews <- function() return(numviews)
    
    ###Updating Functions for base
    update.base <- function()
      {
        if(is.null(data) & is.null(projs))
          {

            enabled(choose.set) <- TRUE
            enabled(choose.matrices) <- FALSE
            enabled(spawn.ggobi) <- FALSE
            enabled(update.view) <- FALSE
          }
        else if(!is.null(data) & is.null(projs))
          {
            enabled(choose.set) <- TRUE
            enabled(choose.matrices) <- TRUE
            enabled(spawn.ggobi) <- FALSE
            enabled(update.view) <- FALSE
          }
        else if(!is.null(data) & !is.null(projs))
          {
            enabled(choose.set) <- TRUE
            enabled(choose.matrices) <- TRUE
            enabled(spawn.ggobi) <- TRUE
            enabled(update.view) <- TRUE
          }
        visible(base) <- TRUE
      }
    ###Basic GUI functions
    chooseData <- function(h,...)
      {
        listOfObjects <- lstObjects(target.class="matrix")
        win <- gwindow("Choose Data Frame...", visible=TRUE)
        group0 <- ggroup(horizontal = FALSE, container=win)
        datatable <- gtable(listOfObjects,container = group0,expand=TRUE,
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
                         data <<- svalue(datatable)
                         dispose(win)
                         update.base()
                       })
      }
    chooseProjections <- function(h,...)
      {
        listOfObjects <- lstObjects(target.class="matrix")
        win <- gwindow("Choose Projection Matrices...", visible=TRUE)
        group0 <- ggroup(horizontal = FALSE, container=win)
        projtab <- gtable(listOfObjects,container = group0,multiple=TRUE,expand=TRUE)
        if(!is.null(projs)) svalue(projtab) <- projs
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
                         if(is.null(projs)) projs <<- svalue(projtab)
                         else projs <<- svalue(projtab)
                         slots <<- svalue(numslots)
                         dispose(win)
                         update.base()
                       })
      }
    
    ###GUI functions for Views Window
    ##createTab only draws, and does not interact with other parts of the program
    createTab <- function(NoteBookHandle,GGobiHandle,TabName,dispnames,outnames,pairpointer)
      {
        #State Variables
        NoteBookHandle <- NoteBookHandle
        GGobiHandle <- GGobiHandle
        Tabname <- TabName
        dispnames <- dispnames
        outnames <- outnames
        pairpointer <- pairpointer
        #Drawing
        newpage <- ggroup(horizontal = FALSE, container=NoteBookHandle, label=TabName)
        #The Frames
        variables.frame <- gframe(text="Variables Info",horizontal=FALSE,container=newpage)
        brushes.frame <- gframe(text="Brushes",horizontal=TRUE,container=newpage)
        optim.frame <- gframe(text="Optimization",horizontal=FALSE,container=newpage)
        buttons.frame <- ggroup(horizontal=TRUE,container=newpage)
        #Variables Frame
        glabel(paste("Display Variables are",dispnames[1],"and",dispnames[2],sep=" "), container = variables.frame)
        glabel(paste("Output Variables are",outnames[1],"and",outnames[2],sep=" "), container = variables.frame)
        #Brushes Frame
        allcols <- GGobiHandle$get.all.cols()
        labels.frame <- gframe("Colour",horizontal = FALSE, container=brushes.frame,expand=TRUE,spacing=18)
        radio.frame <- gframe("Set Type",horizontal=FALSE,container=brushes.frame,expand=TRUE,spacing=13)
        wt.frame <- gframe("Weight",horizontal=FALSE,container=brushes.frame,expand=TRUE,spacing=7)
        r.frame <- gframe("Radius",horizontal=FALSE,container=brushes.frame,expand=TRUE,spacing=7)
        n.frame <- gframe("Points",horizontal=FALSE,container=brushes.frame,expand=TRUE,spacing=7)
        button.frame1 <- gframe(horizontal=FALSE,container=brushes.frame,expand=TRUE)
        button.frame2 <- gframe(horizontal=FALSE,container=brushes.frame,expand=TRUE)
        for(i in 1:length(allcols))
          {
            glabel(allcols[i], container = labels.frame)
            set <- gradio(c("Unfixed","Fixed","Target"), container=radio.frame,
                          selected=1,#which(c("Unfixed set","Fixed Set","Target Set") %in% pair$get.type()),
                          horizontal=TRUE)
            wt <- gspinbutton(from=-1,to=1,by=0.1,value=0,container=wt.frame)
            r <- gspinbutton(from=0.5,to=3,by=0.1,value=0.8,container=r.frame)
            n <- gspinbutton(from=10,to=100,by=1,value=50,container=n.frame)
            but.set <- gbutton("Set",container=button.frame1,
                               handler=function(h,...)
                               {
                                 
                               })
            but.unset <- gbutton("Unset",container=button.frame2)
          }
        #Optim Frame
        neighbour <- ggroup(container=optim.frame)
        glabel("Number of projections to try in the neighbourhood",container=neighbour)
        addSpring(neighbour)
        button.n <- gspinbutton(from=3,to=100,by=1,value=5,container=neighbour)
        tries <- ggroup(container=optim.frame)
        glabel("Number of tries on failure",container=tries)
        addSpring(tries)
        button.tries <- gspinbutton(from=3,to=100,by=1,value=10,container=tries)
        slidehist <- ggroup(container=optim.frame,horizontal=TRUE)
        glabel("Go to previous views",container=slidehist)
        slide.hist <- gslider(from=0,to=1000,by=1,value=1,container=slidehist,expand=TRUE)
        #Buttons Frame
        glob.buts <- ggroup(container=buttons.frame)
        sync.but <- gbutton("Sync Output and Drawing Window",container=glob.buts)
        save.proj.but <- gbutton("Save Current Projection Matrix",container=glob.buts)
        opt.but <- gbutton("Optimize View",container=glob.buts)
        updateTab <- function()
          {
            
          }
        return(updateTab)
      }
    updateUI <- function() TRUE
    ###Getting things going
    Spawn <- function(h,...)
      {
        PairsHandle <<- pair.env(get(data))
        numviews <<- length(projs)
        views <<- vector('list',numviews)
        opthistory <<- vector('list',numviews)
        tabs <<- vector('list',numviews)
        frame <- make.frame(data,projs,slots)
        GGobiHandle <<- ggobi.manager(frame,data)
        for(i in 1:numviews)
          {
            opthistory[[i]] <<- get(projs[i])
            views[[i]] <<- PairsHandle$new.pair()
            tabs[[i]] <<- createTab(NoteBookHandle = nb,
                                    GGobiHandle = GGobiHandle,
                                    TabName=as.character(i),
                                    dispnames=colnames(frame)[c(2*i-1,2*i)],
                                    outnames=rep('dummy',2))
          }
      }
    
    ###Construct Base Window
    base <- gwindow('cepp Engine',visible=TRUE)
    choose.set <- gbutton("Choose Data Set",container=base,handler=chooseData)
    choose.matrices <- gbutton("Choose Projection Matrices",container=base,handler=chooseProjections)
    spawn.ggobi <- gbutton("Spawn GGobi",container=base,handler=Spawn)
    update.view <- gbutton("Update Views Window",container=base,handler=updateUI)
    enabled(choose.set) <- TRUE
    enabled(choose.matrices) <- FALSE
    enabled(spawn.ggobi) <- FALSE
    enabled(update.view) <- FALSE
    ###Construct Views Window
    views.win <- gwindow("Views Window", visible=TRUE)
    group0 <- ggroup(horizontal = FALSE, container=views.win)
    nb <- gnotebook(container = group0, expand=TRUE)
    ###Kick-Off
    update.base()
    ###returns
    return(list(get.data=get.data,
                get.proj=get.proj,
                get.slots=get.slots,
                get.GGobiHandle=get.GGobiHandle,
                get.PairsHandle=get.PairsHandle,
                get.views=get.views,
                get.opthistory=get.opthistory,
                get.tabs=get.tabs,
                get.wts=get.wts,
                get.numviews=get.numviews))
  }
