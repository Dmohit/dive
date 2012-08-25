widget.env <- function()
  {
    require(gWidgets)
    options("guiToolkit"="RGtk2")
    
    ###State Variables
    data <- NULL
    projs <- NULL
    GGobiHandle <- NULL
    PairsHandle <- NULL
    tabs <- NULL
    numtabs <- 0
    
    ###diagnostics
    get.data <- function() return(data)
    get.proj <- function() return(projs)
    get.GGobiHandle <- function() return(GGobiHandle)
    get.PairsHandle <- function() return(PairsHandle)
    get.tabs <- function() return(tabs)
    get.numtabs <- function() return(numviews)
    
    ###Updating Functions for base
    update.base <- function()
      {
        if(is.null(data) & is.null(projs))
          {
            enabled(choose.set) <- TRUE
            enabled(choose.matrices) <- FALSE
            enabled(spawn.ggobi) <- FALSE
            enabled(update.view) <- FALSE
            svalue(basestatusbar) <- "Please choose some data."
          }
        else if(!is.null(data) & is.null(projs))
          {
            enabled(choose.set) <- TRUE
            enabled(choose.matrices) <- TRUE
            enabled(spawn.ggobi) <- FALSE
            enabled(update.view) <- FALSE
            svalue(basestatusbar) <- "Please choose a few projection matrics."
          }
        else if(!is.null(data) & !is.null(projs))
          {
            enabled(choose.set) <- TRUE
            enabled(choose.matrices) <- TRUE
            enabled(spawn.ggobi) <- TRUE
            enabled(update.view) <- TRUE
            if(is.null(GGobiHandle)) svalue(basestatusbar) <- "Must spawn GGobi to proceed."
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
                         dispose(win)
                         update.base()
                       })
      }
    
    ###GUI functions for Views Window

    createTab <- function(NoteBookHandle,GGobiHandle,tabname,dispnames,outnames)
      {
                                        #State Variables
        NoteBookHandle <- NoteBookHandle
        GGobiHandle <- GGobiHandle
        viewpointer <- viewpointer
        TabName <- as.character(viewpointer)
        pairlist <- NULL
        opthistory <- NULL
        allcols <- GGobiHandle$get.all.cols()
                                        #List all pair specific buttons here
        set <- vector('list',length(allcols))
        wt  <- vector('list',length(allcols))
        r   <- vector('list',length(allcols))
        n   <- vector('list',length(allcols))
        lab <- vector('list',length(allcols))
                                        #Handler Functions for buttons
        button.handler.and.refresher <- function(h,...)
          {
            for(i in 1:length(allcols))
              {
                if(!GGobiHandle$exists.color(i))
                  {
                    enabled(set[[witch]]) <- FALSE
                    enabled(wt[[witch]]) <- FALSE
                    enabled(r[[witch]]) <- FALSE
                    enabled(n[[witch]]) <- FALSE
                    enabled(lab[[witch]]) <- FALSE
                  }
                else
                  {
                    
                  }
              }
          }

        newpage <- ggroup(horizontal = FALSE, container=NoteBookHandle, label=TabName)
                                        #The Frames
        variables.frame <- gframe(text="Variables Info",horizontal=FALSE,container=newpage)
        brushes.frame <- gframe(text="Brushes",horizontal=FALSE,container=newpage)
        optim.frame <- gframe(text="Optimization",horizontal=FALSE,container=newpage)
        buttons.frame <- ggroup(horizontal=TRUE,container=newpage)
                                        #Variables Frame
        glabel(paste("Display Variables are",dispnames[1],"and",dispnames[2],sep=" "), container = variables.frame)
        glabel(paste("Output Variables are",outnames[1],"and",outnames[2],sep=" "), container = variables.frame)
                                        #Brushes Frame
        choose.brushes.frame <- ggroup(horizontal=TRUE,container=brushes.frame)
        confirm.brushes.frame <- ggroup(horizontal=TRUE,container=brushes.frame)
        labels.frame <- gframe("Colour",horizontal = FALSE, container=choose.brushes.frame,expand=TRUE,spacing=16)
        radio.frame <- gframe("Set Type",horizontal=FALSE,container=choose.brushes.frame,expand=TRUE,spacing=12)
        wt.frame <- gframe("Weight",horizontal=FALSE,container=choose.brushes.frame,expand=TRUE)
        r.frame <- gframe("Radius",horizontal=FALSE,container=choose.brushes.frame,expand=TRUE)
        n.frame <- gframe("Points",horizontal=FALSE,container=choose.brushes.frame,expand=TRUE)
        allcols <- GGobiHandle$get.all.cols()
        for(i in 1:length(allcols))
          {
            lab[i] <- glabel(allcols[i], container = labels.frame)
            set[i] <- gradio(c("Unfixed","Fixed","Target"), container=radio.frame,selected=1,horizontal=TRUE,
                             handler=button.handler.and.refresher,action=i)
            wt[i] <- gspinbutton(from=-1,to=1,by=0.1,value=0,container=wt.frame,
                                 handler=button.handler.and.refresher,action=i)
            r[i] <- gspinbutton(from=0.5,to=3,by=0.1,value=0.8,container=r.frame,
                                handler=button.handler.and.refresher,action=i)
            n[i] <- gspinbutton(from=10,to=100,by=1,value=50,container=n.frame,
                                handler=button.handler.and.refresher,action=i)
          }
        addSpring(confirm.brushes.frame)
        but.confirm.brushes <- gbutton("Refresh Brushes",container=confirm.brushes.frame)
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
        button.hist <- gbutton("Go!",container=slidehist,
                               handler=function(h,...)
                               {
                                 witch <- svalue(slide.hist)
                                 if(witch > length(opthistory[viewpointer]))
                                   svalue(slide.hist) <-  length(opthistory[[viewpointer]])
                                 else
                                   {
                                     mat <- opthistory[[viewpointer]][witch]
                                     newvals <- get(data) %*% mat
                                     GGobiHandle$redraw(outnames[1],newvals[1])
                                     GGobiHandle$redraw(outnames[1],newvals[2])
                                   }
                               })
                                        #Buttons Frame
        glob.buts <- ggroup(container=buttons.frame)
        sync.od.but <- gbutton("Sync Windows: Output -> Drawing",container=glob.buts,
                               handler=function(h,...)
                               {
                                 GGobiHandle$redraw(dispnames[1],GGobiHandle$get.display.variable(outnames[1]))
                                 GGobiHandle$redraw(dispnames[2],GGobiHandle$get.display.variable(outnames[2]))
                               })
        sync.do.but <- gbutton("Sync Windows Drawing -> Output",container=glob.buts,
                            handler=function(h,...)
                            {
                              GGobiHandle$redraw(outnames[1],GGobiHandle$get.display.variable(dispnames[1]))
                              GGobiHandle$redraw(outnames[2],GGobiHandle$get.display.variable(dispnames[2]))
                            })
        #save.proj.but <- gbutton("Save Current Projection Matrix",container=glob.buts)
        opt.but <- gbutton("Optimize View",container=glob.buts)
                                        #Make Alive By adding Handlers
                                        #Ensure that Brushes Display is Valid
        setBrushesDisplay <- function()
          {
            brushes <- GGobiHandle$get.brush.colors()
            for(i in 1:length(allcols))
                {
                  if(allcols[i] %in% brushes)
                    {
                      enabled(lab[[i]]) <- TRUE
                      pairpointer <- GGHandle$GGobi.brush.to.pair(GGobiHandle,views[viewpointer],allcols[i])
                      if(pairpointer != FALSE)
                        {
                          enabled(set[[i]]) <- TRUE
                          enabled(wt[[i]]) <- TRUE
                          enabled(r[[i]]) <- TRUE
                          enabled(n[[i]]) <- TRUE
                          svalue(set[[i]]) <- views[viewpointer][[pairpointer]]$get.type()
                          svalue(wt[[i]]) <- views[viewpointer][[pairpointer]]$get.optimization.parameters()$wt
                          svalue(r[[i]]) <- views[viewpointer][[pairpointer]]$get.optimization.parameters()$r
                          svalue(n[[i]]) <- views[viewpointer][[pairpointer]]$get.optimization.parameters()$n
                        }
                      enabled(set[[i]]) <- TRUE
                      enabled(wt[[i]]) <- TRUE
                      enabled(r[[i]]) <- TRUE
                      enabled(n[[i]]) <- TRUE
                    }
                  else
                    {
                      enabled(lab[[i]]) <- FALSE
                      enabled(set[[i]]) <- FALSE
                      enabled(wt[[i]]) <- FALSE
                      enabled(r[[i]]) <- FALSE
                      enabled(n[[i]]) <- FALSE
                    }
                }
          }
        
        updateTab <- function()
          {
            setBrushesDisplay()
            setOptimizationDisplay()
          }

        disableTab <- function()
          {
            enabled(variables.frame) <- FALSE
            enabled(brushes.frame) <- FALSE
            enabled(optim.frame) <- FALSE
            enabled(buttons.frame) <- FALSE
          }

        enableTab <- function()
          {
            enabled(variables.frame) <- TRUE
            enabled(brushes.frame) <- TRUE
            enabled(optim.frame) <- TRUE
            enabled(buttons.frame) <- TRUE
          }

        return(list(dispID=dispnames,outID=outnames,updateTab=updateTab,disableTab=disableTab,enableTab=enableTab))
        
          }
    
    updateUI <- function(h,...)
      {
        win.id <- GGobiHandle$get.current.id()
        for(i in 1:numviews)
          {
            if(identical(tabs[[i]]$dispID , win.id) | identical(tabs[[i]]$outID  , win.id))
              {
                svalue(basestatusbar) <- paste("Tab",i,"is now active",sep=' ')
                tabs[[i]]$enableTab()
                tabs[[i]]$setfocus()
#               tabs[[i]]$updateTab()
              }
            else
              {
                tabs[[i]]$disableTab()
              }
          }
      }
    
    ###Getting things going
    Spawn <- function(h,...)
      {
        PairsHandle <<- pair.env(get(data))
        numtabs <<- length(projs)
        tabs <<- vector('list',numtabs)
        frame <<- make.frame(data,projs)
        GGobiHandle <<- ggobi.manager(frame,data)
        for(i in 1:numviews)
          {
            disp.index <- c(2*i-1,2*i)
            out.index <- disp.index + (numtabs*2)
            dispnames <- colnames(frame)[disp.index]
            outnames <- colnames(frame)[out.index]
            tabs[[i]] <<- createTab(NoteBookHandle = nb,
                                    GGobiHandle = GGobiHandle,
                                    tabname=as.character(i),
                                    dispnames=dispnames,
                                    outnames=outnames)
          }
        svalue(basestatusbar) <- "Click on \"Update Views Window\" to sync with GGobi."
      }
    
    ###Construct Base Window
    base <- gwindow('cepp Engine',visible=TRUE)
    choose.set <- gbutton("Choose Data Set",container=base,handler=chooseData)
    choose.matrices <- gbutton("Choose Projection Matrices",container=base,handler=chooseProjections)
    spawn.ggobi <- gbutton("Spawn GGobi",container=base,handler=Spawn)
    update.view <- gbutton("Update Views Window",container=base,handler=updateUI)
    basestatusbar <- gstatusbar("Please Choose a Data Set",container=base)
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
                get.GGobiHandle=get.GGobiHandle,
                get.PairsHandle=get.PairsHandle,
                get.views=get.views,
                get.opthistory=get.opthistory,
                get.tabs=get.tabs,
                get.numviews=get.numviews,
                get.frame=get.frame))
  }
