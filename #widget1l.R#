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
    get.numtabs <- function() return(numtabs)
    
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

    createTab <- function(NoteBookHandle,GGobiHandle,tabname,dispnames,outnames,proj)
      {
                                        #GUI State Variables
        NoteBookHandle <- NoteBookHandle
        GGobiHandle <- GGobiHandle
        TabName <- tabname
        dispnames <- dispnames
        outnames <- outnames
                                        #Internal State Variables
        pairlist <- vector('list',1)
        opthistory <- vector('list',1)
        opthistory[[1]] <- proj
        paintpointer <- 1
        outpointer <- 1
        allcols <- GGobiHandle$get.all.cols()
        pairlist[[1]] <- PairsHandle$new.pair()
                                        #Querying Internal State
        get.opthistory <- function() opthistory
        get.pairlist <- function() pairlist
        get.paintpointer <- function() paintpointer
        get.outpointer <- function() outpointer
                                        #List all pair specific buttons here
        set <- vector('list',length(allcols))
        wt  <- vector('list',length(allcols))
        r   <- vector('list',length(allcols))
        n   <- vector('list',length(allcols))
        lab <- vector('list',length(allcols))
                                        #Drawing GGobi
        draw.paint.window <- function(index)
          {
            if(index > length(opthistory)) return(FALSE)
            paintpointer <<- index
            paint.data <- get(data) %*% opthistory[[index]]
            GGobiHandle$redraw(dispnames,paint.data)
          }

        draw.output.window <- function(index)
          {
            if(index > length(opthistory)) return(FALSE)
            outpointer <<- index
            paint.data <- get(data) %*% opthistory[[index]]
            GGobiHandle$redraw(outnames,paint.data)
          }
                                        #Basic Initializations
        draw.paint.window(1)
        draw.output.window(1)
                                        #Handler Functions for buttons
        brush.handler.and.refresher <- function(h,...)
          {
            lstpointer <- 1
            for(i in 1:length(allcols))
              {
                if(GGobiHandle$exists.color(allcols[i]) == FALSE)
                  {
                    enabled(set[[i]]) <- FALSE
                    enabled(wt[[i]]) <- FALSE
                    enabled(r[[i]]) <- FALSE
                    enabled(n[[i]]) <- FALSE
                    enabled(lab[[i]]) <- FALSE
                  }
                else
                  {
                    enabled(set[[i]]) <- TRUE
                    enabled(wt[[i]]) <- TRUE
                    enabled(r[[i]]) <- TRUE
                    enabled(n[[i]]) <- TRUE
                    enabled(lab[[i]]) <- TRUE
                    
                    ids <- GGobiHandle$get.color.ids(allcols[i])
                                        #Verify Movement
                    movetol <- 1e-2
                    rownorms <- function(x) apply(x,MARGIN=1,FUN=function(x) sqrt(sum(x^2)))
                    true <- get(data) %*% opthistory[[paintpointer]]
                    painted <- as.matrix(GGobiHandle$get.color.vars(allcols[i],dispnames))
                    if(any(abs(rownorms(true[ids,]) - rownorms(painted)) > movetol))
                      {
                        set[[i]][] <- c('Target')
                        svalue(set[[i]]) <- 'Target'
                        pairlist[[lstpointer]] <<- PairsHandle$new.pair(rids = ids,
                                                                       oth = painted,
                                                                       type='target')
                        pairlist[[lstpointer]]$tune.optimization.parameters(r=svalue(r[[i]]),
                                                                            n=svalue(n[[i]]),
                                                                            wt=svalue(wt[[i]]))
                        lstpointer <- lstpointer + 1
                      }
                    else
                      {
                        if(svalue(set[[i]]) == 'Target')
                          {
                            set[[i]][] <- c("Unfixed","Fixed","Target")
                            svalue(set[[i]]) <- 'Unfixed'
                          }
                        if(svalue(set[[i]]) == 'Unfixed')
                          {
                            pairlist[[lstpointer]] <<- PairsHandle$new.pair(rids = ids,
                                                                           type='unfixed')
                            pairlist[[lstpointer]]$tune.optimization.parameters(r=svalue(r[[i]]),
                                                                                n=svalue(n[[i]]),
                                                                                wt=svalue(wt[[i]]))
                            lstpointer <- lstpointer + 1
                          }
                        if(svalue(set[[i]]) == 'Fixed')
                          {
                            pairlist[[lstpointer]] <<- PairsHandle$new.pair(rids = ids,
                                                                           mat = opthistory[[paintpointer]],
                                                                           type='fixed')
                            pairlist[[lstpointer]]$tune.optimization.parameters(r=svalue(r[[i]]),
                                                                                n=svalue(n[[i]]),
                                                                                wt=svalue(wt[[i]]))
                            lstpointer <- lstpointer + 1                             
                          }
                      }
                  }
              }
          }

        optimView <- function(h,...)
          {
            num.new.bases <- 5 #HARDCODED!!!
            brush.handler.and.refresher()
            tmp <- better.view(lst=pairlist,
                               start=opthistory[[outpointer]],
                               bases=num.new.bases,
                               n=svalue(button.n),
                               max.tries=svalue(button.tries))
            if(is.null(tmp[[1]]))
              {
                gmessage("No better bases could be found. \n(You might want to tinker with the optimization controls perhaps?)")
              }
            else
              {
                opthistory <<- c(opthistory[1:outpointer],tmp)
                if(is.null(opthistory[[length(opthistory)]])) opthistory <<- opthistory[1:(length(opthistory)-1)]
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
                             handler=brush.handler.and.refresher,action=i)
            wt[i] <- gspinbutton(from=-1,to=1,by=0.1,value=0,container=wt.frame,
                                 handler=brush.handler.and.refresher,action=i)
            r[i] <- gspinbutton(from=0.5,to=3,by=0.1,value=0.8,container=r.frame,
                                handler=brush.handler.and.refresher,action=i)
            n[i] <- gspinbutton(from=10,to=100,by=1,value=50,container=n.frame,
                                handler=brush.handler.and.refresher,action=i)
          }
        addSpring(confirm.brushes.frame)
        but.confirm.brushes <- gbutton("Refresh Brushes",container=confirm.brushes.frame,handler=brush.handler.and.refresher)
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
        glabel("Go to previous output views",container=slidehist)
        slide.hist <- gslider(from=1,to=1000,by=1,value=1,container=slidehist,expand=TRUE,
                              handler = function(h,...)
                              {
                                if(svalue(slide.hist) > length(opthistory)) svalue(slide.hist) <- length(opthistory)
                                outpointer <<- svalue(slide.hist)
                                slide.hist[] <- seq(1,length(opthistory),1)
                                draw.output.window(outpointer)
                              })
                                                                      #Buttons Frame
        glob.buts <- ggroup(container=buttons.frame)
        sync.od.but <- gbutton("Sync Windows: Output -> Drawing",container=glob.buts,
                               handler=function(h,...)
                               {
                                 paintpointer <<- outpointer
                                 draw.paint.window(outpointer)
                               })
        addSpring(glob.buts)
        opt.but <- gbutton("Optimize View",container=glob.buts,handler=optimView)
        
        updateTab <- function()
          {
            brush.handler.and.refresher()
#            setOptimizationDisplay()
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
            updateTab()
          }

        return(list(dispID=dispnames,outID=outnames,
                    get.pairlist=get.pairlist,get.opthistory=get.opthistory,
                    get.paintpointer=get.paintpointer,get.outpointer=get.outpointer,
                    updateTab=updateTab,disableTab=disableTab,enableTab=enableTab))
        
      }
    
    updateUI <- function(h,...)
      {
        win.id <- GGobiHandle$get.current.id()
        for(i in 1:numtabs)
          {
            if(identical(tabs[[i]]$dispID , win.id) | identical(tabs[[i]]$outID  , win.id))
              {
                svalue(basestatusbar) <- paste("Tab",i,"is now active",sep=' ')
                tabs[[i]]$enableTab()
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
        #Basics
        numtabs <<- length(projs)
        tabs <<- vector('list',numtabs)
        #Frame for GGobi
        projdim <- ncol(get(projs[1]))
        xnames <- paste('x',1:(projdim * numtabs),sep='')
        ynames <- paste('y',1:(projdim * numtabs),sep='')
        frame <- matrix(0,nrow=nrow(get(data)),ncol=projdim * numtabs * 2)
        colnames(frame) <- 1:ncol(frame)
        colnames(frame)[seq(1,ncol(frame),2)] <- xnames
        colnames(frame)[seq(2,ncol(frame),2)] <- ynames
        #Invoke ggobi and internals
        PairsHandle <<- pair.env(get(data))
        GGobiHandle <<- ggobi.manager(frame,data)
        #Invoke Tabs
        for(i in 1:numtabs)
          {
            disp.index <- c(2*i-1,2*i)
            out.index <- disp.index + (numtabs*2)
            dispnames <- colnames(frame)[disp.index]
            outnames <- colnames(frame)[out.index]
            tabs[[i]] <<- createTab(NoteBookHandle = nb,
                                    GGobiHandle = GGobiHandle,
                                    tabname=as.character(i),
                                    dispnames=dispnames,
                                    outnames=outnames,
                                    proj=get(projs[i]))
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
                get.tabs=get.tabs,
                get.numtabs=get.numtabs))
  }
