pair.env <- function(data)
  {
    data <- data
    pair <- function(rids=1:nrow(data),oth=NULL,type="unfixed")
      {
        #Data
        setd <- dset(rids)
        type <- type
        if(type=="unfixed" & oth=NULL)
          {
            oth <- setd$get.data()
            for(i in 1:ncol(oth))
              {
                oth[,i] <- oth[sample.int(setd$number()),i]
              }
          }
        seto <- oset(oth)
        #Optimization Parameters (Sensible Defaults)
        r <- 0.8
        n <- 50
        k <- 2
        ev <- evaluator(n=n1+n2,p=k)
        srank.data <- matrix(0,nrow=n,ncol=k)
        srank.oth <- matrix(0,nrow=n,ncol=k)
        #End Optimization Parameters
        #Functions to interact with data
        set.data.ids <- function(ids) setd$bset.ids(ids)
        add.data.ids <- function(ids) setd$add.ids(ids)
        rem.data.ids <- function(ids) setd$rem.ids(ids)
        get.data.ids <- function() setd$get.ids()
        get.data <- function() data[get.data.ids(),]
        construct.o.from.d <- function(setd)
          {
            oth <- setd$get.data()
            for(i in 1:ncol(oth))
              {
                oth[,i] <- oth[sample.int(setd$number()),i]
              }
            seto <- oset(oth)
            return(seto)
          }
        update.oth <- function(oth=NULL)
          {
            if(oth!=NULL) seto$set.oth(oth)
            else if(type=="unfixed") seto$set.oth(construct.o.from.d(setd))
            else stop("Can't update oth")
          }
        get.oth <- function() oth[get.false.ids(),]
        pair.sanity <- function() setd$number() == seto$number()
        #Functions to deal with optimization
        tune.optimization.parameters <- function()
          {
          }
        return(list(set.data.ids=set.data.ids,add.data.ids=add.data.ids,get.data.ids=get.data.ids,rem.data.ids=rem.data.ids,
                    update.oth,construct.o.from.d,
                    get.data=get.data,get.oth=get.oth,
                    pair.sanity ))
      }
