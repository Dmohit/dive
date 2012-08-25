pair.env <- function(data)#All pairs reside in a single environment so that they can share data
  {
    data <- data
    pair <- function(rids=1:nrow(data),oth=NULL,type="unfixed")
      {
        #Data
        setd <- dset(rids)
        type <- type
        if(type=="unfixed" | is.null(oth==NULL))
          {
            oth <- data[setd$get.ids(),]
            for(i in 1:ncol(oth))
              {
                oth[,i] <- oth[sample.int(setd$number()),i]
              }
            seto <- oset(oth)
          }
        else if(setd$number() == nrow(oth) & ncol(oth) == ncol(data))
          seto <- oset(oth)
        else warning("Its usually safer to specify oth at time of initialization.")
        #Functions to interact with data
        set.data.ids <- function(ids)
          {
            setd$set.ids(ids)
            update.optimizer()
          }
        add.data.ids <- function(ids)
          {
            setd$add.ids(ids)
            update.optimizer()
          }
        rem.data.ids <- function(ids)
          {
            setd$rem.ids(ids)
            update.optimizer()
          }
        get.data.ids <- function()
          {
            setd$get.ids()
            update.optimizer()
          }
        get.data <- function() data[get.data.ids(),]
        construct.o.from.d <- function(mat=NULL)
          {
            if(type=="unfixed")
              {
                oth <- setd$get.data()
                for(i in 1:ncol(oth))
                  {
                    oth[,i] <- oth[sample.int(setd$number()),i]
                  }
                seto <<- oset(oth=oth)
                update.optimizer()
              }
            if(type=="fixed")
              {
                if(is.null(mat)) warning("Can't set oth for pair type matrix and null projection matrix!")
                else
                  {
                    oth <- setd$get.data()
                    seto <<- oset(oth=oth,mat=mat)
                    update.optimizer()
                  }
              }
            if(type=="target") warning("Cant automatically set Oset for pair type target!")
          }
        update.oth <- function(oth=NULL)
          {
            if(!is.null(oth))
              {
                seto$set.oth(oth)
                update.optimizer()
              }
          }
        update.oth.mat <- function(mat)
          seto$set.mat(mat)
        get.oth <- function() seto$get.oth()
        get.type <- function() type
        sanity <- function()
          {
            if(setd$number() != seto$number()) FALSE
            else if(n1 != n2)
              {
                warning("Optimizer not kept in sync")
                n1 <<- setd$number()
                n2 <<- seto$number()
                ev <<- evaluator(n=n1+n2,p=k)
              }
          }
        #Optimization Parameters (Sensible Defaults)
        r <- 0.8
        n <- 50
        k <- 2
        n1 <- setd$number()
        n2 <- seto$number()
        ev <- evaluator(n=n1+n2,p=k)
        srank.data <- matrix(0,nrow=n,ncol=k)
        srank.oth <- matrix(0,nrow=n,ncol=k)
        require(trust)
        #Functions to deal with optimization
        tune.optimization.parameters <- function(r=0.8,n=50,k=2)
          {
            r <<- r
            n <<- n
            k <<- k
            ev <<- evaluator(n=n1+n2,p=k)
            srank.data <<- matrix(0,nrow=n,ncol=k)
            srank.oth <<- matrix(0,nrow=n,ncol=k)
          }
        update.optimizer <- function()
        {
          n1 <<- setd$number()
          n2 <<- seto$number()
          ev <<- evaluator(n=n1+n2,p=k)
        }
       	index <- function(mat,volmult=FALSE)
          {
            mat	<- matrix(mat,ncol=k)	#sometimes not a matrix!
            sanity()
            if(oset$is.projected())
              {
                proj.d <- t(scale(get.data() %*% mat))
                proj <- cbind(proj.d,scale(get.oth()))
              }
            else
              {
                full <- as.matrix(rbind(get.data(),get.oth()))
                proj <-  t(scale(full %*% mat))
              }
            spmed <-  trust(ev,parinit=apply(proj,MARGIN=1,FUN=median),samp=t(proj),u=rep(0,k),rinit=1,rmax=2e5)
            tmax  <-  max(sqrt(colSums((proj - spmed$argument) ^ 2)))	#max distance of all points from spatial median
            ev.points <- cart * tmax * r
            ev.points <- t(ev.points) - spmed$argument
            for(i in 1:n)
              {
                one	<- proj - ev.points[,i]
                norms	<- sqrt(colSums(one^2))
                srank.data[i,]	<- colSums(t(one[,1:n1]) / norms[1:n1])
                srank.oth[i,]	<- colSums(t(one[,(n1+1):(n1+n2)]) / norms[(n1+1):(n1+n2)])
              }
            tmp <- (srank.data - srank.oth)
            if(volmult==TRUE)
              {
                vol <- ((sqrt(pi) * tmax) ^ (k/2)) / gamma(k/2 + 1)
                return(mean(given_norm(tmp)) * vol)
              }
            else return(mean(given_norm(tmp)))
          }
        return(list(set.data.ids=set.data.ids,add.data.ids=add.data.ids,get.data.ids=get.data.ids,rem.data.ids=rem.data.ids,
                    get.data=get.data,get.oth=get.oth,
                    update.oth=update.oth,update.oth.mat=update.oth.mat,construct.o.from.d=construct.o.from.d,
                    get.type=get.type,
                    sanity=sanity,
                    tune.optimization.parameters=tune.optimization.parameters,
                    update.optimizer=update.optimizer,
                    index=index))
      }
    return(pair)
  }
