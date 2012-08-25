#this is a much stricter version that removes all low level interface functions
#All pairs reside in a single environment so that they can share data and norm definiton
pair.env <- function(data,gn=function(x) sqrt(rowSums(x^2)))
  {
    data <- data
    given_norm <- gn
    polar	<- gen_points(n=300,d=2)
    cart	<- angles(polar)
    
    get.data <- function() data
    get.polar.points <- function() polar
    get.cart.points <- function() cart
    
    pair <- function(rids=1:nrow(data),oth=NULL,type="unfixed",mat=NULL)
      {
        stopifnot(type %in% c('unfixed','fixed','target'))
        
        type <- type
        setd <- dset(rids)
        seto <- NULL

        get.o.from.d.unfixed <- function()
          {
            oth <- data[setd$get.ids(),]
            for(i in 1:ncol(oth))
              oth[,i] <- oth[sample.int(setd$number()),i]
            return(oth)
          }

        get.o.from.d.fixed <- function(mat=seto$get.mat)
          {
            oth <- data[setd$get.ids(),]
            return(oth=oth)            
          }
        
        if(type == 'unfixed') seto <- oset(get.o.from.d.unfixed(),mat=NULL)
        else if(type == 'fixed')   seto <- oset(get.o.from.d.fixed(),mat=mat)
        else if(type == 'target')
          {
            if(setd$number() == nrow(oth)) seto <- oset(oth=oth)
            else stop("Unequal number of points in Oset and Dset (type=target)!")
          }
        
        #Functions for safe interaction with sets        
        ## safely.remove.points <- function(ids)
        ##   {
        ##     if(type=='unfixed')
        ##       {
        ##         setd$rem.ids(ids)
        ##         seto$set.oth(get.o.from.d.unfixed())
        ##       }
        ##     if(type=='fixed')
        ##       {
        ##         setd$rem.ids(ids)
        ##         seto$set.oth(get.o.from.d.unfixed())
        ##       }
        ##     if(type=='target')
        ##       {
        ##         aids <- setd$get.ids()
        ##         indices <- which(!(ids %in% aids))
        ##         oth <- seto$get.oth()
        ##         oth2 <- oth[indices,]
        ##         seto$set.oth(oth)
        ##         setd$rem.ids(ids)
        ##       }
        ##   }
        
        ## update.oth.mat <- function(mat)  seto$set.mat(mat)
        
        #Functions to get information about sets
        get.data.ids <- function() setd$get.ids()
        
        get.data <- function() data[get.data.ids(),]
        
        get.oth <- function() seto$get.oth()
        
        get.type <- function() type
        
        #Ensuring Correctness of Sets
        
        sanity <- function()
          {
            if(setd$number() != seto$number()) FALSE
            else TRUE
          }
        
        #Optimization Parameters (Sensible Defaults)
        r <- 0.8
        n <- 50
        wt <- 1
        k <- 2
        n1 <- setd$number()
        n2 <- seto$number()
        ev <- evaluator(n=n1+n2,p=k)
        srank.data <- matrix(0,nrow=n,ncol=k)
        srank.oth <- matrix(0,nrow=n,ncol=k)
        require(trust)
        
                                        #Functions to deal with optimization
        get.optimization.parameters <- function()  return(list(r=r,n=n,wt=wt))
        
        tune.optimization.parameters <- function(r=0.8,n=50,k=2,wt=1)
          {
            r <<- r
            n <<- n
            wt <<- wt
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
            update.optimizer()
            if(seto$is.projected()==TRUE || type=='target')
              {
                proj.d <- t(get.data() %*% mat)
                proj <- cbind(proj.d,t(get.oth()))
              }
            else
              {
                full <- as.matrix(rbind(get.data(),get.oth()))
                proj <-  t(full %*% mat)
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
                return(mean(given_norm(tmp)) * vol * wt)
              }
            else return(mean(given_norm(tmp)) * wt)
          }
        return(list(get.data.ids=get.data.ids,
                    get.data=get.data,get.oth=get.oth,
                    #rem.points=safely.remove.points,
                    #update.oth.mat=update.oth.mat,
                    get.type=get.type,
                    sanity=sanity,
                    get.optimization.parameters=get.optimization.parameters,
                    tune.optimization.parameters=tune.optimization.parameters,
                    update.optimizer=update.optimizer,
                    index=index))
      }
    return(list(new.pair=pair,given_norm=given_norm,
                get.data=get.data,
                get.cart.points=get.cart.points,get.polar.points=get.polar.points))
  }
