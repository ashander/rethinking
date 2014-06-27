# coeftab

# coeftab class definition and show method
setClass( "coeftab" , representation( coefs="matrix" , se="matrix" , nobs="numeric" , AIC="numeric" , digits="numeric" , width="numeric" ) )

coeftab.show <- function( object ) {
    result <- object@coefs
    if ( !is.null(object@nobs) ) {
        result <- rbind( result , object@nobs )
        rownames(result)[ nrow(result) ] <- "nobs"
    }
    coefs <- rrformat( result , digits=object@digits , width=object@width )
    print( coefs , quote=FALSE , justify="right" )
}
setMethod( "show" , "coeftab" , function(object) coeftab.show(object) )

coeftab.plot <- function( x , y , pars , col.ci="black" , by.model=FALSE , prob=0.95, xlim=NULL, ... ) {
    x.orig <- x
    xse <- x@se
    x <- x@coefs
    if ( !missing(pars) ) {
        x <- x[pars,]
        xse <- xse[pars,]
    }
    if ( by.model==FALSE ) {
        xse <- t(xse)
        x <- t(x)
    }

    z <- qnorm( 1-(1-prob)/2 )
    
    left <- x
    right <- x
    for ( k in 1:nrow(x) ) {
        for ( m in 1:ncol(x) ) {
            ci <- x[k,m] + c(-1,1)*z*xse[k,m]
            left[k,m] <- ci[1]
            right[k,m] <- ci[2]
        }
    }

    if(length(xlim) < 2){
        llim <- min(left,na.rm=TRUE)
        rlim <- max(right,na.rm=TRUE)
        xlim <- c(llim,rlim)
    } else {
        llim <- xlim[1]
        rlim <- xlim[2]
    }
    dotchart( x , xlab="Estimate" , xlim=c(llim,rlim) , ... )
    
    for ( k in 1:nrow(x) ) {
        for ( m in 1:ncol(x) ) {
            if ( !is.na(left[k,m]) ) {
                # to compute y position:
                # coefs in groups by model
                # groups have nrow(x)+1 lines
                # empty line between each group
                kn <- nrow(x)
                ytop <- ncol(x)*(kn+2)-1
                ypos <- ytop - (m-1)*(kn+2) - (kn-k+1)
                lines( c(left[k,m],right[k,m]) , c(ypos,ypos) , lwd=2 , col=col.ci )
            }
        }
    }
    abline( v=0 , lty=1 , col=col.alpha("black",0.15) )
}
setMethod( "plot" , "coeftab" , function(x,y,...) coeftab.plot(x,y,...) )

coeftab <- function( ... , se=FALSE , se.inside=FALSE , nobs=TRUE , digits=2 , width=7 , rotate=FALSE , compare=FALSE, pars=NULL) {
    
    # se=TRUE outputs standard errors
    # se.inside=TRUE prints standard errors in parentheses in same column as estimates
    
    if ( se.inside==TRUE ) se <- TRUE
    
    # retrieve list of models

    # ISSUE - need to passing several models as list and manage row.name
    # ISSUE - more specifically: when loaded through devtools load_all,
    ##        the call through to xcoef ends up calling summary.default
    ##        instaed of the method for stanfit, when loaded through
    ##        > library(rstan)
    ##        > library(rethinking)
    ##        things seem to work OK.
    
    L <- list(...)

    if ( is.list(L[[1]]) && length(L)==1 ){
        L <- L[[1]]
        mnames <- names(L)
    } else if (is.list(L[[1]]) && length(L) > 1){
        ## retrieve model names from function call when not a list of models
        mnames <- match.call()
        mnames <- as.character(mnames)[2:(length(L)+1)]
    }


    
    # call xse and xcoef only ONCE
    coef.list <- lapply(L, function(l) xcoef( l , pars))
    se.list <-   lapply(L, function(l)  xse( l , pars))


    # count number of unique parameters
    param.names <- unique(c(sapply(coef.list, names)))

    ##    for ( i in 1:length(L) ) {
    ##        c.names <- names( coef.list[[i]] )
    ##        param.names <- unique( c( param.names , c.names ) )
    ##    }
    
    # columns for standard errors
    if ( se==TRUE && se.inside==FALSE ) {
        for ( i in 1:length(L) ) {
            kse.names <- paste( names( coef.list[[i]] ) , ".se" , sep="" )
            param.names <- unique( c( param.names , kse.names ) )
        }
    }

    # make empty table
    nk <- length(param.names)
    d <- matrix( NA , ncol=nk )
    d <- data.frame(d)
    colnames(d) <- c( param.names )
    dse <- d

    # loop over models and insert values
    for ( i in 1:length(L) ) {
        klist <- coef.list[[i]]
        selist <- se.list[[i]]
        for ( j in 1:length(klist) ) {
            ## changed both from d[i , ] [names(klist[j])] which produces error
            d[i, names( klist[j] ) ] <- as.numeric( round( klist[j] , digits ) ) 
            dse[i, names( klist[j] ) ] <- as.numeric( selist[j] )
        }
    }
    # insert standard errors
    if ( se==TRUE ) {
        for ( i in 1:length(L) ) {
            kse <- se.list[[i]]
            names(kse) <- names( coef.list[[i]] )
            for ( j in 1:length(kse) ) {
                if ( se.inside==FALSE )
                    # own column
                    d[i,][ paste(names(kse)[j],".se",sep="") ] <- as.numeric( round(kse[j],digits) )
                else
                    # combine with estimate
                    d[i,][ names(kse)[j] ] <- paste( formatC( as.real(d[i,][ names(kse)[j] ]) , digits=digits ) , " (" , formatC( as.real( kse[j] ) , digits=digits ) , ")" , sep="" )
            }
        }
    }
    
    # add model names to rows
    rownames(d) <- mnames
    
    # formatting for parenthetical standard errors
    if ( se.inside==TRUE && se==TRUE ) {
        comment(d) <- "Values in parentheses are quadratic estimate standard errors."
        colnames(d) <- paste( colnames(d) , "(se)" )
        for ( i in 1:nrow(d) ) {
            for ( j in 1:ncol(d) ) {
                d[i,j] <- ifelse( is.na(d[i,j]) , "" , d[i,j] )
            }
        }
    }
    
    # add nobs
    if ( nobs==TRUE ) {
        nobs <- sapply( L , xnobs )
    } else {
        nobs <- 0
    }
    
    # add AICc and weights
    if ( compare==TRUE ) {
        require(bbmle)
        d$AICc <- sapply( 1:length(L) , function(z) AICc( L[[z]] , nobs=d$nobs[z] ) )
        deltAICc <- d$AICc - min(d$AICc) 
        wAIC <- exp( -0.5*deltAICc )
        d$weight <- wAIC / sum( wAIC )
    }
    
    # return table
    if ( rotate==FALSE ) {
        d <- t(d) # models along top is default
        dse <- t(dse)
    }
    new( "coeftab" , coefs=as.matrix(d) , se=as.matrix(dse) , nobs=nobs , digits=digits , width=width )
}
