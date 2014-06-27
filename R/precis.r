# my model summary function, précis

precis.whitelist <- data.frame( 
    class=c("map","map2stan","lm","glm","mle2","mer","bmer","polr","data.frame","clmm","clmm2","list","stanfit","lmerMod","glmerMod") , 
    coef.method=c("coef","coef","coef","coef","coef","fixef.plus","fixef.plus","polr","chain","coef","coef","mcarray","stanfit","fixef.plus","fixef.plus") , 
    vcov.method=c("vcov","vcov","vcov","vcov","vcov","vcov.VarCorr","vcov.VarCorr","vcov","chain","vcov","vcov","mcarray","stanfit","vcov.VarCorr","vcov.VarCorr") ,
    nobs.method=c("nobs","nobs","nobs","nobs","mle2","mer","mer","nobs","chain","nobs","nobs","chain","stanfit","mer","mer")
)

# precis class definition and show method
setClass( "precis" , representation( output="data.frame" , digits="numeric" ) )
precis.show <- function( object ) {
    print( round( object@output , object@digits ) )
}
setMethod( "show" , "precis" , function(object) precis.show(object) )

precis.plot <- function( x , y , pars , col.ci="black" , ... ) {
    x <- x@output
    if ( !missing(pars) ) {
        x <- x[pars,]
    }
    n <- nrow(x)
    mu <- x[n:1,1]
    left <- x[[3]][n:1]
    right <- x[[4]][n:1]
    dotchart( mu , labels=rownames(x)[n:1] , xlab="Estimate" , xlim=c(min(left),max(right)) , ... )
    for ( i in 1:length(mu) ) lines( c(left[i],right[i]) , c(i,i) , lwd=2 , col=col.ci )
    abline( v=0 , lty=1 , col=col.alpha("black",0.15) )
}
setMethod( "plot" , "precis" , function(x,y,...) precis.plot(x,y,...) )

precis <- function( model , pars , type.s=FALSE , ci=TRUE , level=0.95 , corr=FALSE , digits=2 , warn=TRUE ) {
    print("warning `pars` argument only works for stanfit class currently")
    the.class <- class(model)[1]
    found.class <- FALSE
    if ( the.class=="numeric" ) {
        # single vector of values
        # coerce to data frame
        model <- as.data.frame(model)
        the.class <- class(model)[1]
    }
    if ( any( precis.whitelist$class==the.class ) ) found.class <- TRUE
    if ( the.class=="list" )
        if ( class( model[[1]] ) != "mcarray" ) found.class <- FALSE
    if ( found.class==TRUE ) {
        est <- xcoef( model ,pars)
        se <- xse( model, pars)
        if ( corr==TRUE ) Rho <- xrho( model, pars)
    }
    if ( found.class==FALSE ) {
        message( paste("No handler found for model of class",the.class) )
        return(invisible())
    }
    # format
    result <- data.frame( est=est , se=se )
    colnames(result) <- c("Mean","StdDev")
    if ( ci==TRUE ) {
        ci <- confint.quad( est=est , se=se , level=level )
        if ( the.class=="data.frame" ) {
            # HPDI from samples
            ci <- t( apply( model , 2 , HPDI , prob=level ) )
        }
        if ( the.class=="map2stan" ) {
            # HPDI from samples
            post <- as.data.frame( extract.samples(model) )
            ci <- t( apply( post , 2 , HPDI , prob=level ) )
        }
        if ( the.class=="stanfit" ) {
            # HPDI from samples
            post <- as.data.frame( extract(model, pars=pars) )
            ci <- t( apply( post , 2 , HPDI , prob=level ) )
        }
        result <- cbind( result , ci )
    }
    if ( corr==TRUE ) {
        result <- cbind( result , Rho )
    }
    if ( type.s==TRUE )
        result[,"Pr(S)"] <- format.pval( type.s( est , se ) )
    if ( precis.whitelist$vcov.method[ precis.whitelist$class==the.class ]=="vcov.VarCorr" ) {
        message( "Quadratic approximation (standard errors) unreliable for variance components. Use MCMC to estimate precision of variance components." )
    }
    new( "precis" , output=result , digits=digits )
}

sfcoef <- function(m, p)
    summary(m)[['summary']][p , 1] ## WORKS


####
xcoef <- function( model, pars) {

    the.class <- class(model)[1]
    the.method <- precis.whitelist$coef.method[ precis.whitelist$class==the.class ]

    if ( the.method=="coef" ) {
        ## make pars
        result <- coef(model)
    }
    if ( the.method=="fixef" ) {
          ## make pars
        result <- fixef(model)
    }
    if ( the.method=="polr" ) {
                  ## make pars
        result <- summary(model)$coefficients[ ,1] 
    }
    if ( the.method=="chain" ) {
        # average of chains
                  ## make pars
        result <- apply( model , 2 , mean )
    }
    if ( the.method=="stanfit" ) {
        #print(class(model))
        #print(summary(model))

        result <- summary( model )[['summary']][pars , 1] ## works?!?
        names(result) <- pars
    }
    if ( the.method=="mcarray" ) {
        ## make pars
        
        # jags.samples result, hopefully
        result <- NULL
        result.names <- NULL
        for ( j in 1:length(model) ) {
            # explode compact arrays of coefficients
            dims <- dim( model[[j]] )
            if ( length(dims)==3 ) {
                est <- rep(0,dims[1])
                for ( k in 1:dims[1] ) {
                    est[k] <- mean( as.vector(model[[j]][ k , , ]) ) # marginalize over iteration and chain
                }
                result <- c( result , est )
                if ( dims[1] > 1 ) {
                    newnames <- paste( names(model)[j] , "[" , 1:dims[1] , "]" , sep="" )
                } else {
                    newnames <- names(model)[j]
                }
                result.names <- c( result.names , newnames )
            } # if dim 3
        } # for each array
        names(result) <- result.names
    }
    if ( the.method=="fixef.plus" ) {
        ## make pars
        
        # fixef from lmer plus variance components
        result <- fixef(model)
        vc <- VarCorr(model)
        clusters <- names(vc)
        for( i in 1:length(clusters) ) {
            sigma <- sqrt(diag(vc[[i]]))
            names(sigma) <- paste( rownames(vc[[i]]) , clusters[i] , sep="|" )
            names(sigma) <- paste( "(" , names(sigma) , ")" , sep="" )
            result <- c( result , sigma )
        }
        sigma.resid <- attr( vc , "sc" )
        if ( !is.na(sigma.resid) ) result['(residual)'] <- sigma.resid
    }
    xcheckconvergence( model )
    result
}

xcheckconvergence <- function( model ) {
    the.class <- class(model)[1]
    k <- 0
    if ( the.class=="mle2" ) {
        if ( model@details$convergence != 0 ) {
            k <- model@details$convergence
        }
    }
    if ( the.class=="map" ) {
        if ( model@optim$convergence != 0 ) {
            k <- model@optim$convergence
        }
    }
    
    if ( k > 0 ) {
        message( paste("Caution, model may not have converged.") )
        if ( k==1 ) {
            message( "Code 1: Maximum iterations reached." )
        }
        if ( k==10 ) {
            message( "Code 10: Degenerate Nelder-Mead simplex." )
        }
    }
}

xse <- function( model , pars) {
    the.class <- class(model)[1]
    the.method <- precis.whitelist$vcov.method[ precis.whitelist$class==the.class ]
    if ( the.method=="vcov" ) {
           ## make pars     
        result <- sqrt(diag(vcov(model)))
    }
    if ( the.method=="vcov.VarCorr" ) {
                ## make pars
        result <- sqrt(diag( as.matrix(vcov(model)) ))
        num.sigma <- length( xcoef(model, pars=NULL) ) - length( fixef(model) )
        result <- c( result , rep(NA,num.sigma) )
    }
    if ( the.method=="chain" ) {
                ## make pars
        
        # sd of chains
        result <- apply( model , 2 , sd )
    }
    if ( the.method=="stanfit" ) {
                ## make pars
        result <- summary( model )[['summary']][pars , 3] 
        names(result) <- pars
    }
    if ( the.method=="mcarray" ) {
        ## make pars
        
        # jags.samples result, hopefully
        result <- NULL
        result.names <- NULL
        for ( j in 1:length(model) ) {
            # explode compact arrays of coefficients
            dims <- dim( model[[j]] )
            if ( length(dims)==3 ) {
                est <- rep(0,dims[1])
                for ( k in 1:dims[1] ) {
                    est[k] <- sd( as.vector(model[[j]][ k , , ]) ) # marginalize over iteration and chain
                }
                result <- c( result , est )
                if ( dims[1] > 1 ) {
                    newnames <- paste( names(model)[j] , "[" , 1:dims[1] , "]" , sep="" )
                } else {
                    newnames <- names(model)[j]
                }
                result.names <- c( result.names , newnames )
            } # if dim 3
        } # for each array
        names(result) <- result.names
    }
    result
}

xrho <- function( model ) {
    the.class <- class(model)[1]
    the.method <- precis.whitelist$vcov.method[ precis.whitelist$class==the.class ]
    if ( the.method=="vcov" ) {
        ## make pars
        result <- cov2cor( vcov(model) ) 
    }
    if ( the.method=="vcov.VarCorr" ) {
        ## make pars
        result <- sqrt(diag( as.matrix(vcov(model)) ))
        num.sigma <- length( xcoef(model) ) - length( fixef(model) )
        result <- c( result , rep(NA,num.sigma) )
    }
    if ( the.method=="chain" ) {
        ## make pars
        # sd of chains
        result <- apply( model , 2 , sd )
    }
    if ( the.method=="stanfit" ) {
        ## make pars
        result <- summary( model )$summary[pars,3]
    }
    if ( the.method=="mcarray" ) {
        ## make pars
        # jags.samples result, hopefully
        result <- NULL
        result.names <- NULL
        for ( j in 1:length(model) ) {
            # explode compact arrays of coefficients
            dims <- dim( model[[j]] )
            if ( length(dims)==3 ) {
                est <- rep(0,dims[1])
                for ( k in 1:dims[1] ) {
                    est[k] <- sd( as.vector(model[[j]][ k , , ]) ) # marginalize over iteration and chain
                }
                result <- c( result , est )
                if ( dims[1] > 1 ) {
                    newnames <- paste( names(model)[j] , "[" , 1:dims[1] , "]" , sep="" )
                } else {
                    newnames <- names(model)[j]
                }
                result.names <- c( result.names , newnames )
            } # if dim 3
        } # for each array
        names(result) <- result.names
    }
    result
}

xnobs <- function( model ) {
    the.class <- class(model)[1]
    the.method <- precis.whitelist$nobs.method[ precis.whitelist$class==the.class ]
    if ( the.method=="nobs" ) result <- nobs(model)
    if ( the.method=="mle2" ) result <- length(model@data[[1]])
    if ( the.method=="mer" ) result <- nrow(model@frame)
    if ( the.method=="chain" ) result <- nrow(model)
    if ( the.method=="stanfit" ) result <- 0
    result
}

# row-by-row matrix formatting function
rrformat <- function( matrix , digits=2 , width=7 ) {
    if ( length(digits)==1 ) digits <- rep(digits,nrow(matrix))
    result <- matrix
    for ( i in 1:nrow(matrix) ) {
        result[i,] <- format( round(matrix[i,],digits[i]) , width=width )
    }
    result
}

