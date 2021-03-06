test.map2stan.eagles <- function() {
    data(eagles)
    d <- eagles
    d$PL <- ifelse( d$P=="L" , 1 , 0 )
    d$VL <- ifelse( d$V=="L" , 1 , 0 )
    d$PA <- ifelse( d$A=="A" , 1 , 0 )
    
    m <- map2stan(
      list(
        y~dbinom(n,p),
        logit(p) ~ a + (ba*PA) + (bv*VL) + (bp*PL) + (bpa*PL*PA),
        a~dnorm(0,10),
        bp~dnorm(0,5),
        bv~dnorm(0,5),
        ba~dnorm(0,5),
        bpa~dnorm(0,5)
      ),
      data=d, start=list(a=0, bp=0, bv=0, ba=0, bpa=0) , sample=TRUE )
  
  return(m)

}

test.map2stan.pois <- function() {
    
    data(islands)
    d <- islands
    d$log.pop <- log(d$Population)
    d$contact.high <- ifelse( d$Contact=="high" , 1 , 0 )
    f <- list(
            Total.Tools ~ dpois( lambda ) ,
            log(lambda) ~ a + bp*log.pop + bc*contact.high ,
            a ~ dnorm(3,10),
            bp ~ dnorm(0,1),
            bc ~ dnorm(0,1)
        )
    m <- map2stan( 
        f ,
        data=d , start=list(a=log(mean(d$Total.Tools)),bp=0,bc=0) )
        
    return(m)
}

test.map2stan.binom <- function() {
    data(UCBadmit)
    d <- UCBadmit
    d$admit2 <- as.numeric(d$admit)
    d$male <- ifelse( d$applicant.gender=="male" , 1 , 0 )
    d$deptB <- ifelse( d$dept=="B" , 1 , 0 )
    d$deptC <- ifelse( d$dept=="C" , 1 , 0 )
    d$deptD <- ifelse( d$dept=="D" , 1 , 0 )
    d$deptE <- ifelse( d$dept=="E" , 1 , 0 )
    d$deptF <- ifelse( d$dept=="F" , 1 , 0 )
    m <- map2stan( 
    list(
        admit2 ~ dbinom( applications , p ) ,
        logit(p) ~ a + bdB*deptB + bdC*deptC + bdD*deptD + bdE*deptE + bdF*deptF + bm*male
    ) , 
    data=d , 
    start=list(a=0,bm=0,bdB=0,bdC=0,bdD=0,bdE=0,bdF=0) ,debug=TRUE )
    return(m)
}