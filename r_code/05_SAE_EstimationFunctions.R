#------------------------------------------------------------------------------#
#
# Small Area Estimation (SAE) 
# for city statistics 
# and other functional areas part II  
#
# R-Code:
# Functions for the Estimation using Small Area Approaches
# 
# Authors:
# Ralf Münnich, Jan Pablo Burgard, Florian Ertz, 
# Simon Lenau, Julia Manecke, Hariolf Merkle
# Economic and Social Statistics Department Trier University
#
#------------------------------------------------------------------------------#

# Code explanation -------------------------------------------------------------
# 
# This code contains the respective function calls to perform small area 
# estimation according to the investigated strategies.
#
# These strategies, which are extensively described in Deliverable 2 and 3 of 
# this project, include:
#
# HT_N    Horvitz-Thompson estimator
# HT_w    Weighted sample mean
# CL      Horvitz-Thompson estimator at cluster-level (synthetic estimation)
# GREG    GREG estimator
# BHF     Battese-Harter-Fuller estimator
# FH      Fay-Herriot estimator
# YL      Ybarra-Lohr estimator based on the measurement error model
#
#-------------------------------------------------------------------------------

# HT_N    Horvitz-Thompson estimator ----

HT_N.fkt <- function(samp, DOI, level, dat){
    samp <- as.data.frame(samp)
    dat <- as.data.frame(dat)
  
    if (level == "H"){
        res <- tapply(samp$y * samp$weight, samp[,DOI], sum) /
            tapply(dat$HHS, dat[,DOI], sum)
    
  } else {
    res <- tapply(samp$y * samp$weight, samp[,DOI], sum) /
      table(dat[,DOI])
  }
  return(res)
}

# HT_w    Weighted sample mean ----

HT_w.fkt <- function(samp, DOI){
  samp <- as.data.frame(samp)
  
  res <- tapply(samp$y * samp$weight, samp[,DOI], sum) /
    tapply( samp$weight_Pers, samp[,DOI], sum)

  return(res)
}


# CL      Horvitz-Thompson estimator at cluster-level (synthetic estimation) ----

Clus.fkt <- function(samp, DOI, Clus){
  samp <- as.data.frame(samp)
  
  res <- (tapply(samp$y * samp$weight, samp[,DOI], sum) /
            tapply( samp$weight_Pers, samp[,DOI], sum))[Clus]
  
  
  return(res)
  
}


# GREG    GREG estimator ----


GREG.fkt <- function(formula, samp,dat, level, DOI, link){
  res <- vector(length= nrow(unique(na.omit(subset(dat, select=paste(DOI))))))
  
  dat <- data.table(dat)
  
  if (level == "H"){
    mod <- glm(formula, 
               data = samp, 
               weights = weight, 
               family = poisson()
    )
  } else {
    mod <- glm(formula, 
               data = samp, 
               weights = weight, 
               family = binomial(link = link)
    )
  }
  
  
  y_hat_U <- tapply(predict(mod, 
                            newdata = subset(dat, select = all.vars(formula)[-1]), 
                            type = "response"),
                    subset(dat, select=paste(DOI)), sum, na.rm=TRUE)
  
  y <- rep(0, nrow(samp))
  y[mod$na.action] <- NA
  y[!is.na(y)] <- mod$y
  
  fit <- rep(0, nrow(samp))
  fit[mod$na.action] <- NA
  fit[!is.na(fit)] <- mod$fitted.values
  
  BKT <- tapply((y-fit) * samp$weight, 
                (samp[,DOI , with = FALSE]), sum, na.rm=TRUE)
  
  N <- table(dat[,DOI, with = FALSE])
  
  
  N_Nhat <- N /
    tapply(samp$weight, samp[,DOI, with=FALSE], sum)
  
  
  tau_GREG <- y_hat_U + ifelse(is.na(BKT * N_Nhat), 0, BKT * N_Nhat) 
  
  mu_GREG <- tau_GREG / N
  
  
  res <-  mu_GREG[!is.na(mu_GREG)] 
  
  
  return(res)
}


# BHF     Battese-Harter-Fuller estimator ----

BHF.fkt <-function(formula, samp,dat, level, DOI){
  res <- vector(length= nrow(unique(na.omit(subset(dat, select=paste(DOI))))))
  
  dat <- data.table(dat)
  
  samp$DOI <- as.data.frame(samp[,DOI , with = FALSE])[,1]
  seldom <- levels(samp$DOI)
  meanXP_unsort <- as.data.frame(subset(dat, select = all.vars(formula)[-1])[, lapply(.SD, mean, na.rm=TRUE), by = dat[,DOI, with = FALSE]])
  meanXP <- meanXP_unsort[order(meanXP_unsort[,1]),]
  PSize <- cbind.data.frame(seldom, unname(table(dat[,DOI , with = FALSE])))[,c(1,3)]
  
  rownames(meanXP) <- NULL
  rownames(PSize) <- NULL
  
  res <- try(eblupBHF(formula, 
                      dom = DOI,
                      selectdom = seldom, 
                      meanxpop = meanXP, 
                      popnsize = PSize,
                      data = samp)$eblup$eblup,
             silent = TRUE)
  
  if(sum(class(res) == "try-error") == 1){
    return(rep(NA, nrow(unique(na.omit(subset(dat, select=paste(DOI)))))))
    
  } else {
    return(res)
  }
  
  
  
  #return (res)
}


# FH      Fay-Herriot estimator ----

# a) Functions to calculate variance of the HT AROPE estimator

# One important of the Fay-Herriot estimator is that the variance of the direct
# and design-based Horvitz-Thompson estimates, which are part of the Fay-Herriot
# estimator, is known.
# The non-trivial estimation of the variance for complex designs and the AROPE-
# rate as an example for non-linear indicators is treated by the functions below.
# The estimated variance of the point estimates is plugged in the Fay-Herriot 
# estimation function afterwards.

svyaroper <-
  function(inc.formula, 
           lwi.formula,
           dep.formula,
           design,
           quantiles = 0.5, 
           percent = 0.6, 
           na.rm=FALSE, 
           gamod = FALSE,
           ...) {
    
    
    # if( any( ! c(class(inc.formula),class(lwi.formula),class(dep.formula)) %in% "formula" )  )
    #   stop("All variable names must be given as formulas")
    
    if (is.null(attr(design, "full_design"))) 
      stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")
    
    
    formula <- nlme::asOneFormula(inc.formula,dep.formula,lwi.formula) 
    
    # domain
    vars <- model.frame(formula, design$variables, na.action = na.pass)
    
    #handeling of na's
    if(na.rm){
      nas    <- !complete.cases(incvar)
      design <- design[!nas,]
      if (length(nas) > length(design$prob)) 
        vars <- var[!nas,] else vars[nas,] <- 0
    }
    
    if( is.null( names( design$prob ) ) ) 
      ind <- as.character( seq( length( design$prob ) ) ) else ind <- names(design$prob)
    
    design$variables <-
      eval(parse(
        text =  paste0(
          'dplyr::rename(design$variables,
          inc = ',all.vars(inc.formula),',
          lwi = ',all.vars(lwi.formula),',
          dep = ',all.vars(dep.formula),
          ')'
        )
      ))
    
    
    #w <- 1/design$prob
    N <- sum(1/design$prob)
    
    
    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) 
      full_design <- design else full_design <- attr(design, "full_design")
    
    varsfull <- model.frame(formula, full_design$variables, na.action = na.pass)
    
    if(na.rm){
      nas    <- !complete.cases(incvar)
      full_design <- full_design[!nas,]
      if (length(nas) > length(full_design$prob)) 
        vars <- var[!nas,] else vars[nas,] <- 0
    }
    
    if( is.null( names( full_design$prob ) ) ) 
      ncom <- as.character( seq( length( full_design$prob ) ) ) else ncom <- names(full_design$prob)
    
    
    #################################################################################################
    
    full_design$variables <-
      eval(parse(
        text =  paste0(
          'dplyr::rename(full_design$variables,
          inc = ',all.vars(inc.formula),',
          lwi = ',all.vars(lwi.formula),',
          dep = ',all.vars(dep.formula),
          ')'
        )
      ))
    
    
    full_design$variables$g <-
      with(full_design$variables, 1 - lwi - dep + lwi * dep)
    design$variables$g <-
      with(design$variables, 1 - lwi - dep + lwi * dep)
    
    # for ARPT Estimation the full design is needed.
    ARPT   <- convey::svyarpt(formula =  ~inc, design=full_design, 
                              quantiles = quantiles, percent = percent, na.rm = na.rm,...)
    
    
    arptv   <- coef(ARPT)
    
    # add 'arope' variable to survey data
    design$variables$aroper <-
      with(design$variables, g * as.numeric(inc <= arptv) + (1 - g))
    
    #point estimation 
    AROPER  <- survey::svymean(~ aroper, design)
    rval    <- coef(AROPER)
    
    # if (naive) {
    #   return(AROPER)
    # }
    # 
    
    
    #linerized variance estimation
    
    ## estimating expected value of 'g' give 'inc=p*med'
    if (gamod) {
      ## with gam, but without weights at the moment
      g.mod  <-
        mgcv::gam(
          g ~ s(inc) ,
          family = 'binomial',
          optimizer = c("outer", "nlm"),
          design$variables,
          weights = weights(design)/sum(weights(design))
        )
    } else{
      ## with glm
      g.mod <- survey::svyglm(g ~ inc , family = 'binomial', design)
    }
    Eg_pth <-
      predict(g.mod , data.frame(inc = arptv), type = "response")
    
    if  (sum(1/design$prob==0) > 0) ID <- 1*(1/design$prob!=0)   else   ID <- 1 * ( ncom %in% ind )
    ## adding the values of the emp. influence function of arope to the survey data
    
    arope.lin <-
      with(
        full_design$variables,
        ( (g * as.numeric(inc <= arptv)*ID -
             percent *  as.numeric(Eg_pth) * (as.numeric(inc <= arptv/percent) - quantiles) +
             (1 - g)*ID ) - rval*ID ) / N*ID
      )
    
    #
    variance <-
      survey::svyrecvar(
        arope.lin/full_design$prob,
        full_design$cluster,
        full_design$strata,
        full_design$fpc,
        postStrata = full_design$postStrata
      )
    
    colnames( variance ) <- 
      rownames( variance ) <-  
      names( rval ) <- 
      paste0( all.vars( formula ), collapse = ".")
    
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "aroper"
    attr(rval, "lin") <- arope.lin
    rval
    
    # attr(AROPER, 'var') <-  SE(svytotal( ~ z, design)) ^ 2
    # 
    # AROPER
  }


a             <- function(beta, s2x){
  vapply(s2x,function(sig) crossprod(beta,crossprod(sig,beta)),numeric(1))  # s2x
}
b             <- function(beta, s2x, s2u, s2y){
  a (beta, s2x) + s2u + s2y
}
Vinv          <- function(beta, s2x, s2u, s2y){
  diag(1/b(beta, s2x, s2u, s2y))
}
G             <- function(Vinv,x){
  Vinvx <- crossprod(Vinv,x) #Vinv symmetric
  Vinvx%*%solve(crossprod(x,Vinvx))
}
P             <- function(Vinv,x,G){
  (diag(nrow(x))-tcrossprod(G,x))%*%Vinv
}

FHerrorREML_S <- function(y,P){
  -0.5*sum(diag(P)) + 0.5*tcrossprod(crossprod(y,P)) # P symmetric 
}
FHerrorREML_F <- function(y,P){
  P2 <- crossprod(P)   # P symmetric
  0.5*sum(diag(P2))
}

g1            <- function(beta,s2u,s2x,s2y){# page 12 g1
  atmp <- a(beta,s2x)
  ((atmp+s2u)*s2y)/(atmp+s2u+s2y)
}
g2            <- function(x,beta,s2u,s2x,s2y){# page 12 g2
  atmp <- a (beta,s2x)
  Vinv_tmp <- Vinv(beta, s2x, s2u, s2y)
  Q <- solve(crossprod(crossprod(Vinv_tmp,x),x)) # cheaper by crossprod(1/b*x,x)
  (s2y^2)/(atmp+s2u+s2y)^2 * apply(x,1,function(z)t(z)%*%Q%*%z) # richtig herumgerechnet ?
}
g3            <- function(x,beta,s2u,s2x,s2y){ # page 12 g3
  atmp <- a (beta,s2x)
  (s2y^2)/(atmp+s2u+s2y)^3 * 2/sum(1/(atmp + s2u + s2y)^2) }

gama          <- function(beta, s2x, s2u, s2y){
  atmp <- a (beta, s2x)
  (s2u + atmp) / (s2u + atmp + s2y)
}
FHerrorREML   <- function(y, x, s2y, s2x, s2u = NULL, eps = 1e-8, maxiter=500,verbose=FALSE){
  require(MASS)
  require(sae)
  
  if(is.null(s2x)) {
    warning("s2x was not set. Taking the x as observed without error.")
    s2x <- lapply(1:nrow(x),function(a) matrix(0,ncol(x),ncol(x)))
  }
  
  if (any(vapply(s2x,function(a)(any(diag(a)<0)|any(is.na(a))),logical(1))))
    stop ("All diagonal elements of s2x have to non-negative and may not contain any missings.")
  
  if (length(s2x) != nrow(x) | all(as.vector(vapply(s2x,dim,integer(2)))!=ncol(x)))
    stop ("x and s2x have to have same dimension")
  
  if (length(y)!=nrow(x) | length(y)!=length(s2y))
    stop ("length(y) must be equal to nrow(x) and equal to length(s2x)")
  
  ##### decide on Fixs2u
  if (!is.null(s2u)) Fixs2u <- TRUE else Fixs2u <- FALSE
  
  
  ##### Initial values
  # estimate a FH model
  if(FALSE){
    mod <- eblupFH(y ~ x[,-1],s2y,method="REML")
    beta <- beta.new <- mod$fit$estcoef[,1]
    
    if(is.null(s2u)) s2u <- s2u.new <- mod$fit$refvar[1] else s2u.old<-s2u
    if(s2u==0) s2u <- var(y)
  } else {
    beta <- beta.new <- coef(lm(y ~-1+ x))
    
    if(is.null(s2u)) s2u <- s2u.new <- median(s2y) else s2u.old<-s2u
    if(s2u==0) s2u <- median(s2y)
  }
  
  l <- function(y, x, beta, s2x, s2u, s2y){
    Vinv_tmp <- Vinv(beta, s2x, s2u, s2y)
    G_tmp <- G(Vinv_tmp,x)
    P_tmp <- P(Vinv_tmp,x,G_tmp)
    btmp <-  b (beta, s2x, s2u, s2y)
    -(length(y)-length(beta))/2*log(2*pi) + 0.5 * log(det(t(x)%*%x)) - 0.5 * log(prod(btmp)) - 0.5 * log(det(t(x)%*%Vinv_tmp%*%x)) - 0.5 * t(y)%*%P_tmp%*%y
  }
  
  beta.old <- 5 * beta.new
  s2u.old  <- s2u.new  <- if(Fixs2u) s2u else 5*s2u
  
  ##### begin optimization
  iter <- 0
  converged <- TRUE
  while((any(abs((beta.new - beta.old)/beta.old) > eps) | abs((s2u.new-s2u.old)/s2u.old) > eps)   &  iter < maxiter){
    # not sufficient convergence in beta has to be checked as well, only checking the s2u leads to worse results
    iter      <- iter + 1
    beta.old  <- beta.new
    s2u.old   <- s2u.new
    Vinv_tmp  <- Vinv(beta.old, s2x, s2u.old, s2y)
    
    if(!Fixs2u){
      G_tmp <- G(Vinv_tmp,x)
      P_tmp <- P(Vinv_tmp,x,G_tmp)
      s2u.new <- as.vector( s2u.old + solve(FHerrorREML_F(y,P_tmp))%*%FHerrorREML_S(y,P_tmp) )
      
      if(s2u.new<=0) {
        warning("negative or zero restricted maximum-liklihood estimate for sigma_u^2. Taking values of previous iteration.")
        s2u.old <- s2u.new <- 0
        converged <- FALSE
        break;
      }
      
      Vinv_tmp <- Vinv(beta.old, s2x, s2u.new, s2y)
      
    }
    
    Vinvx    <- crossprod(Vinv_tmp,x)
    beta.new <- as.vector(solve(a = crossprod(x,Vinvx), b = crossprod(Vinvx,y)))
    
    if(verbose){
      verb <- formatC(c(beta.old,s2u.old,l(y, x, beta.old, s2x, s2u.old, s2y)),format="f",digits = 8)
      if(iter==1) cat(formatC(c(paste0("beta",1:length(beta.old)),"s2u","loglik"),width=10),"\n")
      cat(verb,"\n")
    }
  }
  beta  <- beta.new
  s2u   <- s2u.new
  Vinv_tmp <- Vinv(beta.old, s2x, s2u.new, s2y)
  Vinvx <- crossprod(Vinv_tmp,x)
  mse <- g1(beta,s2u,s2x,s2y) + g2(x,beta,s2u,s2x,s2y) +
    2*g3(x,beta,s2u,s2x,s2y)
  
  gam <- gama(beta, s2x, s2u, s2y)
  eblup <- gam * y + (1-gam) * x%*%beta
  
  ##########################################
  Q <- solve(crossprod(x,Vinvx))
  std.errorbeta <- sqrt(diag(Q))
  tvalue <- beta/std.errorbeta
  pvalue <- 2 * pnorm(abs(tvalue), lower.tail = FALSE)
  Xbeta.REML <- x %*% beta
  resid <- y - Xbeta.REML
  loglike <- l(y, x, beta, s2x, s2u, s2y)
  p <- ncol(x)-1
  m <- nrow(x)
  AIC <- (-2) * loglike + 2 * (p + 1)
  BIC <- (-2) * loglike + (p + 1) * log(m)
  goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
  coefs <- data.frame(beta = beta, std.error = std.errorbeta,
                      tvalue, pvalue)
  
  
  #########################################
  
  
  
  
  
  
  if(iter==maxiter) {
    warning("did not converge")
    converged<-FALSE
  }
  list(parameters = list(iter=iter,beta=beta,s2u=s2u,gamma=gam,converged=converged,coef=coefs,goodness=goodness), eblup=as.vector(eblup), mse=mse) 
}


# b) Main estimation function of the Fay-Herriot estimator
FH.fkt <- function(formula, dat, which) {
  
  mod_dat <- model.frame(formula, data = dat)
  
  res <-  rep(NA, nrow(dat))
  
  mod <- eblupFH(formula,
                 vardir = AROPE_vest,
                 data = dat[which,])
  
  MF <- model.matrix(formula, data = model.frame(formula, data = dat, na.action = NULL))
  #MF[,1] <- 1
  
  
  res[is.element(seq(1:length(res)), which)] <- mod$eblup
  res[!is.element(seq(1:length(res)), which)] <- as.matrix(MF[!is.element(seq(1:length(res)), which),]) %*% 
    mod$fit$estcoef$beta
  
  return(res)
  
}






# YL      Ybarra-Lohr estimator based on the measurement error model ----


YL_REML <- function(which, data, s2x, X_hat){
  
  res <-  rep(NA, nrow(data))
  
  YL <- FHerrorREML(y = data[which,]$AROPE_pest,
                    x = (cbind(1,X_hat[which,])),
                    #model.matrix( ~ ISCED56 + UER + COB_LOC , data_CIT[which(!is.na(data_CIT$AROPE_pest)),]),
                    s2y = data[which,]$AROPE_vest,
                    s2u = NULL,
                    s2x = s2x[which])
  
  res[is.element(seq(1:length(res)), which)] <- YL$eblup
  res[!is.element(seq(1:length(res)), which)] <- (cbind(1,X_hat[!is.element(seq(1:1580), which(!is.na(data$AROPE_pest))),])) %*%
    YL$parameters$beta
  
  return(res)
}

