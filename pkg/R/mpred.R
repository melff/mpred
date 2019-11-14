#' Generic function to preduce predictive margins
#' 
#' @export
#' @import parallel
#' @import stats
#'
#' @param obj a model object, e.g. returned by \code{lm}, \code{glm}, etc.
#' @param settings an optional data frame of settings for independent
#'     variables. 
#' @param data an optional data frame for which the predictive margins are
#'     computed. If ommited, an attempt is made to obtain the data from the
#'     model object.
#' @param subset an optional logical vector that defines a subset for which a
#'     predictive margin is computed
#' @param groups a variable that defines groups for which predictive margines
#'     are computed. This variable has to have the same number of observations
#'     as the data to which the model was fitted.
#' @param setup an optional expression that is evaluated for each setting,
#'     i.e. individually for each row of the settings data frame. Can be
#'     used to modify independent variables.
#' @param cifunc a function to compute prediction intervals.
#' @param level level of confidence intervals of predictions.
#' @param parallel logical value that determines whether predictions for
#'     individual settings are computed in parallel. (Does not yet work on
#'     windows.)
#' @param mc.cores number of CPU cores used for parallel processing.
#' @param \dots optional vectors of values of independent variabls. These further
#'     arguments, if present, are used to create a data frame of settings,
#'     using \code{\link{expand.grid}}.
#' 
#' @details The generic function \code{predmarg} computes predictive
#'     margins for various settings of the independent variables. It is also
#'     possible to provide settings for independent variables that are
#'     included in the model, but that are used in the \code{setup}
#'     expression to transform independent variables. See the examples below.
#'
#' @return a data frame with the following variables:
#' \item{pred}{the mean prediction for the setting of the independent
#'     variables}
#' \item{var.pred}{the (estimated) variance of the mean prediction}
#' \item{se.pred}{the standard error of prediction, i.e. the square root of
#'     the variance of the mean prediction}
#' \item{lower}{lower prediction interval computed with \code{qfunc}}
#' \item{upper}{upper prediction interval computed with \code{qfunc}}
#' \item{\dots}{the independent variables for which values are set to create
#'     the predictions are also included in the resulting data frame.}
#'
#' @example examples/mpred-xmpl.R


predmarg <- function(obj,
                     settings,
                     data,
                     subset,
                     groups=NULL,
                     setup=NULL,
                     cifunc=cinorm,
                     level=0.95,
                     parallel=TRUE,
                     mc.cores=if(.Platform$OS.type == "windows") 1L
                              else max.cores,
                     ...) {

    if(missing(data)){
        
        data <- model.frame(obj)
        w <- weights(obj)
        if(!length(w))
            w <- rep(1,nrow(data))
        nact <- na.action(obj)
        
        if(length(nact)){
            data <- data[-nact,,drop=FALSE]
            w <- w[-nact]
        }
    }
    else {
        data <- model.frame(obj)
        w <- weights(obj)
    }

    if(!missing(groups)){
        groups <- substitute(groups)
        groups.name <- deparse(groups)
        groups <- eval(groups,data,parent.frame())
        attr(groups,"name") <- groups.name
    }
    else
        groups <- NULL

    if(!missing(subset)){
        subset <- eval(substitute(subset),data,parent.frame())
        if(!is.logical(subset)) stop("'subset' should be evaluate as logical")

        data <- data[subset,,drop=FALSE]
        w <- w[subset]
    }

    settings <- get_settings(data,
                             settings,
                             parent=parent.frame(),
                             ...)

    n <- nrow(data)
    m <- nrow(settings)

    i <- 1:n
    j <- 1:m

    data$.i <- i
    data$.w <- w
    settings$.j <- j

    if(!missing(setup))
        setup <- substitute(setup)
    else
        setup <- NULL

    if(parallel){

        max.cores <- detectCores(logical = TRUE)
        if(!length(max.cores))
            max.cores <- getOption("mc.cores",2L)
        else if(mc.cores > max.cores)
            message("\nMaximum number of cores available is ",max.cores,".")
        
        mc.cores <- min(mc.cores,max.cores)
        message("Using ",mc.cores," cores ...\n")

        smries <- mclapply(j,
                           predmarg1,
                           obj=obj,
                           data=data,
                           settings=settings,
                           groups=groups,
                           setup=setup,
                           cifunc=cifunc,
                           level=level,
                           parent=parent.frame(),
                           mc.cores=mc.cores)
    }
    else{
        
        smries <- lapply(j,
                         predmarg1,
                         obj=obj,
                         data=data,
                         settings=settings,
                         groups=groups,
                         setup=setup,
                         cifunc=cifunc,
                         level=level,
                         parent=parent.frame())
    }

    do.call(rbind,smries)
}

predmarg1 <- function(obj,...) UseMethod("predmarg1")

#' @export
predmarg1.default <- function(obj,
                              j,
                              data,
                              settings,
                              groups,
                              setup,
                              cifunc,
                              level,
                              parent){
    n <- nrow(data)
    settings.j <- settings[j,,drop=FALSE]
    nd1 <- setdiff(names(data),names(settings))
    newdata <- data.frame(data[,nd1],settings.j,
                          row.names=1:n)
    if(length(setup)){
        e <- evalq(environment(),newdata,parent)
        eval(setup,e)
        l <- as.data.frame(as.list(e))
        names.l <- names(l)
        newdata[names.l] <- l
    }
    w <- newdata$.w
    
    mu <- predict_response(obj,newdata)
    mu.theta <- mu_theta(obj,mu,newdata)
    cov.theta <- vcov(obj)

    if(!length(groups)){
        
        sum.w <- sum(w)
        mu.bar <- sum(w*mu)/sum.w
        mu.bar.theta <- colSums(w*mu.theta)/sum.w
        var.mu.bar <- mu.bar.theta %*% cov.theta %*% mu.bar.theta
        se.mu.bar <- sqrt(var.mu.bar)
        ci <- cifunc(mean=mu.bar,sd=se.mu.bar,level=level)
        lower <- ci$lower
        upper <- ci$upper
        
        res <- data.frame(pred=mu.bar,
                          var.pred=var.mu.bar,
                          se.pred=se.mu.bar,
                          lower=lower,
                          upper=upper)
        res <- cbind(res,settings.j)
    }
    else {
        
        if(length(groups)!=nrow(data))
            stop("'groups' argument has incorrect length.")
        groups.name <- attr(groups,"name") 
        
        ugrps <- unique(groups)      
        k <- match(groups,ugrps)
        sum.w.k <- drop(rowsum(w,k))
        mu.bar.k <- rowsum(w*mu,k)/sum.w.k
        mu.theta.bar.k <- rowsum(w*mu.theta,k)/sum.w.k
        var.mu.bar.k <- rowSums(mu.theta.bar.k*(mu.theta.bar.k%*%cov.theta))
        se.mu.bar.k <- sqrt(var.mu.bar.k)
        ci <- cifunc(mean=mu.bar.k,sd=se.mu.bar.k,level=level)
        lower <- ci$lower
        upper <- ci$upper
        res <- data.frame(pred=mu.bar.k,
                          var.pred=var.mu.bar.k,
                          se.pred=se.mu.bar.k,
                          lower=lower,
                          upper=upper)
        res[[groups.name]] <- ugrps
        res <- cbind(res,settings.j)
    }
    
    res$.j <- NULL
    res
}



predmarg1.default_multieq <- function(obj,
                                      j,
                                      data,
                                      settings,
                                      groups,
                                      setup,
                                      cifunc,
                                      level,
                                      parent){
    n <- nrow(data)
    settings.j <- settings[j,,drop=FALSE]
    nd1 <- setdiff(names(data),names(settings))
    newdata <- data.frame(data[,nd1],settings.j,
                          row.names=1:n)
    if(length(setup)){
        e <- evalq(environment(),newdata,parent)
        eval(setup,e)
        l <- as.data.frame(as.list(e))
        names.l <- names(l)
        newdata[names.l] <- l
    }
    w <- newdata$.w
    
    mu <- predict_response(obj,newdata)
    num.eqs <- ncol(mu)
    cov.theta <- vcov(obj)

    res <- list()
    
    if(!length(groups)){

        sum.w <- sum(w)
        mu.bar <- colSums(w*mu)/sum.w
        
        for(h in 1:num.eqs){

            mu.bar.h <- mu.bar[h]
            mu.theta <- mu_theta(obj,mu,newdata,h)
            mu.bar.theta <- colSums(w*mu.theta)/sum.w
            var.mu.bar <- mu.bar.theta %*% cov.theta %*% mu.bar.theta
            se.mu.bar <- sqrt(var.mu.bar)
            ci <- cifunc(mean=mu.bar.h,sd=se.mu.bar,level=level)
            lower <- ci$lower
            upper <- ci$upper

            res.h <- data.frame(pred=mu.bar.h,
                                var.pred=var.mu.bar,
                                se.pred=se.mu.bar,
                                lower=lower,
                                upper=upper,
                                eqnum=h)
            res.h <- cbind(res.h,settings.j)
            res.h$.j <- NULL
            res[[h]] <- res.h
        }
    }
    else {
        
        if(length(groups)!=nrow(data))
            stop("'groups' argument has incorrect length.")
        groups.name <- attr(groups,"name") 
        
        ugrps <- unique(groups)      
        k <- match(groups,ugrps)

        sum.w.k <- drop(rowsum(w,k))
        mu.bar.k <- rowsum(w*mu,k)/sum.w.k
        for(h in 1:num.eqs){

            mu.bar.hk <- mu.bar.k[,h]
            mu.theta <- mu_theta(obj,mu,newdata,h)
            mu.theta.bar.k <- rowsum(w*mu.theta,k)/sum.w.k
            var.mu.bar.k <- rowSums(mu.theta.bar.k*(mu.theta.bar.k%*%cov.theta))
            se.mu.bar.k <- sqrt(var.mu.bar.k)
            ci <- cifunc(mean=mu.bar.hk,sd=se.mu.bar.k,level=level)
            lower <- ci$lower
            upper <- ci$upper
            res.h <- data.frame(pred=mu.bar.hk,
                                var.pred=var.mu.bar.k,
                                se.pred=se.mu.bar.k,
                                lower=lower,
                                upper=upper,
                                eqnum=h)
            res.h[[groups.name]] <- ugrps
            res.h <- cbind(res.h,settings.j)
            
            res.h$.j <- NULL
            res[[h]] <- res.h
        }       
    }

    res <- do.call(rbind,res)
    if(length(colnames(mu)))
        res$response <- factor(res$eqnum,labels=colnames(mu))

    res
}

#' @export
predmarg1.mblogit <- function(obj,...) predmarg1.default_multieq(obj,...)

#' Confidence/prediction interval based on a normal distribution 
#' 
#' @export
#'
#' @param mean a scalar; the mean parameter of the normal distribution
#' @param sd a scalar; the standard deviation parameter of the normal distribution
#' @param level a scalar; the confidence level
cinorm <- function(mean,sd,level){
    alpha <- (1-level)/2
    p.lower <- alpha
    p.upper <- 1-alpha

    list(lower=qnorm(p=p.lower,mean=mean,sd=sd),
         upper=qnorm(p=p.upper,mean=mean,sd=sd))
}

get_settings <- function(data,
                         settings,
                         parent,
                         ...){

    if(missing(settings))
        settings <- eval(substitute(list(...)),data,parent)
    else
        settings <- eval(substitute(settings),data,parent)
    
    if(!is.data.frame(settings)){
        if(!is.list(settings)) stop("'settings' should be a data frame or a list")
        settings <- structure(lapply(settings,unique),
                              names=names(settings))
        settings <- expand.grid(settings)
    }

    settings
}

predict_response <- function(obj,data) UseMethod("predict_response")
#' @export
predict_response.lm <- function(obj,data) predict(obj,newdata=data,type="response")
#' @export
predict_response.glm <- function(obj,data) predict(obj,newdata=data,type="response")

#' @export
predict_response.mblogit <- function(obj,data)
    predict(obj,newdata=data,type="response")

#' @export
predict_response.mclogit <- function(obj,data)
    predict(obj,newdata=data,type="response")


mu_theta <- function(obj,mu,data,k) UseMethod("mu_theta")

#' @export
mu_theta.lm <- function(obj,mu,data,k){
    X <- model.matrix.default(obj,data=data)
    X
}

#' @export
mu_theta.glm <- function(obj,mu,data,k){
    X <- model.matrix.default(obj,data=data)
    na.act <- obj$na.action
    if(length(na.act))
        X <- X[-na.act,,drop=FALSE]
    coef <- coef(obj)
    eta <- X%*%coef
    mu_eta <- obj$family$mu.eta(eta)
    X*as.vector(mu_eta)
}

#' @export
mu_theta.mblogit <- function(obj,mu,data,k){
    X <- model.matrix.default(obj,data=data)
    na.act <- obj$na.action
    if(length(na.act))
        X <- X[-na.act,,drop=FALSE]
    coef <- coef(obj)
    nc <- names(coef)
    nc <- strsplit(nc,"~")
    ync <- sapply(nc,"[",1)
    xnc <- sapply(nc,"[",2)

    w <- -mu[,k]*mu
    if(k>1)
        w[,k]<-w[,k]+mu[,k]
    w <- w[,-1,drop=FALSE]
    h <- match(ync,unique(ync))
        
    w[,h]*X[,xnc]
}

#' @export
mu_theta.mclogit <- function(obj,mu,data,k){
    
    rhs <- obj$formula[-2]
    fo <- obj$formula
    lhs <- fo[[2]]
    if(deparse(lhs[[1]])=="cbind"){
        lhs <- lhs[[3]]
    }
    fo[[2]] <- lhs
    m <- model.frame(fo,data=data)
    set <- m[[1]]
    
    X <- model.matrix.default(rhs,m,
                              contasts.arg=obj$contrasts,
                              xlev=obj$xlevels
                              )
    na.act <- obj$na.action
    if(length(na.act))
        X <- X[-na.act,,drop=FALSE]
    cf <- coef(obj)
    ncf <- names(cf)
    X <- X[,ncf,drop=FALSE]
    
    set <- match(set,unique(set))

    wX <- mu*(X - rowsum(mu*X,set)[set,,drop=FALSE])

    wX
}
