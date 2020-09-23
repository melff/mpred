#' Generic function to preduce predictive margins
#' 
#' @export
#' @import parallel
#' @import stats
#'
#' @param obj a model object, e.g. returned by \code{lm}, \code{glm}, etc.
#' @param settings an optional data frame of settings for independent variables.
#' @param data an optional data frame for which the predictive margins are
#'     computed. If ommited, an attempt is made to obtain the data from the
#'     model object.
#' @param subset an optional logical vector that defines a subset for which a
#'     predictive margin is computed
#' @param groups a variable that defines groups for which predictive margines
#'     are computed. This variable has to have the same number of observations
#'     as the data to which the model was fitted.
#' @param setup an optional expression that is evaluated for each setting,
#'     i.e. individually for each row of the settings data frame. Can be used to
#'     modify independent variables.
#' @param type an optional character string that specifies the type of
#'     predictions, e.g. probabilities or cumulative probabilites. For future
#'     versions only.
#' @param cifunc a function to compute prediction intervals. By default it is
#'     the chosen by the function of the same name.
#' @param level level of confidence intervals of predictions.
#' @param parallel logical value that determines whether predictions for
#'     individual settings are computed in parallel. (Does not yet work on
#'     windows.)
#' @param mc.cores number of CPU cores used for parallel processing.
#' @param \dots optional vectors of values of independent variabls. These
#'     further arguments, if present, are used to create a data frame of
#'     settings, using \code{\link{expand.grid}}.
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
                     type = NULL,
                     groups=NULL,
                     setup=NULL,
                     cifunc=get_cifunc(obj),
                     level=0.95,
                     parallel=FALSE,
                     mc.cores=if(.Platform$OS.type == "windows") 1L
                              else max.cores,
                     ...) {

    cifunc <- force(cifunc)
    
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
                           type=type,
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
                         type=type,
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
                              type,
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
    
    mu <- predict_response(obj,newdata,type=type)
    mu.theta <- attr(mu,"Jacobian")
    cov.theta <- vcov(obj)

    if(!length(groups)){
        
        sum.w <- sum(w)
        mu.bar <- sum(w*mu)/sum.w
        if(length(mu.theta)){
            mu.bar.theta <- colSums(w*mu.theta)/sum.w
            var.mu.bar <- mu.bar.theta %*% cov.theta %*% mu.bar.theta
            se.mu.bar <- sqrt(var.mu.bar)
            ci <- cifunc(mean=mu.bar,sd=se.mu.bar,level=level)
            lower <- ci$lower
            upper <- ci$upper
        }
        else {
            var.mu.bar <- NA
            se.mu.bar <- NA
            lower <- NA
            upper <- NA
        }
        
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
        if(length(mu.theta)){
            mu.theta.bar.k <- rowsum(w*mu.theta,k)/sum.w.k
            var.mu.bar.k <- rowSums(mu.theta.bar.k*(mu.theta.bar.k%*%cov.theta))
            se.mu.bar.k <- sqrt(var.mu.bar.k)
            ci <- cifunc(mean=mu.bar.k,sd=se.mu.bar.k,level=level)
            lower <- ci$lower
            upper <- ci$upper
        }
        else {
            var.pred <- NA
            se.pred <- NA
            lower <- NA
            upper <- NA
        }
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
                                      type,
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
    
    mu <- predict_response(obj,newdata,type=type)
    mu.theta <- attr(mu,"Jacobian")
    num.eqs <- ncol(mu)
    cov.theta <- vcov(obj)

    res <- list()
    
    if(!length(groups)){

        sum.w <- sum(w)
        mu.bar <- colSums(w*mu)/sum.w
        
        for(h in 1:num.eqs){

            mu.bar.h <- mu.bar[h]
            if(length(mu.theta)){
                mu.theta.h <- mu.theta[[h]]
                mu.bar.theta.h <- colSums(w*mu.theta.h)/sum.w
                var.mu.bar.h <- mu.bar.theta.h %*% cov.theta %*% mu.bar.theta.h
                se.mu.bar.h <- sqrt(var.mu.bar.h)
                ci <- cifunc(mean=mu.bar.h,sd=se.mu.bar.h,level=level)
                lower <- ci$lower
                upper <- ci$upper
            }

            res.h <- data.frame(pred=mu.bar.h,
                                var.pred=var.mu.bar.h,
                                se.pred=se.mu.bar.h,
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
            if(length(mu.theta)){
                mu.theta.h <- mu.theta[[h]]
                mu.theta.bar.hk <- rowsum(w*mu.theta.h,k)/sum.w.k
                var.mu.bar.hk <- rowSums(mu.theta.bar.hk*(mu.theta.bar.hk%*%cov.theta))
                se.mu.bar.hk <- sqrt(var.mu.bar.hk)
                ci <- cifunc(mean=mu.bar.hk,sd=se.mu.bar.hk,level=level)
                lower <- ci$lower
                upper <- ci$upper
            }
            else {
                var.mu.bar.hk <- NA
                se.mu.bar.hk <- NA
                lower <- NA
                upper <- NA
            }
            res.h <- data.frame(pred=mu.bar.hk,
                                var.pred=var.mu.bar.hk,
                                se.pred=se.mu.bar.hk,
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

predict_response <- function(obj,data,...) UseMethod("predict_response")

model_matrix <- function(obj,data){
    na.act <- obj$na.action
    trms <- terms(obj)
    trms <- delete.response(trms)
    mf <- model.frame(trms,data,xlev=obj$xlevels)
    model.matrix(trms,data=mf,contrasts.arg=obj$contrasts)
}

#' @export
predict_response.lm <- function(obj,data,...){
    X <- model_matrix(obj,data)
    coef <- coef(obj)
    mu <- X%*%coef
    structure(mu,Jacobian=X)
}

#' @export
predict_response.glm <- function(obj,data,...){
    #prd <- predict(obj,newdata=data,type="response")
    X <- model_matrix(obj,data)
    coef <- coef(obj)
    eta <- X%*%coef
    mu <- obj$family$linkinv(eta)
    mu_eta <- obj$family$mu.eta(eta)
    Jacobian <- X*as.vector(mu_eta)
    structure(mu,Jacobian=Jacobian)
}

#' @export
predict_response.mblogit <- function(obj,data,...){
    mu <- predict(obj,newdata=data,type="response")
    X <- model_matrix(obj,data=data)
    # na.act <- obj$na.action
    # if(length(na.act))
    #     X <- X[-na.act,,drop=FALSE]
    coef <- coef(obj)
    nc <- names(coef)
    nc <- strsplit(nc,"~")
    ync <- sapply(nc,"[",1)
    xnc <- sapply(nc,"[",2)

    n.eqs <- ncol(mu)
    Jacobian <- list()
    for(k in 1:n.eqs){
        w <- -mu[,k]*mu
        if(k>1)
            w[,k]<-w[,k]+mu[,k]
        w <- w[,-1,drop=FALSE]
        h <- match(ync,unique(ync))
        Jacobian[[k]] <- w[,h]*X[,xnc]
    }
    structure(mu,Jacobian=Jacobian)
}

#' @export
predict_response.mclogit <- function(obj,data){
    mu <- predict(obj,newdata=data,type="response")
    rhs <- obj$formula[-2]
    fo <- obj$formula
    lhs <- fo[[2]]
    if(deparse(lhs[[1]])=="cbind"){
        lhs <- lhs[[3]]
    }
    fo[[2]] <- lhs
    m <- model.frame(fo,data=data)
    set <- m[[1]]
    
    X <- model_matrix(obj,data=data)
    na.act <- obj$na.action
    # if(length(na.act))
    #     X <- X[-na.act,,drop=FALSE]
    cf <- coef(obj)
    ncf <- names(cf)
    X <- X[,ncf,drop=FALSE]
    
    set <- match(set,unique(set))

    Jacobian <- mu*(X - rowsum(mu*X,set)[set,,drop=FALSE])
    structure(mu,Jacobian=Jacobian)
}

