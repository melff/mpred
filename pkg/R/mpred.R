#' Generic function to preduce predictive margins
#' 
#' @export
#'
#' @param obj a model object, e.g. returned by \code{lm}, \code{glm}, etc.
#' @param settings an optional data frame of settings for independent
#'     variables. 
#' @param restrict an optional logical vector that restricts the application of the
#'     settings for independent variables to a subset of observations
#' @param subset an optional logical vector that defines a subset for which a
#'     predictive margin is computed
#' @param setup an optional expression that is evaluated for each setting,
#'     i.e. individually for each row of the settings data frame. Can be
#'     used to modify independent variables.
#' @param quick.setup an optional expression that is evaluated in the
#'     expanded data set that contains the original model data and
#'     \emph{all} settings of the designated independent variables for which
#'     predictive margins computed. Using this argument is quicker than the
#'     'setup' argument, but also more risky - the length of the expanded
#'     data might not exactly be as expected.
#' @param qfunc a function to compute prediction intervals.
#' @param alpha the cut-off probability of the prediction intervals
#'     (i.e. such that the confidence level is 1-alpha/2)
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
                     restrict,
                     subset,
                     setup=NULL,
                     quick.setup=NULL,
                     qfunc=qnorm,
                     alpha=0.05,
                     ...) UseMethod("predmarg")

#' @export
predmarg.default <- function(obj,
                             settings,
                             restrict,
                             subset,
                             setup=NULL,
                             quick.setup=NULL,
                             qfunc=qnorm,
                             alpha=0.05,
                             ...) {

    data <- get_data(obj)

    nact <- na.action(obj)
    if(length(nact))
        data <- data[-nact,,drop=FALSE]
    w <- weights(obj)
    if(!length(w))
        w <- rep(1,nrow(data))

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

    i <- rep(i,m)
    j <- rep(j,each=n)

    nd <- names(data)
    ns <- names(settings)
    nd1 <- setdiff(nd,ns)
    ns2 <- setdiff(ns,nd)
    
    if(!missing(restrict)){
        restrict <- eval(substitute(restrict),data,parent.frame())
        if(!is.logical(restrict)) stop("'restrict' should be evaluate as logical")
        newdata <- cbind(data,settings[j,ns2,drop=FALSE])
        newdata[restrict,ns] <- settings[j[restrict],,drop=FALSE]
    }
    else 
        newdata <- cbind(data[i,nd1,drop=FALSE],settings[j,,drop=FALSE])

    if(!missing(setup)){
        for(jj in 1:m){
            e <- as.environment(settings[jj,ns2,drop=FALSE])
            parent.env(e) <- parent.frame()
            e <- evalq(environment(),newdata[j==jj,,drop=FALSE],e)
            eval(substitute(setup),e)
            l <- as.data.frame(as.list(e))
            names.l <- names(l)
            newdata[j==jj,names.l] <- l
        }
    }

    if(!missing(quick.setup)){
        e <- as.environment(newdata[ns2])
        parent.env(e) <- parent.frame()
        e <- evalq(environment(),newdata,e)
        eval(substitute(quick.setup),e)
        l <- as.data.frame(as.list(e))
        names.l <- names(l)
        newdata[names.l] <- l
    }

    w <- newdata$.w
    
    mu <- predict_response(obj,newdata)
    sum.w.j <- as.vector(rowsum(w,j))
    mu.bar.j <- rowsum(w*mu,j)/sum.w.j
    num.eqs <- ncol(mu.bar.j)

    cov.theta <- vcov(obj)
    if(num.eqs == 1){
        mu.theta <- mu_theta(obj,mu,newdata)
        mu.theta.bar.j <- rowsum(w*mu.theta,j)/sum.w.j
        var.mu.bar.j <- rowSums(mu.theta.bar.j*(mu.theta.bar.j%*%cov.theta))
        se.mu.bar.j <- sqrt(var.mu.bar.j)

        lower <- qfunc(p=alpha/2,mean=mu.bar.j,sd=se.mu.bar.j)
        upper <- qfunc(p=1-alpha/2,mean=mu.bar.j,sd=se.mu.bar.j)
        
        res <- data.frame(pred=mu.bar.j,
                          var.pred=var.mu.bar.j,
                          se.pred=se.mu.bar.j,
                          lower=lower,
                          upper=upper)
        res <- cbind(res,settings)
        res$.j <- NULL
        res
    }
    else {
        res <- list()
        for(k in 1:num.eqs){
            mu.theta <- mu_theta(obj,mu,newdata,k)
            mu.theta.bar.j <- rowsum(w*mu.theta,j)/sum.w.j
            var.mu.bar.j <- rowSums(mu.theta.bar.j*(mu.theta.bar.j%*%cov.theta))
            se.mu.bar.j <- sqrt(var.mu.bar.j)

            mu.bar.jk <- mu.bar.j[,k]
            
            lower <- qfunc(p=alpha/2,mean=mu.bar.jk,sd=se.mu.bar.j)
            upper <- qfunc(p=1-alpha/2,mean=mu.bar.jk,sd=se.mu.bar.j)
            
            res.k <- data.frame(pred=mu.bar.jk,
                                var.pred=var.mu.bar.j,
                                se.pred=se.mu.bar.j,
                                lower=lower,
                                upper=upper,
                                response=k)
            res.k <- cbind(res.k,settings)
            res.k$.j <- NULL
            res[[k]] <- res.k
        }
        res <- do.call(rbind,res)
        if(length(colnames(mu)))
            res$response <- factor(res$response,labels=colnames(mu))
        res
    }
    
}

#' @export
get_data <- function(obj) UseMethod("get_data")

#' @export
get_data.lm <- function(obj){

    terms <- obj$terms
    env <- environment(terms)

    call <- obj$call
    data <- eval(call$data,parent.frame())
    wc <- call$weights
    res <- get_all_vars(obj,data=data)
    if(length(wc)){
        wn <- all.vars(wc)
        wdf <- data.frame(w=eval(wc,data,
                                 parent.frame()))
        names(wdf) <- wn
        res <- cbind(res,wdf)
    }
    res
}

#' @export
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
predict_response.lm <- function(obj,data) predict(obj,newdata=data,type="response")
predict_response.glm <- function(obj,data) predict(obj,newdata=data,type="response")

predict_response.mblogit <- function(obj,data)
    predict(obj,newdata=data,type="response")


mu_theta <- function(obj,mu,data,k) UseMethod("mu_theta")

mu_theta.lm <- function(obj,mu,data,k){
    X <- model.matrix(obj,data=data)
    X
}

mu_theta.glm <- function(obj,mu,data,k){
    X <- model.matrix(obj,data=data)
    coef <- coef(obj)
    eta <- X%*%coef
    mu_eta <- obj$family$mu.eta(eta)
    X*as.vector(mu_eta)
}

mu_theta.mblogit <- function(obj,mu,data,k){
    X <- model.matrix(obj,data=data)
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
