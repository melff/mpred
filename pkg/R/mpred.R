sumz <- function(p,alpha=.05){
    
    mean <- weighted.mean(p$fit,w=p$w,na.rm=TRUE)
    mean.var <- weighted.mean(p$w*p$se.fit^2,w=p$w,na.rm=TRUE)
    var.mean <- mean.var/sum(p$w[is.finite(p$se.fit)])
    var <- mean.var + var.mean
    
    se <- sqrt(var)
    
    upr <- mean + se*qnorm(1-alpha/2)
    lwr <- mean + se*qnorm(alpha/2)
    
    c(mean=mean,var=var,se=se,lwr=lwr,upr=upr)
}


mpredict_vector <- function(obj,settings,subset,parent,setup,...){

    data <- get_all_vars(obj$formula,obj$data)
    mf <- model.frame(obj)
    ii <- rownames(mf)
    data <- data[ii,]
    w <- weights(obj)

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
    
    if(!missing(subset)){
        subset <- eval(substitute(subset),subset,parent)
        if(!is.logical(subset)) stop("'subset' should be evaluate as logical")
    }
    else {
        subset <- rep(TRUE,nrow(data))
    }

    ns <- names(settings)
    ns1 <- intersect(names(data),ns)
    ns2 <- setdiff(ns,names(data))
    
    n <- nrow(settings)

    pr <- list()
    for(i in 1:n){
        data.i <- data
        data.i[subset,ns1] <- settings[i,ns1,drop=FALSE]
        if(length(setup)){
            ee <- settings[i,ns2]
            if(length(ee)){
                ee <- as.environment(ee)
                parent.env(ee) <- parent
            }
            else
                ee <- parent
            e <- evalq(environment(), data.i, ee)
            eval(setup,e)
            l <- as.data.frame(as.list(e))
            names.l <- names(l)
            data.i[subset,names.l] <- l[subset,,drop=FALSE]
        }
        data.i[ns2] <- NULL
        pr.i <- as.data.frame(
            predict(object=obj,newdata=data.i,
                    type="response",se.fit=TRUE))
        pr.i[["__id__"]] <-i
        pr.i$w <- w
        pr[[i]] <- pr.i
    }
    
    prdf <- do.call(rbind,pr)
    
    list(
        prdf     = prdf,
        id       = 1:n,
        settings = settings
    )
    
}

#' Generic function to preduce predictive margins
#' 
#' @export
mpredict <- function(obj,...) UseMethod("mpredict")

#' Default method to preduce predictive margins
#' 
#' @export
mpredict.default <- function(obj,
                             ...,
                             settings,
                             subset,
                             setup=NULL){
    if(missing(setup))
        setup <- NULL
    else
        setup <- substitute(setup)
    mpv <- mpredict_vector(obj=obj,
                           settings=settings,
                           subset=subset,
                           parent=parent.frame(),
                           setup=setup,
                           ...)
    prdf <-     mpv$prdf
    settings <- mpv$settings
    id <-       prdf[["__id__"]]

    prdf <- split(prdf,id)
    summaries <- sapply(prdf,sumz)
    res <- cbind(settings,t(summaries))
    res[["__id__"]]<- NULL
    res
}


#' Method for 'mclogit' objects to preduce predictive margins
#' 
#' @export
mpredict.mclogit <- function(obj,
                             settings,
                             categories,
                             subset,
                             setup,
                             ...){

    categs <- eval(substitute(categories),obj$data,parent.frame())
    categories <- deparse(substitute(categories))
    if(missing(setup))
        setup <- NULL
    else
        setup <- substitute(setup)
    mpv <- mpredict_vector(obj=obj,
                           settings=settings,
                           subset=subset,
                           parent=parent.frame(),
                           setup=setup,
                           ...)
    
    prdf <-     mpv$prdf
    settings <- mpv$settings
    id <-       prdf[["__id__"]]

    categs <- rep(categs,length=nrow(prdf))
    ucategs <- sort(unique(categs))
    
    id.categs <- interaction(categs,id)
    
    prdf <- split(prdf,id.categs)
    summaries <- sapply(prdf,sumz)

    s.id <- 1:nrow(settings)

    s.id <- rep(s.id,each=length(ucategs))
    settings <- settings[s.id,,drop=FALSE]
    categs <- rep(categs,length=length(s.id))
    categs <- data.frame(categs)
    names(categs) <- categories
    
    cbind(settings,categs,
          t(summaries))
}

#' Method for 'mblogit' objects to preduce predictive margins
#' 
#' @export
mpredict.mblogit <- function(obj,
                             ...,
                             settings,
                             subset,
                             setup=NULL){
    if(missing(setup))
        setup <- NULL
    else
        setup <- substitute(setup)
    mpv <- mpredict_vector(obj=obj,
                           settings=settings,
                           subset=subset,
                           parent=parent.frame(),
                           setup=setup,
                           ...)
    prdf <-     mpv$prdf
    settings <- mpv$settings
    id <-       prdf[["__id__"]]

    fitnames <- grep("^fit.",names(prdf),value=TRUE)
    sefitnames <- grep("^se.fit.",names(prdf),value=TRUE)
    resp.categs <- levels(model.response(obj$model))
    resp.name <- names(obj$model)[1]
    
    res <- list()
    for(i in seq_along(fitnames)){
        f <- fitnames[i]
        s <- sefitnames[i]
        prdf.i <- data.frame(fit=prdf[[f]],
                             se.fit=prdf[[s]],
                             w=prdf$w)
        prdf.i <- split(prdf.i,id)
        summaries.i <- sapply(prdf.i,sumz)
        res.i <- cbind(settings,t(summaries.i))
        res.i[[resp.name]] <- rep(resp.categs[i],nrow(res.i))
        res.i[["__id__"]]<- NULL
        res[[i]] <- res.i
    }
    res <- do.call(rbind,res)
    res[[resp.name]] <- factor(res[[resp.name]],
                               levels=resp.categs)
    res
}
