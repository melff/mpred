#' @importFrom ordinal ranef condVar

# From ordinal package:
findbars <- function (term) 
{
    if (is.name(term) || !is.language(term)) 
        return(NULL)
    if (term[[1]] == as.name("(")) 
        return(findbars(term[[2]]))
    if (!is.call(term)) 
        stop("term must be of class call")
    if (term[[1]] == as.name("|")) 
        return(term)
    if (length(term) == 2) 
        return(findbars(term[[2]]))
    c(findbars(term[[2]]), findbars(term[[3]]))
}

add.terms <- function(x,y) as.call(list(as.name("+"),x,y))
combinebars <- function(terms){
    for(i in seq_along(terms))
        terms[[i]] <- as.call(list(as.name("("),terms[[i]]))
    terms <- Reduce(add.terms,terms)
    as.call(list(as.name("~"),terms))
}

dropbars <- function(fo){
    if(is.name(fo) || !is.language(fo)) return(fo)
    if(fo[[1]] == as.name("|"))
        return(NULL)
    if(length(fo) == 2){
        res <- dropbars(fo[[2]])
        if(length(res))
            return(as.call(list(fo[[1]],res)))
        else return(NULL)
    }
    else if(length(fo) == 3) {
        res1 <- dropbars(fo[[2]])
        res2 <- dropbars(fo[[3]])
        if(length(res1) && length(res2))
            return(as.call(list(fo[[1]],res1,res2)))
        else if(length(res1)) return(res1)
        else if(length(res2)) return(res2)
        else return(NULL)
    }
    return(fo)
}

model_matrix_clmm <- function(obj, data){
    rhs <- obj$formula[-2]
    random <- findbars(rhs)
    all_random <- combinebars(random)
    rhs <- as.formula(dropbars(rhs))
    vars <- unique(c(all.vars(rhs),all.vars(all_random),all.vars(obj$call$weights)))
    fo <- paste("~",paste(vars,collapse=" + "))
    fo <- as.formula(fo,env=parent.frame())
    mf <- model.frame(fo,data=data,na.action=na.exclude)
    na.act <- attr(mf,"na.action")
    X <- model.matrix(rhs,mf,
                      contrasts.arg=obj$contrasts,
                      xlev=obj$xlevels)
    if(length(na.act))
        X <- X[-na.act,,drop=FALSE]
    Z <- list()
    Znames <- character()
    for(k in seq_along(random)){
        rnd <- random[[k]]
        z <- rnd[[2]]
        groups <- rnd[[3]]
        Z.k <- model.matrix(asOneSidedFormula(z),mf,
                            contrasts.arg=obj$contrasts,
                            xlev=obj$xlevels)
        Znames[k] <- deparse(groups)
        groups <- eval(groups,envir=data)
        if(length(na.act)){
            Z.k[-na.act,,drop=FALSE]
            groups <- groups[-na.act]
        }
        Z[[k]] <- structure(Z.k,
                            groups=groups)
    }
    names(Z) <- Znames
    list(X=X, Z=Z)
}

predict_response.clmm <- function(obj, data, ...){

    XZ <- model_matrix_clmm(obj, data)
    X <- XZ$X
    intcp <- which(colnames(X)=="(Intercept)")
    X <- X[,-intcp,drop=FALSE]
    eta <- c(X  %*% obj$beta)
    Z <- XZ$Z
    re <- ranef(obj)
    re_names <- names(re)
    for(re_n in re_names){
        Z.k <- Z[[re_n]]
        re.k <- re[[re_n]]
        re.k <- as.matrix(re.k)
        Znames.k <- colnames(Z.k)
        grp <- as.character(attr(Z.k,"groups"))
        Zb.k <- rowSums(Z.k*re.k[grp,Znames.k])
        eta <- eta + Zb.k
    }
    theta <- c(obj$Theta)
    eta <- outer(eta,theta,"-")
    if(obj$info$link == "logit")
        cumul.prob <- 1/(1+exp(-eta))
    else stop("unsupported link function")
    gamma <- cbind(1,cumul.prob)
    m1 <- ncol(gamma)
    D <- diff(diag(m1+1))[,-1]
    mu <- gamma %*% D
    ggamma <- gamma*(1-gamma)
    ggamma <- ggamma[,-1,drop=FALSE]
    n.eqs <- ncol(mu)
    d.mu.d.gamma <- D[-1,,drop=FALSE]
    d.kappa.d.alpha <- obj$tJac
    Jacobian <- list()
    ranefJacobian <- list()
    q <- ncol(vcov(obj))
    filler <- matrix(0,nrow=nrow(X),
                     ncol=q-ncol(X)-ncol(d.kappa.d.alpha))
    for(k in 1:n.eqs){
        d.mu_k.d.gamma <- d.mu.d.gamma[,k]
        d.mu_k.d.eta <- c(ggamma %*% d.mu_k.d.gamma)
        d.mu_k.d.beta <- d.mu_k.d.eta*X
        d.mu_k.d.alpha <- ggamma %*% diag(x=d.mu_k.d.gamma) %*% d.kappa.d.alpha
        Jacobian[[k]] <- cbind(d.mu_k.d.alpha,d.mu_k.d.beta,filler)
        ranefJacobian.k <- list()
        for(re_n in re_names){
            Z.kl <- Z[[re_n]]
            ranefJacobian.k[[re_n]] <- d.mu_k.d.eta*Z.kl
        }
        ranefJacobian[[k]] <- ranefJacobian.k
    }
    structure(mu,Jacobian=Jacobian,ranefJacobian=ranefJacobian)
}

#' @export
predmarg1.clmm <- function(obj,
                           j,
                           data,
                           settings,
                           groups,
                           type,
                           setup,
                           cifunc,
                           level,
                           parent,
                           ...){
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
    mu.ranef <- attr(mu,"ranefJacobian")
    num.eqs <- ncol(mu)
    cov.theta <- vcov(obj)
    var.ranef <- condVar(obj)

    res <- list()
    
    if(!length(groups)){

        sum.w <- sum(w)
        mu.bar <- colSums(w*mu)/sum.w
        
        for(h in 1:num.eqs){

            mu.bar.h <- mu.bar[h]
            mu.ranef.h <- mu.ranef[[h]]
            if(length(mu.theta)){
                mu.theta.h <- mu.theta[[h]]
                mu.bar.theta.h <- colSums(w*mu.theta.h)/sum.w
                var.mu.bar.h <- mu.bar.theta.h %*% cov.theta %*% mu.bar.theta.h
                for(n in names(mu.ranef.h)){
                    mu.ranef.hn <- mu.ranef.h[[n]]
                    var.ranef.n <- var.ranef[[n]]
                    j <- attr(mu.ranef.hn,"groups")
                    var.mu.bar.ranef.n <- sum(w*mu.ranef.hn*(mu.ranef.hn*var.ranef.n[j,]))/sum.w
                    var.mu.bar.h <- var.mu.bar.h + var.mu.bar.ranef.n
                }
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
            mu.ranef.h <- mu.ranef[[h]]
            if(length(mu.theta)){
                mu.theta.h <- mu.theta[[h]]
                mu.theta.bar.hk <- rowsum(w*mu.theta.h,k)/sum.w.k
                var.mu.bar.hk <- rowSums(mu.theta.bar.hk*(mu.theta.bar.hk%*%cov.theta))
                for(n in names(mu.ranef.h)){
                    mu.ranef.hn <- mu.ranef.h[[n]]
                    var.ranef.n <- var.ranef[[n]]
                    j <- attr(mu.ranef.hn,"groups")
                    var.mu.bar.ranef.hkn <- rowsum(w*mu.ranef.hn*(mu.ranef.hn*var.ranef.n[j,]),k)/sum.w.k
                    var.mu.bar.ranef.hkn <- rowSums(var.mu.bar.ranef.hkn)
                    var.mu.bar.hk <- var.mu.bar.hk + var.mu.bar.ranef.hkn
                }
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
