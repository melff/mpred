ranef <- function(object, ...) UseMethod("ranef")

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
    q <- ncol(vcov(obj))
    filler <- matrix(0,nrow=nrow(X),
                     ncol=q-ncol(X)-ncol(d.kappa.d.alpha))
    for(k in 1:n.eqs){
        d.mu_k.d.gamma <- d.mu.d.gamma[,k]
        d.mu_k.d.eta <- c(ggamma %*% d.mu_k.d.gamma)
        d.mu_k.d.beta <- d.mu_k.d.eta*X
        d.mu_k.d.alpha <- ggamma %*% diag(x=d.mu_k.d.gamma) %*% d.kappa.d.alpha
        Jacobian[[k]] <- cbind(d.mu_k.d.alpha,d.mu_k.d.beta,filler)
    }
    structure(mu,Jacobian=Jacobian)
}

mu_theta.clmm <- function(obj,mu,data,k){
    m <- ncol(mu)
    D <- diff(diag(m+1))[,-1]
    iD <- solve(D)

    gamma <- mu%*%iD[,-1]
    ggamma <- gamma*(1-gamma)

    d.mu.d.gamma <- t(diff(diag(m)))

    XZ <- model_matrix_clmm(obj, data)
    X <- XZ$X
    intcp <- which(colnames(X)=="(Intercept)")

    d.eta.d.beta <- X[,-intcp,drop=FALSE]
    d.mu_k.d.gamma <- d.mu.d.gamma[k,]
    d.mu_k.d.eta <- c(ggamma %*% d.mu_k.d.gamma)
    d.mu_k.d.beta <- d.mu_k.d.eta*d.eta.d.beta

    d.kappa.d.alpha <- obj$tJac
    d.mu_k.d.alpha <- ggamma %*% diag(x=d.mu_k.d.gamma) %*% d.kappa.d.alpha

    mu_theta <- cbind(d.mu_k.d.alpha,d.mu_k.d.beta)
    q <- ncol(vcov(obj))
    filler <- matrix(0,nrow=nrow(mu_theta),ncol=q-ncol(mu_theta))
    cbind(mu_theta,filler)
}


#' @export
predmarg1.clmm <- function(obj,...) predmarg1.default_multieq(obj,...)
