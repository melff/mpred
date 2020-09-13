#' @export
predict_response.clm <- function(obj, data, ...){

    X <- model_matrix(obj,data=data)
    intcp <- which(colnames(X)=="(Intercept)")
    X <- X[,-intcp,drop=FALSE]
    eta <- c(X  %*% obj$beta)
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
    for(k in 1:n.eqs){
        d.mu_k.d.gamma <- d.mu.d.gamma[,k]
        d.mu_k.d.eta <- c(ggamma %*% d.mu_k.d.gamma)
        d.mu_k.d.beta <- d.mu_k.d.eta*X
        d.mu_k.d.alpha <- ggamma %*% diag(x=d.mu_k.d.gamma) %*% d.kappa.d.alpha
        Jacobian[[k]] <- cbind(d.mu_k.d.alpha,d.mu_k.d.beta)
    }
    structure(mu,Jacobian=Jacobian)
}


#' @export
predmarg1.clm <- function(obj,...) predmarg1.default_multieq(obj,...)
