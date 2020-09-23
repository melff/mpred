#' Confidence/prediction interval based on a normal distribution 
#'
#' Creates prediction intervals from a normal distribution with given mean and
#' standard deviation.
#' 
#'
#' @param mean a scalar; the mean parameter of the normal distribution
#' @param sd a scalar; the standard deviation parameter of the normal distribution
#' @param level a scalar; the confidence level
#' @export
cinorm <- function(mean,sd,level){
    alpha <- (1-level)/2
    p.lower <- alpha
    p.upper <- 1-alpha

    list(lower=qnorm(p=p.lower,mean=mean,sd=sd),
         upper=qnorm(p=p.upper,mean=mean,sd=sd))
}

#' Confidence/prediction interval based on a beta distribution 
#' 
#' Creates prediction intervals from a beta distribution with given mean and
#' standard deviation. Prediction intervals stay between 0 and 1.
#'
#' @param mean a scalar; the mean of the beta distribution
#' @param sd a scalar; the standard deviation of the beta distribution
#' @param level a scalar; the confidence level
#' @export
cibeta <- function(mean,sd,level){
    alpha <- (1-level)/2
    p.lower <- alpha
    p.upper <- 1-alpha

    nu <- mean*(1-mean)/sd^2-1
    shape1 <- mean*nu
    shape2 <- (1-mean)*nu
    
    list(lower=qbeta(p=p.lower, shape1=shape1, shape2=shape2),
         upper=qbeta(p=p.upper, shape1=shape1, shape2=shape2))
}

#' A generic function that returns functions for confidence/predicion intervals
#'
#' Returns a function for confidence or prediction intervals appropriate for an
#' fitted model object. For linear regression this will be \code{cinorm()}, for
#' logistic regression (done by \code{glm}) this will be \code{cibeta}, etc.
#'
#' @param obj an fitted model object
#' @export
get_cifunc <- function(obj) UseMethod("get_cifunc")

#' @export
#' @rdname get_cifunc
get_cifunc.lm <- function(obj) cinorm
#' @export
#' @rdname get_cifunc
get_cifunc.glm <- function(obj) {
    family <- family(obj)$family
    if(family %in% c("binomial","quasibinomial")) cibeta
    else cinorm
}
#' @export
#' @rdname get_cifunc
get_cifunc.mblogit <- function(obj) cibeta
#' @export
#' @rdname get_cifunc
get_cifunc.mclogit <- function(obj) cibeta
#' @export
#' @rdname get_cifunc
get_cifunc.clm <- function(obj) cibeta
#' @export
#' @rdname get_cifunc
get_cifunc.clmm <- function(obj) cibeta
