% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mpred.R
\name{predmarg}
\alias{predmarg}
\title{Generic function to preduce predictive margins}
\usage{
predmarg(
  obj,
  settings,
  data,
  subset,
  type = NULL,
  groups = NULL,
  setup = NULL,
  cifunc = get_cifunc(obj),
  level = 0.95,
  parallel = FALSE,
  n = 21,
  trim = 0,
  mc.cores = if (.Platform$OS.type == "windows") 1L else max.cores,
  ...
)
}
\arguments{
\item{obj}{a model object, e.g. returned by \code{lm}, \code{glm}, etc.}

\item{settings}{an optional data frame of settings for independent variables
or a formula with variables for which settings are to be generated.}

\item{data}{an optional data frame for which the predictive margins are
computed. If ommited, an attempt is made to obtain the data from the
model object.}

\item{subset}{an optional logical vector that defines a subset for which a
predictive margin is computed}

\item{type}{an optional character string that specifies the type of
predictions, e.g. probabilities or cumulative probabilites. For future
versions only.}

\item{groups}{a variable that defines groups for which predictive margines
are computed. This variable has to have the same number of observations
as the data to which the model was fitted.}

\item{setup}{an optional expression that is evaluated for each setting,
i.e. individually for each row of the settings data frame. Can be used to
modify independent variables.}

\item{cifunc}{a function to compute prediction intervals. By default it is
the chosen by the function of the same name.}

\item{level}{level of confidence intervals of predictions.}

\item{parallel}{logical value that determines whether predictions for
individual settings are computed in parallel. (Does not yet work on
windows.)}

\item{n}{an integer number, the number of values generated for 
an independent variable if \code{settings} is a formula.
The values form an equally spaced grid that cover the range from
the variable in \code{data}.}

\item{trim}{a number between 0 and 1. The proportion of 
extreme values of variable in \code{data} that should be
trimed from the range of a generated set of values of 
an independent variable.}

\item{mc.cores}{number of CPU cores used for parallel processing.}

\item{\dots}{optional vectors of values of independent variabls. These
further arguments, if present, are used to create a data frame of
settings, using \code{\link{expand.grid}}.}
}
\value{
a data frame with the following variables:
\item{pred}{the mean prediction for the setting of the independent
    variables}
\item{var.pred}{the (estimated) variance of the mean prediction}
\item{se.pred}{the standard error of prediction, i.e. the square root of
    the variance of the mean prediction}
\item{lower}{lower prediction interval computed with \code{qfunc}}
\item{upper}{upper prediction interval computed with \code{qfunc}}
\item{\dots}{the independent variables for which values are set to create
    the predictions are also included in the resulting data frame.}
}
\description{
Generic function to preduce predictive margins
}
\details{
The generic function \code{predmarg} computes predictive
    margins for various settings of the independent variables. It is also
    possible to provide settings for independent variables that are
    included in the model, but that are used in the \code{setup}
    expression to transform independent variables. See the examples below.
}
\examples{
library(magrittr)

# Simple linear regression

fm <- lm(weight ~ poly(height, 2), data = women)
pm <-predmarg(fm,
              height=seq(from=58,to=72,
                         length=10))

str(pm)

plot(pred~height,data=pm,
     type="l")

with(women, points(height,weight))
with(pm, lines(height,lower,lty=2))
with(pm, lines(height,upper,lty=2))

# Logistic regression

library(carData)
Chile \%<>\% within({
    vote2 <- factor(vote,levels=c("N","Y"))
    vote2 <- as.integer(vote2=="Y")
})

glm.Chile.1 <- glm(vote2~sex+age+income+education,
                   data=Chile,
                   family=binomial)
summary(glm.Chile.1)

pm.Chile.1.income <- predmarg(glm.Chile.1,
                              income=seq(from=2500,to=200000,length=20))

plot(pred~income,data=pm.Chile.1.income,
     type="l")

# Baseline category logit

library(mclogit)
library(MASS)

mb.Chile <- mblogit(vote~statusquo,
                    data=Chile)
pm.mb.Chile <- predmarg(mb.Chile,
                        statusquo=seq(from=-2,to=2,length=20))
str(pm.mb.Chile)

library(ggplot2)
(ggplot(pm.mb.Chile,
       aes(x=statusquo,
           y=pred,
           fill=response
           )
       ) + geom_area())

(ggplot(pm.mb.Chile,
       aes(x=statusquo,
           y=pred,
           ymin=lower,
           ymax=upper
           )
       ) + geom_line() +geom_ribbon(alpha=.25) + facet_grid(~response))


mb.hs <- mblogit(Sat~Infl+Type+Cont,weights=Freq,
                 data=housing)

pm.mb.hs <- predmarg(mb.hs,
                     Infl=levels(Infl),
                     Type=levels(Type))

dodge <- position_dodge(width=.8)
(ggplot(pm.mb.hs)
    +facet_wrap(~Type)
    +geom_bar(
         aes(fill=response,
             x=Infl,
             y=pred),
         stat='identity',position=dodge,width=.8)
    +geom_errorbar(
         aes(x=Infl,
             ymin=lower,
             ymax=upper,group=response),
         position=dodge,width=.4)) 

# The following requires the most current 'mclogit' version on GitHub
# and fails with the CRAN version
# # Baseline category logit with random effects
# 
# # Some artificial data
# exadata <- local({
#     B <- cbind(c(-.5,.3),
#                c(.5,-.5))
#     set.seed(42)
#     x <- rnorm(n=60)
#     X <- cbind(1,x)
#     Eta <- X \%*\% B
#     j <- rep(1:10,6)
#     jf <- as.factor(j)
#     u1 <- rnorm(n=10,sd=.8)
#     u2 <- rnorm(n=10,sd=.8)
#     Eta <- Eta + cbind(u1[j],x*u2[j])
#     expEta <- cbind(1,exp(Eta))
#     sum.expEta <- rowSums(expEta)
#     pi <- expEta/sum.expEta
#     Y <- t(apply(pi,1,rmultinom,n=1,size=300))
#     res <-data.frame(Y,x,j,jf)
#     names(res)[1:3] <- paste0("y",1:3)
#     res
# })
# 
# # Baseline logit model with random intercepts and random slopes
# mbrsl <- mblogit(cbind(y1,y2,y3)~x,data=exadata,
#                  random = ~1+x|j)
# summary(mbrsl)
# 
# # Predictive margins for values of x
# pm.mbrsl <- predmarg(mbrsl,x=seq(from=min(x),to=max(x),length=24))
# (ggplot(pm.mbrsl,
#        aes(x=x,
#            y=pred,
#            fill=response
#            )
#        ) + geom_area())
# 
# # Predictive margins for the random effects
# pm.mbrsl.j <- predmarg(mbrsl,j=1:10)
# (ggplot(pm.mbrsl.j,
#        aes(x=j,
#            y=pred,
#            fill=response
#            )
#        ) + geom_bar(position="fill",stat="identity"))
# 
# pm.mbrsl.jx <- predmarg(mbrsl,
#                     j=1:10,
#                     x=seq(from=min(x),to=max(x),
#                           length=24))
# 
# (ggplot(pm.mbrsl.jx,
#        aes(x=x,
#            y=pred,
#            fill=response
#            )
#        ) + geom_area()
#     + facet_wrap(~j))
}
