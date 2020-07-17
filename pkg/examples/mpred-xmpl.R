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
Chile %<>% within({
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

# Baseline category logit with random effects

# Some artificial data
exadata <- local({
    B <- cbind(c(-.5,.3),
               c(.5,-.5))
    set.seed(42)
    x <- rnorm(n=60)
    X <- cbind(1,x)
    Eta <- X %*% B
    j <- rep(1:10,6)
    jf <- as.factor(j)
    u1 <- rnorm(n=10,sd=.8)
    u2 <- rnorm(n=10,sd=.8)
    Eta <- Eta + cbind(u1[j],x*u2[j])
    expEta <- cbind(1,exp(Eta))
    sum.expEta <- rowSums(expEta)
    pi <- expEta/sum.expEta
    Y <- t(apply(pi,1,rmultinom,n=1,size=300))
    res <-data.frame(Y,x,j,jf)
    names(res)[1:3] <- paste0("y",1:3)
    res
})

# Baseline logit model with random intercepts and random slopes
mbrsl <- mblogit(cbind(y1,y2,y3)~x,data=exadata,
                 random = ~1+x|j)
summary(mbrsl)

# Predictive margins for values of x
pm.mbrsl <- predmarg(mbrsl,x=seq(from=min(x),to=max(x),length=24))
(ggplot(pm.mbrsl,
       aes(x=x,
           y=pred,
           fill=response
           )
       ) + geom_area())

# Predictive margins for the random effects
pm.mbrsl.j <- predmarg(mbrsl,j=1:10)
(ggplot(pm.mbrsl.j,
       aes(x=j,
           y=pred,
           fill=response
           )
       ) + geom_bar(position="fill",stat="identity"))

pm.mbrsl.jx <- predmarg(mbrsl,
                    j=1:10,
                    x=seq(from=min(x),to=max(x),
                          length=24))

(ggplot(pm.mbrsl.jx,
       aes(x=x,
           y=pred,
           fill=response
           )
       ) + geom_area()
    + facet_wrap(~j))
