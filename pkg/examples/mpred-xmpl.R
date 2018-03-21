library(magrittr)

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

library(car)
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

library(mclogit)
library(MASS)

mb.hs <- mblogit(Sat~Infl+Type+Cont,weights=Freq,
                 data=housing)

pm.mb.hs <- predmarg(mb.hs,Infl=levels(Infl))

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


women2 <- within(women,{
    censor <- ifelse(1:nrow(women)>7,1,0)
})

fm <- lm(weight ~ poly(height, 2), data = women2)

pm1 <-predmarg(fm,
               height=seq(from=58,to=72,
                         length=10),
               quick.setup={
                   # The data expanded by the settings is longer than
                   # the original data frame 'women2', i.e. ten times as long
                   # due to the lenght of the 'height' argument.
                   # We therefore need to adapt the length of the variable
                   # 'censor' if we want to use it to censor the variable
                   # 'heigth' in the settings.
                   censor <- rep(women2$censor,length=length(height))
                   height <- ifelse(censor>0,height,0)
               })

pm2 <-predmarg(fm,
               height=seq(from=58,to=72,
                         length=10),
               setup={
                   # The expression is evaluated for each individual 
                   # setting of 'height' and a copy of the data to which
                   # the model was fit.
                   height <- ifelse(women2$censor>0,height,0)
               })


pm1 - pm2

pm3 <-predmarg(fm,
               height=seq(from=58,to=72,
                          length=10),
               multi=c(1,2),
               setup={
                   height <- height*multi
               })

str(pm3)
