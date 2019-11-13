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
(ggplot(pm.mb.hs,
        aes(x=Infl,
            y=pred,
            ymin=lower,
            ymax=upper,
            fill=response,
            group=response
            )
        )
    +facet_wrap(~Type)
    +geom_bar(stat='identity', position=dodge,width=.7)
    +geom_errorbar(position=dodge,width=.4)
) 
