####HW 5####

library(faraway)
data(pipeline)
head(pipeline)
attach(pipeline)
lmod1<-lm(Lab~Field)
plot(lmod1$fitted, lmod1$residuals)
abline(0,0)







