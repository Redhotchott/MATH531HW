####HW 5####

library(faraway)
data(pipeline)
head(pipeline)
attach(pipeline)
lmod1<-lm(Lab~Field)
plot(lmod1) #gets you a plot of the residuals for question 1 and a qqplot of residuals

#PART B
i  <-order(pipeline$Field)
npipe<-pipeline[i,]
ff <- gl(12,9)[-108]
meanfield <- unlist(lapply(split(npipe$Field,ff),mean))
varlab <-unlist(lapply(split(npipe$Lab,ff),var))

lmod2<-lm(log(varlab)~log(meanfield))
summary(lmod2)

lmod3<-lm(Lab~Field, weights=1/(Field)^1.12)
plot(lmod3)

