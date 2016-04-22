####HW 5####
rm(list=ls())
library(faraway)
library(s20x)
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
a0<-exp(lmod2$coefficients[1])
a1<-lmod2$coefficients[2]
w<-a0*(Field^a1)
length(w)
lmod3<-lm(Lab~Field, weights=(1/w))
plot(lmod3$fitted,lmod3$residuals)
abline(0,0)
par(mfrow=c(1,2))
plot(lmod3$fitted,lmod3$residuals)
plot(lmod1$fitted,lmod1$residuals)

#Part c
par(mfrow=c(1,2))
FieldI<-Field^(-1)
LabI<-Lab^(-1)
Lab2<-Lab^.5
Field2<-Field^.5
#lmodt2<-lm((log(Lab))~log(Field))
#lmodt2<-lm(((Lab2))~(Field2))
lmodt1<-lm((log(Lab))~log(Field))
plot(lmodt1$fitted,lmodt1$residuals)
abline(0,0)
hist(lmodt1$residuals,prob=T)
detach(pipeline)



#Q2
data(denim)
attach(denim)
#part a
par(mfrow=c(1,1))
plot(supplier,waste, xlab='Supplier', ylab='Waste', main='Weekly Supplier Waste')

#partb
lmod4<-lm(waste~supplier)
summary(lmod4)
anova(lmod4)

#partc
par(mfrow=c(1,2))
plot(jitter(lmod4$fitted.values),lmod4$residuals)
abline(0,0)
qqnorm(residuals(lmod4))
qqline(residuals(lmod4))
par(mfrow=c(1,1))
hist(residuals(lmod4),prob=T)

levene.test(waste~supplier)
unique(supplier)
#part d
which(denim==max(denim[which(supplier==1),1])) #82
which(denim==max(denim[which(supplier==2),1])) #87
denim.red<-denim[-c(82,87),]
lmod5<-lm(waste~supplier, data=denim.red)
summary(lmod5)
anova(lmod5)
plot(denim.red[,2], denim.red[,1], main='Outliers Removed', xlab='Suppliers', ylab='Waste')
detach(denim)

#part e 
(tci <-TukeyHSD(aov(waste~supplier, denim.red)))


#Q4
# r=6
# n=10
n=4
r=3
nt=r*n
k=r*(r-1)/2
zz<-seq(from=0, to=1, by=0.001)
length(zz)
alpha=0.9
(bf=qt(alpha/k, nt-r))
(tuk=(1/sqrt(2))*qtukey(alpha,n,(nt-n)))
i=0
while(bf<tuk){
  i=i+1
  alpha=zz[i]
  tuk=1/sqrt(2)*qtukey(alpha, n, (nt-n))
  bf=qt(alpha/k, nt-r)
  if(i==1001){break}
}
print(zz[i])


tukarray<-qtukey(zz,n,r)
