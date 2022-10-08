install.packages("car")
library("car")


#Data_Describtion
data=read.csv(file.choose(),header=TRUE)
time=data[,1]-1
c=15.2/70
len=c*data[,2]
len=len-len[1]
plot(len~time)

#Model_Selection
fitg=lm(len~I(time^2)+I(time)-1)
fitr=lm(len~I(time^2)-1)
anova(fitr,fitg)

#Fitting
fit=fitg
f<-Vectorize(function(x){sum(fit$coef*c(x*x,x))})
plot(len~time)
curve(f,add=T)
summary(fit)

#Error_Analysis
plot(fit$res)
abline(h=0)
fit1=lm(len~I(time^2)+I(time^3)+I(time)-1)
plot(fit1$res)
abline(h=0)
fit2=lm(len~I(time^2)+I(time)+I(sin(time))-1)
plot(fit2$res)
abline(h=0)
qqPlot(fit$resid)
qqPlot(fit2$resid)
influence.measures(fit)

#Calculating_FPS
g=980.665
fps=(g/(2*fit$coef[1]))^0.5

#95%_C.I(Bootstrap)
set=c()
for(i in 1:1000){
	s=fit$resid
    s=sample(fit$resid,,replace=TRUE)
    s=s+fit$fit
    a=lm(s~I(time^2)+I(time)-1)
    fps=(g/(2*a$coef[1]))^0.5
    set=c(set,fps)}
sd(set)


