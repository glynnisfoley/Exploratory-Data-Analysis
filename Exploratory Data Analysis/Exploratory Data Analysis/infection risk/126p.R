library(leaps)
library(MASS)
data('infectionrisk.txt')
#dat=read.table('infectionrisk.txt', header=T)

ID=infectionrisk$ID
Stay=infectionrisk$Stay
Age=infectionrisk$Age
InfctRsk=infectionrisk$InfctRsk
Culture=infectionrisk$Culture
Xray=infectionrisk$Xray
Beds=infectionrisk$Beds
MedSchool=infectionrisk$MedSchool
Region=infectionrisk$Region
Region=factor(Region)
Census=infectionrisk$Census
Nurses=infectionrisk$Nurses
Facilities=infectionrisk$Facilities
mod0=lm(InfctRsk~1)
mod.all=lm(InfctRsk~.,data=infectionrisk)
step(mod0, scope = list(lower = mod0, upper = mod.all), k = log(n))
mod1=lm(InfctRsk~Culture + Stay + Facilities +Region+Xray)
bs=coef(mod1)
summary(mod1)
boxcox(mod1)
#boxcox output shows a center around 1 ==> no transformation should be used
n=dim(infectionrisk)[1]
p=12
hv=hatvalues(mod1)
hv.max=which(hv==max(hv))
plot(hv, ylab = 'Hat Values', main = 'Hat Values')
text(1:n, hv, cex= .7, pos=4)
avg.hat = p/n
#cutoff: 2p/n
abline(h=2*avg.hat, col=4)
#cutoff: 3p/n
abline(h=3*avg.hat, col=2)
text(2, y=2*avg.hat, expression(2 %*% p/n), pos=3)
text(2, y=3*avg.hat, expression(3 %*% p/n), pos=3)

