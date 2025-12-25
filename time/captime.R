rm(list=ls())
library(scales)
library(dplyr)
library(gt)

timeDF <- read.csv("C:/Users/Saketh/Documents/STAT482/time/railairdata.csv")
timeDF$HSR <- timeDF$Avg.rail.speed..km.h. > 200
group = as.factor(timeDF$HSR)

plot(x=timeDF$Rail..hrs.,y=timeDF$Flight..hrs.,col=alpha(c("red","blue")[group],alpha = 0.3),pch=19,main="Flight vs. Rail Duration", 
     ylab = "Flight Duration (hrs)", xlab = "Rail Duration (hrs)")
abline(a=0,b=1,col="green")
legend("topright",legend=c("High Speed Rail (>200 kmh)", "Conventional Rail (<200 kmh)"),col=c("blue","red"),pch=c(19,19))

plot(x=timeDF$Dist.km.,y=timeDF$Rail..hrs./timeDF$Flight..hrs.,col=alpha(c("red","blue")[group],alpha = 0.3),pch=19,
     main="Time Ratio vs. Distance", xlab="Distance (km)",ylab="Time Ratio (Rail/Flight) (hrs)")
abline(h=1,col="green")
legend("topleft",legend=c("High Speed Rail (>200 kmh)", "Conventional Rail (<200 kmh)"),col=c("blue","red"),pch=c(19,19))




hsridx <- timeDF$HSR
crt <- t.test(timeDF[!hsridx,]$Rail..hrs.,timeDF[!hsridx,]$Flight..hrs.,paired=TRUE)
hsrt <- t.test(timeDF[hsridx,]$Rail..hrs.,timeDF[hsridx,]$Flight..hrs.,paired=TRUE)
crt

timeDF$ratio <- timeDF$Rail..hrs./timeDF$Flight..hrs.

hsrlin <- lm(ratio ~ log(Dist.km.),data=timeDF[hsridx,])
#plot(hsrlin)
shapiro.test(resid(hsrlin))
summary(hsrlin)
crlin <- lm(ratio ~ log(Dist.km.),data=timeDF[!hsridx,])
#plot(crlin)
shapiro.test(resid(crlin))
summary(crlin)
   
exp((1+2.1934) / 0.511)
#timeDF$diff = timeDF$Rail..hrs. - timeDF$Flight..hrs.
#lm(timeDF$diff ~ timeDF$Dist.km.)
#plot(lm(timeDF$diff ~ timeDF$Dist.km.))
#shapiro.test(resid(lm(timeDF$diff ~ timeDF$Dist.km.)))
usdist <- read.csv("C:/Users/Saketh/Documents/STAT482/time/USairportdist.csv")
usdist$Dist.km. <- usdist$DISTANCE.IN.MILES * 1.60934
midrangeflights <- usdist[usdist$Dist.km. >= 200 & usdist$Dist.km. < 600,]
fsample <- midrangeflights[sample(1:length(midrangeflights$Dist.km.),size=1000),]
hsrpred <- predict(hsrlin,newdata=fsample)
crpred <- predict(crlin,newdata=fsample)
summary(hsrpred)
summary(crpred)
sum(hsrpred >= 1) / length(hsrpred)
sum(crpred >= 1) / length(crpred)
sd(hsrpred)
sd(crpred)


summary(timeDF$Rail..hrs.)
sd(timeDF$Rail..hrs.)
summary(timeDF$Flight..hrs.)
sd(timeDF$Flight..hrs.)
railSumm <- c(summary(timeDF$Rail..hrs.)[c(1,3,4,6)],sd(timeDF$Rail..hrs.))
flightSumm <- c(summary(timeDF$Flight..hrs.)[c(1,3,4,6)],sd(timeDF$Flight..hrs.))
diffSumm <- railSumm - flightSumm
ratioSumm <- c(summary(timeDF$ratio)[c(1,3,4,6)],sd(timeDF$ratio))
summdf <- as.data.frame(rbind(railSumm,flightSumm,diffSumm,ratioSumm))
summdf$Row <- c("Rail", "Flight", "Diff","Ratio")
colnames(summdf)[5] <- "SD"
summdf <- summdf[,c(6,1:5)]
gt(summdf)

extrapPred <- as.data.frame(cbind(c(397,543.1,117.3,300.5),c(1.167,1.5,0.8333,1.333)))
colnames(extrapPred) <- c("Dist.km.","Time Flight Hr")
extrapPred$crPred <- predict(crlin,newdata = extrapPred)
extrapPred$crPredInt <- predict(crlin,newdata = extrapPred,interval = "confidence",level=0.95)
extrapPred$hsrPred <- predict(hsrlin,newdata = extrapPred)
extrapPred$hsrPredInt <- predict(hsrlin,newdata = extrapPred,interval = "confidence",level=0.95)


