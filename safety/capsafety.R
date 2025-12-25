library(dplyr)
library(tidyr)
library(plotrix)
library(ggplot2)
library(viridis)
library(lme4)
library(lmerTest)
library(merTools)
library(modelsummary)

rm(list=ls())
par(mfrow=c(1,1))
railpa <- read.csv("C:/Users/Saketh/Documents/STAT482/safety/railpa.csv")
railpa <- railpa[,c(8,9,10,12)]
colnames(railpa)[4] <- "rail passengers (millions passenger-kilometers)"
colnames(railpa)[2] <- "country"

roadpa <- read.csv("C:/Users/Saketh/Documents/STAT482/safety/roadpa.csv")
roadpa <- roadpa[,c(10,11,12,14)]
colnames(roadpa)[4] <- "road passengers (millions passenger-kilometers)"
colnames(roadpa)[2] <- "country"

railacc <- read.csv("C:/Users/Saketh/Documents/STAT482/safety/railaccidents.csv")
railacc <- railacc[,c(14,15,16,18)]
colnames(railacc)[4] <- "rail accidents"
colnames(railacc)[2] <- "country"


roadacc <- read.csv("C:/Users/Saketh/Documents/STAT482/safety/roadaccidents.csv")
roadacc <- roadacc[,c(10,11,12,14)]
colnames(roadacc)[2] <- "country"
colnames(roadacc)[4] <- "road accidents"

CRI <- read.csv("C:/Users/Saketh/Documents/STAT482/safety/CRI.csv")
CRI <- CRI[,c(2,3)]
colnames(CRI)[1] <- "country"

GDP <- read.csv("C:/Users/Saketh/Documents/STAT482/safety/gdpdata.csv")
GDPLong <- GDP %>%
  pivot_longer(
    cols = starts_with("X20"),
    names_to = "TIME_PERIOD",
    values_to = "gdp"
  ) %>%
  mutate(TIME_PERIOD = as.integer(sub("X","",TIME_PERIOD)))
colnames(GDPLong)[1] <- "country"

area <- read.csv("C:/Users/Saketh/Documents/STAT482/safety/area.csv")
area <- area[,c(6,8)]
colnames(area) <- c("geo","area")
substr(area$geo[1],1,2)
for(i in 1:length(area$geo)){
  area$geo[i] <- substr(area$geo[i],1,2)
}

allpa <- merge(railpa,roadpa,by=c("geo","country","TIME_PERIOD"),all = TRUE)
railaccallpa <- merge(railacc,allpa,by=c("geo","country","TIME_PERIOD"),all=TRUE)
safetyDFALL <- merge(roadacc,railaccallpa,by=c("geo","country","TIME_PERIOD"),all=TRUE)
CRIMerge <- merge(safetyDFALL,CRI,by="country")
GDPMerge <- merge(CRIMerge,GDPLong,by=c("country","TIME_PERIOD"),all=TRUE)
areaMerge <- merge(GDPMerge,area,by="geo",all=TRUE)

safetyDF <- areaMerge[complete.cases(areaMerge),]
safetyDF$roadAccByVol <- safetyDF$`road accidents` / safetyDF$`road passengers (millions passenger-kilometers)`
safetyDF$railAccByVol <- safetyDF$`rail accidents` / safetyDF$`rail passengers (millions passenger-kilometers)`
safetyDF$ratio <- safetyDF$roadAccByVol / safetyDF$railAccByVol

safetyByYear <- safetyDF %>% 
  group_by(TIME_PERIOD) %>%
  summarise(across(c(`road accidents`,`road passengers (millions passenger-kilometers)`,`rail accidents`,`rail passengers (millions passenger-kilometers)`),sum),
            ratio = mean(`ratio`, na.rm=TRUE), CRI.score = mean(`CRI.score`,na.rm=TRUE),gdp = mean(`gdp`,na.rm=TRUE),area = mean(`area`,na.rm=TRUE))
safetyByYear$roadAccByVol <- safetyByYear$`road accidents`/ safetyByYear$`road passengers (millions passenger-kilometers)`
safetyByYear$railAccByVol <- safetyByYear$`rail accidents` / safetyByYear$`rail passengers (millions passenger-kilometers)`

plot(safetyByYear$TIME_PERIOD, safetyByYear$roadAccByVol,type="l",col="red",ylim=c(0,0.009),xlab="Year", ylab="accidents by volume",main="Fig 2.1")
lines(safetyByYear$TIME_PERIOD, safetyByYear$railAccByVol,type="l",col="blue")
points(safetyByYear$TIME_PERIOD, safetyByYear$roadAccByVol,col="red",pch=19)
points(safetyByYear$TIME_PERIOD, safetyByYear$railAccByVol,col="blue",pch=19)
legend("topright",legend=c("Road Accidents by Volume", "Rail Accidents by Volume"),col=c("red","blue"),pch=c(19,19))


safetyByGeo <- safetyDF %>%
  group_by(geo) %>%
  summarise(across(c(`road accidents`,`road passengers (millions passenger-kilometers)`,`rail accidents`,`rail passengers (millions passenger-kilometers)`),sum))
safetyByGeo$roadAccByVol <- safetyByGeo$`road accidents` / safetyByGeo$`road passengers (millions passenger-kilometers)`
safetyByGeo$railAccByVol <- safetyByGeo$`rail accidents` / safetyByGeo$`rail passengers (millions passenger-kilometers)`
safetyByGeo$ratio <- safetyByGeo$roadAccByVol / safetyByGeo$railAccByVol

top5 <- order(safetyByGeo$ratio, decreasing = TRUE)[1:5]
safetyByGeo$roadAccByVol[top5]
safetyByGeo$geo[top5]
safetyByGeo$railAccByVol[top5]
ncol(rbind(safetyByGeo$roadAccByVol[top5],safetyByGeo$railAccByVol[top5]))
length(safetyByGeo$geo[top5])
par(mfrow = c(1,2))
barplot( rbind(safetyByGeo$roadAccByVol[top5],safetyByGeo$railAccByVol[top5]), 
         names.arg=paste0(safetyByGeo$geo[top5],"\n",round(safetyByGeo$ratio[top5],digits=3)), 
         beside = TRUE, col=c("red","blue"), legend.text=c("Road Acc by Volume", "Rail Acc by Volume"),
         args.legend = list(x = "topleft"),main="Fig 2.2")


bot5 <- order(safetyByGeo$ratio, decreasing = FALSE)[1:5]
safetyByGeo$roadAccByVol[bot5]
safetyByGeo$geo[bot5]
safetyByGeo$railAccByVol[bot5]
ncol(rbind(safetyByGeo$roadAccByVol[bot5],safetyByGeo$railAccByVol[bot5]))
length(safetyByGeo$geo[bot5])
barplot( rbind(safetyByGeo$roadAccByVol[bot5],safetyByGeo$railAccByVol[bot5]), 
         names.arg=paste0(safetyByGeo$geo[bot5],"\n",round(safetyByGeo$ratio[bot5],digits=3)), 
         beside = TRUE, col=c("red","blue"), legend.text=c("Road Acc by Volume", "Rail Acc by Volume"),
         args.legend = list(x = "topright"),main="Fig 2.3")

safetyDF <- safetyDF[order(safetyDF$TIME_PERIOD,decreasing = FALSE),]

safetyyrsplit <- split(safetyDF,safetyDF$TIME_PERIOD)


safetyt <- function(df){
  return(t.test(df$ratio,mu=1,alternative="greater"))
}

safetyt2side <- function(df){
  return(t.test(df$ratio,mu=1))
}

tlist <- sapply(safetyyrsplit,safetyt)
tsig <- tlist[3,] < 0.05

tlist2side <- sapply(safetyyrsplit,safetyt2side)

viztable <-cbind(tlist[3,],tsig,tlist[2,],tlist2side[4,])
colnames(viztable) <- c("p val", "significant", "n-1","confidence interval")
safetyyrsplit[["2009"]]
write.csv(viztable, "C:/Users/Saketh/Documents/STAT482/safety/viztable.csv")

fixtable <- read.csv("C:/Users/Saketh/Documents/STAT482/safety/viztablemodded.csv")
fixtable <- fixtable %>%
  rowwise() %>%
  mutate(CI = list(c(lowlevel,highlevel))) %>%
  ungroup()


par(mfrow=c(1,1))
plot(safetyByYear$TIME_PERIOD,safetyByYear$ratio,ylim=c(0,6),type="l",col="red",main="Ratio by Year")  
points(safetyByYear$TIME_PERIOD,safetyByYear$ratio,col="red",pch=19)
abline(h=1,lty=2)
segments(x0 = fixtable$year,y0 = fixtable$lowlevel, y1=fixtable$highlevel,col = "blue", lwd = 2)
arrows(x0 = fixtable$year,y0 = fixtable$lowlevel, y1=fixtable$highlevel,col = "blue", angle = 90, code = 3, length = 0.05, lwd=2)

fixtable$width <- fixtable$highlevel - fixtable$lowlevel
fixtable$year[order(fixtable$width,decreasing = TRUE)]

mean(safetyDF[safetyDF$TIME_PERIOD == 2008,]$ratio)
#View(fixtable[c(4,10,15),])

lms <- list()
shapiro <- list()
shapiroP <- c()
coeffsList <- list()
r2list <- c()
lmpvalList <- list()
predRat <- c()

#predict(lm(data = safetyDF[safetyDF$TIME_PERIOD == 2015,], formula = ratio ~ CRI.score + gdp + area),newdata = safetyByYear[safetyByYear$TIME_PERIOD==2016,7:9])
#print(predict(lm(data = safetyDF[safetyDF$TIME_PERIOD == 2015,], formula = ratio ~ CRI.score + gdp + area),newData=safetyByYear[safetyByYear$TIME_PERIOD == 2015,7:9]))

for(i in min(safetyDF$TIME_PERIOD):max(safetyDF$TIME_PERIOD)){
  currlm <- lm(data = safetyDF[safetyDF$TIME_PERIOD == i,], formula = ratio ~ CRI.score + gdp + area)
  lms <- c(lms,list(summary(currlm)))
  shapiro <- c(shapiro,list(t(shapiro.test(resid(currlm)))))
  shapiroP <- c(shapiroP,shapiro.test(resid(currlm))$p.value)
  
  coeffsList <- c(coeffsList,list(t(currlm$coefficients)))
  lmpvalList <- c(lmpvalList,list(t(summary(currlm)$coefficients[,4])))
  predRat <- c(predRat,predict(currlm,newdata = safetyByYear[safetyByYear$TIME_PERIOD == i,7:9]))
  r2list <- c(r2list,summary(currlm)$r.squared)
  
  #print(i)
  #print(currlm$coefficients)
  #print(predict(currlm,newData=safetyByYear[safetyByYear$TIME_PERIOD == i,7:9]))
  #coeffs[j] <- as.data.frame(t(currlm$coefficients))
  #lmpval[j] <- summary(currlm)[,4]
  
  
}
coeffs <- do.call(rbind,lapply(coeffsList,as.data.frame))
lmpval <- do.call(rbind,lapply(lmpvalList,as.data.frame))
sigCoeff <- lmpval[,2:4] < 0.05
# use write.table to output the coefficients into a table
# take average cri, gdp, and area for each year to get the ybar for each year. we can track this throughout years in order to see how the "average country" changes over the years
# dont have a separate tablefor each one in the report, pick significantly interesting years.
shapiroP < 0.05
shapiroP
r2df <- cbind(min(safetyDF$TIME_PERIOD):max(safetyDF$TIME_PERIOD),r2list)
#View(safetyDF[safetyDF$TIME_PERIOD == 2016,])
#plot(lm(data = safetyDF[safetyDF$TIME_PERIOD == 2016,], formula = ratio ~ CRI.score + gdp + area))

yearlyPred <- cbind(min(safetyDF$TIME_PERIOD):max(safetyDF$TIME_PERIOD),predRat)
colnames(yearlyPred) <- c("TIME_PERIOD", "pred")
ggplot(yearlyPred, aes(x = TIME_PERIOD, y= pred))+
  geom_line(col="steelblue")+
  geom_point(col="steelblue")+
  theme_minimal()+
  labs(
    
  )
# new EDA (CRI,GDP,Area)
EuroCRI <- as.data.frame(cbind(unique(safetyDF$country), unique(safetyDF$CRI.score), unique(safetyDF$geo)))
colnames(EuroCRI) <- c("country", "CRI.score", "geo")
EuroCRI$CRI.score <- as.numeric(EuroCRI$CRI.score)
EuroCRI <- EuroCRI[order(EuroCRI$CRI.score, decreasing = TRUE),]
ggplot(EuroCRI, aes(x=reorder(geo,-CRI.score),y=CRI.score))+
  geom_bar(stat = "identity",fill="lightcoral")+
  labs(title = "CRI Score by Country",
       x = "country",
       y = "CRI Score")


ggplot(GDPLong, aes(x = TIME_PERIOD, y = gdp, color = country, group = country)) + 
  geom_line()+
  geom_point()+
  labs(title = "GDP per capita over time",
       x = "Year",
       y = "GDP per capita",
       color = "Country")+
  scale_color_viridis(discrete = TRUE, option = "A") + 
  theme_dark()
length(unique(safetyDF$TIME_PERIOD))

write.csv(fixtable,file="fixtable.csv")

View(safetyDF[safetyDF$TIME_PERIOD == 2020,])

#shapiro.test(resid(lm(data = safetyDF[safetyDF$TIME_PERIOD == 2016,], formula = ratio ~ CRI.score + gdp + area)))
#plot(lm(data = safetyDF[safetyDF$TIME_PERIOD == 2016,], formula = ratio ~ CRI.score + gdp + area))

head(shapiroP)
head(lms)
capture.output(shapiro,file="shapiro.txt")
capture.output(lms,file = "lms.txt")

safetyDF$gdp <- log(safetyDF$gdp)
safetyDF$area <-log(safetyDF$area)



lin1 <- lmer(ratio ~  area + gdp + (1 | geo) ,data=safetyDF[,])
lin1
par(mfrow=c(1,1))
summary(lin1)
  ranef(lin1)
fixef(lin1)
anova(lin1)


predictInterval(lin1,newdata = safetyDF[1,])


get_gof(lin1)
plot(lin1)
qqnorm(resid(lin1))
qqline(resid(lin1))
shapiro.test(resid(lin1))

extrapDat <- read.csv("C:/Users/Saketh/Documents/STAT482/safety/extrapDataSafety.csv")
extrapDatMarginal <- extrapDat[,2:4]
colnames(extrapDatMarginal)[3] <- "geo"
predictInterval(lin1,newdata = extrapDatMarginal, level=0.95,include.resid.var = TRUE)
predictInterval(lin1,newdata = extrapDat, level=0.95,include.resid.var = TRUE)


