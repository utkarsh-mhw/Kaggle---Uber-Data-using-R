###SET UP ENV###
rm(list=ls(all.names=TRUE))
getwd()
setwd("C:/Users/utkar/OneDrive/Documents/projects/data science and big data assgn")
getwd()
###IMPORT DATA###
data<-read.csv("trip.csv")
data<-data[,-1]
###SUMMARISE DATA###
View(data)
str(data)
summary(data)
summary(is.na(data))
###DATA IMPUTATION###

library(mice) #only for visulaising
md.pairs(data)

png("p1.png")
md.pattern(data)
dev.off()

library(VIM)
png("p2.png")
pbox(data,pos=1,cex=0.7)
dev.off()


###imputing values using linear regression

symnum(cor(data,use='complete.obs'))
### trip length is correlated with mostfreqspeed
###use indicator variable
ind=function(t){
  x<-dim(length(t))
         x[which(!is.na(t))]=1
         x[which(is.na(t))]=0
         return(x)
}
data$I<-ind(data$TripLength)
View(data)
###now find coefficients for predictions
summary(lm(TripLength~MostFreqSpeed+MaxSpeed,data=data))
### now predict each na value
for(i in 1:nrow(data))
{
  if(data$I[i]==0)
  {
    data$TripLength[i]=-57.4906+(0.8949*data$MostFreqSpeed[i])+(1.1857*data$MaxSpeed[i])
  }
}
data<-data[,-ncol(data)]
View(data)


#####IMPUTATION COMPLETED####

### EXPLORATORY DATA ANALYSIS###

str(data) ##only numeric present
sapply(data,class)
sapply(data,range)
summary(data)
round(cor(data),2)


png("p3.png")
library(LaplacesDemon)
plotMatrix(cor(data),pch=7,cex=1.2,col=rainbow(ncol(data)))
dev.off()


png("p4.png")
hist(data$TripLength)
dev.off()
png("p5.png")
hist(data$MaxSpeed)
dev.off()
png("p6.png")
hist(data$MostFreqSpeed)
dev.off()
png("p7.png")
hist(data$TripDuration)
dev.off()
png("p8.png")
hist(data$Brakes)
dev.off()
png("p9.png")
hist(data$IdlingTime)
dev.off()
png("p10.png")
hist(data$Honking)
dev.off()
###DATA NORMALISATION###


##### (x-min)/(max-min)

normalise<-function(t)
{
  x<-dim(length(t))
  for(i in 1:length(t))
  {
    x[i]<-((t[i]-min(t))/sd(t))
  }
  return(x)
}

data$ntl<-normalise(data$TripLength)
data$nms<-normalise(data$MaxSpeed)
data$nmfs<-normalise(data$MostFreqSpeed)
data$ntd<-normalise(data$TripDuration)
data$nbks<-normalise(data$Brakes)
data$nit<-normalise(data$IdlingTime)
data$nhk<-normalise(data$Honking)
ndata<-data.frame(data$ntl,data$nms,data$nmfs,data$ntd,data$nbks,data$nit,data$nhk)

View(ndata)

###k means

kcl<-kmeans(ndata,2)
kcl
###reduce wcss by optimally choosing number of clusters using elbow method


k.max<-15
wss<-rep(NA,k.max)
nclust<-list()
for(i in 1:k.max)
{
  temp<-kmeans(ndata,i)
  wss[i]<-temp$tot.withinss
  nclust[[i]]<-temp$size
}
png("p11.png")
plot(1:k.max,wss,main="elbow method",type="b",pch=7,xlab="number of clusters k",ylab="total wcss")
dev.off()

##optimal k=4

cl_final<-kmeans(ndata,4)
cl_final


cl_final$tot.withinss
kcl$tot.withinss

### plotting clusters using PCA

pr<-prcomp(ndata)
pr
summary(pr)
png("p11.png")
plot(pr,type="l")
dev.off()
png("p12.png")
biplot(pr,scale=0)
dev.off()

###extract pc scores

str(pr)
pr$x
ndata<-cbind(ndata,pr$x[,1:2])
head(ndata)


###plot with ggplot2

library(cluster)
png("p14.png")
clusplot(ndata,cl_final$cluster,color=TRUE,shade=TRUE,labels=23,lines=0)
dev.off()
png("p15.png")
with(ndata, pairs(ndata[,-c(8,9)], col=c(1:4)[cl_final$cluster]))   
dev.off()
  
  
  
  
  
