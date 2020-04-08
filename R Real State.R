rp <- read.csv(file.choose())
View(rp)
library(ggplot2)
install.packages("Amelia")
library(Amelia)
summary(rp)
?describe()
missmap(rp)#FOR CHECKING FOR MISSING DATA IN OUR DATASET
rp <- rp[,-1]#1 st column iis serial no 
View(rp)
ggplot(rp,aes(rp$X1.transaction.date,rp$Y.house.price.of.unit.area))+geom_boxplot()
summary(rp$X2.house.age)
hist(rp$X2.house.age)
install.packages("dplyr")#For using mutate function for creating new variable
library(dplyr)
install.packages("magrittr")
install.packages("GGally")
install.packages("tidyverse")
library(tidyverse)
library(GGally)
library(magrittr)#For using %>%
rp <- rp %>%mutate(age <- as.factor(findInterval(rp$X2.house.age,c(10,20,30,40))))#findinterval is used to create interval as required
#AND mutate() is used crete a new variable which is a manipulation of an existing variable
view(rp)
rename(rp$`... <- NULL`,age)
ggplot(rp,aes(rp$`... <- NULL`,rp$Y.house.price.of.unit.area))+geom_boxplot()
#so we see that the age of house between (20 to 30)and (10 to 20) has higher price
hist(rp$X3.distance.to.the.nearest.MRT.station)
rp <- rp %>%mutate(dis <- as.factor(findInterval(rp$X3.distance.to.the.nearest.MRT.station,c(1000,2000,3000,4000,5000,6000))))
view(rp)
ggplot(rp,aes(rp$`... <- NULL`,rp$Y.house.price.of.unit.area))+geom_boxplot()
# we see that the PRICEis high withoutlier with the distance between (0 to 2000)
hist(rp$X4.number.of.convenience.stores)
rp <- rp %>%mutate(store <- as.factor(findInterval(rp$X4.number.of.convenience.stores,c(2,4,6,8,10))))
ggplot(rp,aes(rp$`... <- NULL`,rp$Y.house.price.of.unit.area))+geom_boxplot()
#we see that price where no of store is 6 and 10 is high 
view(rp)
rp <- rp[-8]
cor(rp)
#longitude and distance to nearest MRT station is -0.80
library(corpcor)
cor2pcor(cor(rp))
rp1 <- rp[c(-271,-114,-36,-221,-313),]
model.rp <- lm(rp1$Y.house.price.of.unit.area~rp1$X1.transaction.date+rp1$X2.house.age+rp1$X3.distance.to.the.nearest.MRT.station+rp1$X4.number.of.convenience.stores+rp1$X5.latitude,rp1)
summary(model.rp)
library(mvinfluence)
influencePlot(model.rp)
plot(model.rp)


