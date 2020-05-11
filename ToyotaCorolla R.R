price <- read.csv(file.choose())
View(price)
c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")
attach(price)
cost <- price[,c(3,4,7,9,13,14,16,17,18)]
View(cost)
