library(ggplot2)
library(gcookbook)
library(MASS)
library(dplyr)
midwest <- midwest


midwest$inmetro <- recode(midwest$inmetro, `0`="n", `1`="y")
ggplot(midwest,aes(x=state,fill=inmetro))+ geom_bar(position="dodge")


## Label race values in data
bwdata <- birthwt
bwdata$race <- factor(bwdata$race, labels=c("white","black","other"))
ggplot(bwdata, aes(x=race, y=bwt)) + geom_boxplot()+
  geom_point(shape=4,aes(color=race))


ggplot(bwdata, aes(x=race, y=bwt)) + geom_boxplot()+
  geom_jitter(shape=4,aes(color=race))
table(bwdata$smoke)              
