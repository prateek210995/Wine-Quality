setwd("E:/GMU/AIT-580/Final Project/Workspace_PJT")
library(tidyverse)
library(corrplot)
library(ggplot2)

rwwine <- read.csv("MixedWine.csv", header = TRUE)

names(rwwine)

summary(rwwine)

sum(is.na(rwwine))

w <- rwwine[c(2:13)]
crw = cor(w)

#Correlation plot between attributes
corrplot(crw, method = "number")

#Scatter plots
plot(rwwine$pH, rwwine$fixed.acidity)
plot(rwwine$total.sulfur.dioxide, rwwine$free.sulfur.dioxide)
plot(rwwine$alcohol, rwwine$density)
plot(rwwine$total.sulfur.dioxide, rwwine$free.sulfur.dioxide)
plot(rwwine$quality, rwwine$alcohol)
plot(rwwine$quality, rwwine$citric.acid)

#Simple visualizations
ggplot(rwwine, aes(x = quality)) + geom_bar(color="black", fill="green") +
  labs(x = "Red and White wine quality", y = "Total number of smaples", title = "Bar plot for total count of quality of wine")

ggplot(rwwine, aes(x = rwwine$rating)) + geom_bar(color="black", fill="orange") +
  labs(x = "Ratings of wine", y = "Total count of each rating for wine", title = "Bar plot for Ratings")

ggplot(data = rwwine, aes(x=rwwine$quality)) + geom_histogram(fill="blue",binwidth = 1) +
  labs(x = "Quality of wines", y = "Total count of each type of quality", title = "Histogram for the quality of wines")

ggplot(data = rwwine) + geom_bar(aes(x=rwwine$ï..wine.type, fill = as.factor(quality))) +
  labs(x = "Types of wines", y = "Total count of each type of quality", title = "Stacked bar plot for types of quality of wines")



#Total count of specific quality of both the wines.
ggplot(rwwine[rwwine$ï..wine.type == "Red",], aes(x = quality)) + geom_bar(fill = "Red", color = "black")+
  labs(x = "Quality of Red wine", y = "Total count of each quality",title = "Bar plot shows total count of red wine")
ggplot(rwwine[rwwine$ï..wine.type == "White",], aes(x = quality)) + geom_bar(fill = "Yellow", color = "black")+
  labs(x = "Quality of White wine", y = "Total count of each quality",title = "Bar plot shows total count of white wine")


#Wine quality increase with increase in alcohol for both wines.
ggplot(rwwine, aes(x = factor(rwwine$quality, levels = c(0,1,2,3,4,5,6,7,8,9,10)), y = rwwine$alcohol)) + 
  geom_boxplot(color="black", fill="green") + labs(x="Red & White wine quality", y="Alcohol amount", title = "Quality vs Alcohol")

#Wine quality increase with increase in citric acid for both wines.
ggplot(rwwine, aes(x = factor(rwwine$quality, levels = c(0,1,2,3,4,5,6,7,8,9,10)), y = rwwine$citric.acid)) + 
  geom_boxplot(color="black", fill="blue") + labs(x="Red & White wine quality", y="Citric acid", title = "Quality vs Citric acid")

#Residual Sugar is less in high quality wines
ggplot(rwwine, aes(x = factor(rwwine$quality, levels = c(0,1,2,3,4,5,6,7,8,9,10)), y = rwwine$residual.sugar)) + 
  geom_boxplot(color="black", fill="Pink") + labs(x="Red & White wine quality", y="Residual Sugar", title = "Quality vs Residual Sugar")

#Total sulphur dioxide is less in white high quality wines
ggplot(data =rwwine[rwwine$ï..wine.type == "White",], aes(x = factor(quality, levels = c(0,1,2,3,4,5,6,7,8,9,10)), y = total.sulfur.dioxide)) + 
  geom_boxplot(color="black", fill="yellow") + labs(x="White wine quality", y="Total sulphur dioxide", title = "Quality vs Total Sulphur dioxide")
#Total sulphur dioxide is less in red high quality wines
ggplot(data =rwwine[rwwine$ï..wine.type == "Red",], aes(x = factor(quality, levels = c(0,1,2,3,4,5,6,7,8,9,10)), y = total.sulfur.dioxide)) + 
  geom_boxplot(color="black", fill="Yellow") + labs(x="Red wine quality", y="Total sulphur dioxide", title = "Quality vs Total Sulphur dioxide")

#Chlorides is less in white high quality wines
ggplot(data =rwwine[rwwine$ï..wine.type == "White",], aes(x = factor(quality, levels = c(0,1,2,3,4,5,6,7,8,9,10)), y = chlorides)) + 
  geom_boxplot(color="black", fill="Purple") + labs(x="White wine quality", y="Chlorides", title = "Quality vs Chlorides")
#Chlorides is less in red high quality wines
ggplot(data =rwwine[rwwine$ï..wine.type == "Red",], aes(x = factor(quality, levels = c(0,1,2,3,4,5,6,7,8,9,10)), y = chlorides)) + 
  geom_boxplot(color="black", fill="Purple") + labs(x="Red wine quality factor", y="Chlorides", title = "Quality vs Chlorides")


#Lesser acidity makes wine more costlier as it is kept for longer time.
ggplot(data =rwwine, aes(x = factor(quality, levels = c(0,1,2,3,4,5,6,7,8,9,10)), y = fixed.acidity)) + 
  geom_boxplot(color="black", fill="Green") + labs(x="Red & White wine quality", y="Fixed Acidity", title = "Quality vs Fixed Acidity")


#Linear Regression in which alcohol is target variable
plot(x= rwwine$quality, y= rwwine$alcohol, main = "Linear Regression Model")
mod <- lm(rwwine$alcohol ~ rwwine$quality)
summary(mod)
abline(mod, col = "Blue", lwd = 3)

#Linear regression in which density is target variable
d<-rwwine[c(5,9,12)]
r <- lm(d$density~., data = d)
plot(r)
summary(r)


#Hypothesis test
boxplot(rwwine$alcohol)
t.test(rwwine$alcohol, alternative = "less", mu = 11)

