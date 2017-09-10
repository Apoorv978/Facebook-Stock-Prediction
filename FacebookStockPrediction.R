# Project Title:  Predicting Stock Exchange Volume of company Facebook on any given day
# NAME: Apoorv Agarwal
# EMAIL: aapoorv75@gmail.com
# COLLEGE: Indian Institute of Technology Madras

#Reading data in R
fbstock.df <- read.csv(paste("FB_Stock_History.csv"),sep = ",")
View(fbstock.df)
library(psych)
describe(fbstock.df[c(1:4,6)])
# Observation 1 : mean of the open value of stocks shows that company is expensive(and expensive infers to successful).
# Observation 2 : sd values of the variables is high enough, showing the growth of the company in past 5 yrs
# observation 3 : out of all the variables, volume of stock exchange's sd value is the highest, showing the increasing popularity of the comapany.

# Opening stock value is something company sets, it seems like volume of stock exchange is one big depnedent variable.
# Possible model will be Volume = F(x1,x2,...)
# Keeping a qualitative approach in mind, open , High , low , these can be our independent variable.
# Updated possible model will be Volume = F(open,High,Low)

#Checking the stastical correlation b/w the dependent variable and independent variable.

#Correlation tests

#open vs volume
attach(fbstock.df)
cor.test(Open,Volume)
#statistically significant correlation as p-value<0.05 and cor = -0.504(negative correlation)
#High vs volume
cor.test(High,Volume)
#statistically significant correlation as p-value<0.05 and cor = -0.500(negative correlation)
#Low vs volume
cor.test(Low,Volume)
#statistically significant correlation as p-value<0.05 and cor = -0.510(negative correlation)

#Visualizing Dependent and independent variables: -

#Histograms
par(mfrow=c(2,2))
with(fbstock.df,hist(Volume,main = "Frequency distribution of stock volume(over 5 years)",xlab = "Stock vol.(units)",ylab = "Count",col="red"))
with(fbstock.df,hist(Open,main = "Frequency distribution of Opening stock value(over 5 years)",xlab = "Stock value(in USD)",ylab = "Count",col="green"))
with(fbstock.df,hist(High,main = "Frequency distribution of Highest stock Price(over 5 years)",xlab = "Stock value(in USD)",ylab = "Count",col="yellow"))
with(fbstock.df,hist(Low,main = "Frequency distribution of Lowest stock price(over 5 years)",xlab = "Stock value(in USD)",ylab = "Count",col="blue"))

#boxplot
with(fbstock.df,boxplot(Volume,main = "Stock volume(over 5 years)",xlab = "Stock vol.(units)",horizontal = TRUE,col = "red"))
with(fbstock.df,boxplot(Open,main = "Opening stock prices(over 5 years)",xlab = "Stock value(in USD)",horizontal = TRUE,col="green"))
with(fbstock.df,boxplot(High,main = "Highest stock prices(over 5 years)",xlab = "Stock value(in USD)",horizontal = TRUE,col = "yellow"))
with(fbstock.df,boxplot(Low,main = "Lowest stock prices(over 5 years)",xlab = "Stock value(in USD)",horizontal = TRUE,col = "blue"))
#Lot of outliers for stock exchange volume

#scatter plot
par(mfrow=c(3,1))
with(fbstock.df,plot(Open,Volume,main = "Stock exchange Volume vs Opening Stock price", xlab = "Stock value(in USD)", ylab="Stock Exchange Volume(in units)",col="red"))
with(fbstock.df,plot(High,Volume,main = "Stock exchange Volume vs Highest Stock price", xlab = "Stock value(in USD)", ylab="Stock Exchange Volume(in units)",col="yellow"))
with(fbstock.df,plot(Open,Volume,main = "Stock exchange Volume vs Lowest Stock price", xlab = "Stock value(in USD)", ylab="Stock Exchange Volume(in units)",col="blue"))
par(mfrow=c(1,1))

#Variance-Covariance matrix
cov(fbstock.df[,c(1:3,6)])
# cov matrix shows that the Stock Exchange volume and Lowest Stock Price of that particular day are more co-variated than other parameters.

#Correlation matrix
cor(fbstock.df[,c(1:3,6)])
# cor matrix shows Stock exchange volume is strongly correlated to Lowest Stock value of a particular day.

#Corrgram
library(corrplot)
library(gplots)
corrplot.mixed(corr = cor(fbstock.df[,c(1:3,6)],use="complete.obs"),
               upper="ellipse",tl.pos = "lt",col=colorpanel(50,"red","gray60","blue"))

