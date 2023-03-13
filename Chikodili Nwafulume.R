#Read the .csv file
Trade <- read.csv("Trade_data.csv", header= TRUE)
#Inspect the dataset to view its contents
names(Trade)
#To know the top and end part of the dataset
head(Trade)
tail(Trade)
#To display the entire dataset
View(Trade)
#To know the dimension of the dataset
dim(Trade)
#To get the variables structure
str(Trade)
#To get the summary statistics of the data objects
summary(Trade)
describe(Trade)
#To get the skewness and kurtosis, install psych
library(psych)

#Creating dataframe
Trade_China <- (Trade[Trade$Country_Name == "China",])
Trade_United_States <- (Trade[Trade$Country_Name == "United_States",])
Trade_Germany <- (Trade[Trade$Country_Name == "Germany",])
Trade_Netherlands <- (Trade[Trade$Country_Name == "Netherlands",])
Trade_United_Kingdom <- (Trade[Trade$Country_Name == "United_Kingdom",])
Trade_France <- (Trade[Trade$Country_Name == "France",])
Trade_Japan <- (Trade[Trade$Country_Name == "Japan",])
Trade_Hong_Kong <- (Trade[Trade$Country_Name == "Hong_Kong",])
Trade_Singapore <- (Trade[Trade$Country_Name == "Singapore",])
Trade_Canada <- (Trade[Trade$Country_Name == "Canada",])

#Analysis
China_Analysis=describe(Trade_China)
United_States_Analysis=describe(Trade_United_States)
Germany_Analysis=describe(Trade_Germany)
Netherlands_Analysis=describe(Trade_Netherlands)
United_Kingdom_Analysis=describe(Trade_United_Kingdom)
France_Analysis=describe(Trade_France)
Japan_Analysis=describe(Trade_Japan)
Hong_Kong_Analysis=describe(Trade_Hong_Kong)
Singapore_Analysis=describe(Trade_Singapore)
Canada_Analysis=describe(Trade_Canada)

#View Country's Analysis
#Import tidyverse for ease data manipulation and visualization
library(tidyverse)
view(China_Analysis)
view(United_States_Analysis)
view(Germany_Analysis)
view(Netherlands_Analysis)
view(United_Kingdom_Analysis)
view(France_Analysis)
view(Japan_Analysis)
view(Hong_Kong_Analysis)
view(Singapore_Analysis)
view(Canada_Analysis)

# Addition of new columns to DataFrame
#Creation of Key Exports column 
Trade$Key_exports<- as.numeric(apply(Trade[,5:10],1,sum))
head(Trade)

# Creation of Trade exports Column
Trade$Trade_exports<- as.numeric(apply(Trade[,12:13],1,sum))
head(Trade)

# Creation of Trade imports Column
Trade$Trade_imports<- as.numeric(apply(Trade[,14:15],1,sum))
head(Trade)

#Creation of Balance of Trade Column
Trade$Bal_of_Trade<-(Trade$Trade_exports-Trade$Trade_imports)
head(Trade)

#TASK 4.1..Comparative Analysis of my needed variables
library(moments)
skewness(Trade$GDP)
sd(Trade$GDP)
kurtosis(Trade$GDP)

mode = function() {
  #call
  return(names(sort(-table(Trade$GDP)))[1])
}
mode()

#Trade Imports
skewness(Trade$Trade_imports)
sd(Trade$Trade_imports)
kurtosis(Trade$Trade_imports)

mode = function() {
  #call
  return(names(sort(-table(Trade$Trade_imports)))[1])
}
mode()

#Trade Exports
skewness(Trade$Trade_exports)
sd(Trade$Trade_exports)
kurtosis(Trade$Trade_exports)

mode = function() {
  #call
  return(names(sort(-table(Trade$Trade_exports)))[1])
}
mode()

#Bal_of_Trade
skewness(Trade$Bal_of_Trade)
sd(Trade$Trade_Bal_of_Trade)
kurtosis(Trade$Bal_of_Trade)

mode = function() {
  #call
  return(names(sort(-table(Trade$Bal_of_Trade)))[1])
}
mode()


#TASK 4.2
#Correlation
library(ggstatsplot)
library(corrplot)

#Correlation Table for all the variables
cor(Trade[,3:25])

#Corrplot
corrplot(cor(Trade[, 3:25]), 
         method = "number", 
         type = "upper", 
         tl.cex = 0.3);


#TASK 4.3
# PERFORM MULTIPLE LINEAR REGRESSION USING CHINA
China <- read.csv("Chinatrade.csv", header= TRUE)

# Creation of China Trade exports Column
China$Trade_exports<- as.numeric(apply(China[,12:13],1,sum))
head(China)

# Creation of China Trade imports Column
China$Trade_imports<- as.numeric(apply(China[,14:15],1,sum))
head(China)

#Correlation matrix for China only
cor_China = round(cor(China[, 3:27]),
digits = 3 # rounded to 2 decimals
)

cor_China

#Executing Linear Regression Analysis
mymodel <- lm(GDP ~ Trade_imports + Trade_exports, data = China)
summary(mymodel)

#review of 4 assumptions of MLR
#Plot scatter Plot for Linearity
pairs(China[,c(26,27)], lower.panel = NULL, pch =19, cex = 0.2)


#Checking for residual's independence
plot(mymodel, 1)

#Checking for Normality of Residuals
plot(mymodel, 2)

#Checking for Homoscedasticity
plot(mymodel, 3)

#Checking for the multicollinrearity
library(car)
vif(mymodel)

# PERFORM SIMPLE LINEAR REGRESSION FOR GDP and Trade Export
reg1 <- lm(GDP ~ Trade_exports, data = China)
summary(reg1)

#Plot scatter Plot for GDP
plot(GDP ~ Trade_exports, China,
     col = "blue", 
     main = "Regression: GDP of China",
     xlab = "Trade Exports",
     ylab = "GDP")

#Regression Line
abline(reg1, col="Red")


#TASK 4.4
#Time Series For GDP of China
GDP_timeseries = ts(Trade$GDP)
GDP_timeseries <- ts(Trade$GDP, frequency=12, start=c(2011, 1))
GDP_timeseries
plot.ts(GDP_timeseries)


#Decomposing Time series for non seasonal data
library("TTR")
GDP_timeseriesSMA3 <- SMA(GDP_timeseries, n=3)
plot.ts(GDP_timeseriesSMA3)

GDP_timeseriesSMA10 <- SMA(GDP_timeseries, n=10)
plot.ts(GDP_timeseriesSMA10)


library(urca)
library(caret)
library(TSstudio)
library(tseries)
library(forecast)



CGDP <- read.csv("Chinatrade.csv", header=TRUE)
CGDP_timeseries <- ts(CGDP$GDP, start=c(2011))
plot.ts(CGDP_timeseries)

#Generating ACF and PACF to select a candidate ARIMA
ggAcf(CGDP_timeseries) + ggtitle("ACF of CGDP")
ggPacf(CGDP_timeseries) + ggtitle("PACF of CGDP")

#Diff to remove non stationary component
DiffCGDP_timeseries <- diff(CGDP_timeseries)
autoplot(DiffCGDP_timeseries)+ggtitle("Differenced CGDP, 2011 to 2020") + labs(x = "Year", y = "CGDP")
ggAcf(DiffCGDP_timeseries) + ggtitle("ACF of CGDP Differenced")
ggPacf(DiffCGDP_timeseries) + ggtitle("PACF of CGDP Differenced")

#Testing for stationarity using Augumented Dicker Fuller
adf.test(CGDP_timeseries)
adf.test(CGDP_timeseries, k = 1)
adf.test(CGDP_timeseries, k = 2)
adf.test(DiffCGDP_timeseries)

#Testing for stationarity using Phillips Perron
pp.test(CGDP_timeseries)
pp.test(DiffCGDP_timeseries)

#Testing for stationarity using KPSS
kpss.test(CGDP_timeseries)
kpss.test(DiffCGDP_timeseries)


#Training the model with training and testing dataset
split_CGDP_timeseries <- ts_split(CGDP_timeseries, sample.out = 3)
training <- split_CGDP_timeseries$train
testing <- split_CGDP_timeseries$test
length(training)
length(testing)

#Diagonising the training data
arima_diag(training)

#Using autoarima (0,1,0) as model 1
auto <- auto.arima(training, seasonal = TRUE)
auto
autoplot(auto)

#Using Arima 110 as model 2
arima110 <- arima(training, order = c(1,1,0))
autoplot(arima110)

#Using Arima 111 as model 3
arima111 <- arima(training, order = c(1,1,1))
autoplot(arima111)

#Using Arima 011 as model 4
arima011 <- arima(training, order = c(0,1,1))
autoplot(arima011)


#Forcasting with model 1
fcast1 <- forecast(auto, h = 5)
test_forecast(actual = CGDP_timeseries, forecast.obj = fcast1, test = testing)
accuracy(fcast1,testing)

#Forcasting with model 2
fcast2 <- forecast(arima110, h = 5)
test_forecast(actual = CGDP_timeseries, forecast.obj = fcast2, test = testing)
accuracy(fcast2,testing)

#Forcasting with model 3
fcast3 <- forecast(arima111, h = 5)
test_forecast(actual = CGDP_timeseries, forecast.obj = fcast3, test = testing)
accuracy(fcast3,testing)

#Forcasting with model 4
fcast4 <- forecast(arima011, h = 5)
test_forecast(actual = CGDP_timeseries, forecast.obj = fcast4, test = testing)
accuracy(fcast4,testing)


#Finalfit with model 1
finalfit1 <- auto.arima(CGDP_timeseries, seasonal =TRUE)
autoplot(finalfit1)

finalfit2 <- arima(CGDP_timeseries, order = c(1,1,0))
autoplot(finalfit2)

finalfit3 <- arima(CGDP_timeseries, order = c(1,1,1))
autoplot(finalfit3)

finalfit4 <- arima(CGDP_timeseries, order = c(0,1,1))
autoplot(finalfit4)


#Forecasting with the 4th model with the lowest RMSE
fcast4 <- forecast(CGDP_timeseries, model = finalfit4, h = 5)
plot_forecast(fcast4)
summary(fcast4)

#Checking dist. of forcast error
acf(fcast4$residuals, lag.max=10)
Box.test(fcast4$residuals, lag=10, type="Ljung-Box")

# time plot forecast error
plot.ts(fcast4$residuals)
fcast4$residuals <- fcast4$residuals
!is.na(fcast4$residuals)

# make a histogram
plotForecastErrors(fcast4$residuals)



#TASK 4.5
#Comparative Hypothesis

install.packages("datarium")
install.packages("qqplotr") 
install.packages("ggplot2")
library(ggplot2)
library(datarium)
library(qqplotr)

Mysample <- read.csv("Mysample.csv", header= TRUE)
head(Mysample)
str(Mysample)

#Checking the normality of the data using Q-Q plot
ggplot(mapping = aes(sample=Mysample$GDP)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="green") +
  xlab("Theoretical") + ylab("Sample")


#When data is normally distributed
Mysample$GDP <- rnorm(20) 
#perform shapiro-wilk test 
shapiro.test(Mysample$GDP) 

#Kolmogorov smirn off
Mysample <- rnorm(20)
ks.test(Mysample, 'pnorm')


#Hypothesis Testing
#1st Test.....Using Independent Two Sample T-test
boxplot(Mysample$GDP ~ Mysample$Country_Name, data=Mysample, names=c("China", "United_States"), 
        xlab="Country",
        ylab="GDP", main="GDP for China and United_States")

#Performing the test
t.test(Mysample$GDP ~ Mysample$Country_Name, Mysample)


#2nd test---Using a one way anova

#N0 4 Assumptions...Checking for any outliers
boxplot(Mysample$GDP ~ Mysample$Country_Name , data=Mysample, names=c("China", "United_States"),
        xlab="Country", ylab="GDP",
        main="GDP for Each Country")

#No 6 Assumptions....Checking for Homogenity
bartlett.test(Mysample$GDP ~ Mysample$Country_Name , data=Mysample)

#One-way Anova Test
oneway.test(Mysample$GDP ~ Mysample$Country_Name, data=Mysample, var.equal = TRUE)


#Wilcox test
wilcox.test(Mysample$GDP ~ Mysample$Country_Name, data=Mysample)



