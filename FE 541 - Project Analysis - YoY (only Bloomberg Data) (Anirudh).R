library(corrplot)
# Load the data
#dollar <- read.csv("/Users/simranjariwala/Financial_analytics/FE541/data/dollar_price.csv")
dollar

#debt <- read.csv("/Users/simranjariwala/Financial_analytics/FE541/data/Public Data v2.csv")
debt
#CAD <- read.csv("/Users/simranjariwala/Financial_analytics/FE541/data/Current Account Balance v2.csv")
CAD
#CPI <- read.csv("/Users/simranjariwala/Financial_analytics/FE541/data/CPI v2.csv")
CPI
#FDTR <- read.csv("/Users/simranjariwala/Financial_analytics/FE541/data/FDTR v2.csv")
FDTR
#GDP <- read.csv("/Users/simranjariwala/Financial_analytics/FE541/data/GDP Performance v2.csv")
GDP

dollar <- read.csv("Dollar Price Movement.csv")
dollar

debt <- read.csv("Public Data v2.csv")
debt
CAD <- read.csv("Current Account Balance v2.csv")
CAD
CPI <- read.csv("CPI v2.csv")
CPI
FDTR <- read.csv("FDTR v2.csv")
FDTR
GDP <- read.csv("GDP Performance v2.csv")
GDP


# Writing Functions to convert quarterly data to yearly data 
convert.year <- function(dataframe, date, price){
  dataframe <- dataframe[-(1:6), ]
  dataframe[ , price] <- as.numeric(paste(dataframe[ , price]))
  dataframe$date1 <- as.POSIXlt(as.character(dataframe[ , date]), format = "%d-%m-%Y")
  year.price <- as.numeric(dataframe[which(dataframe$date1$mon == 2), price])
  dates <- dataframe$date1[which(dataframe$date1$mon == 2)]
  names(year.price) <- dates[1:length(dates) - 1]
  return(year.price)
}

convert.year.change <- function(dataframe, date, price){
  dataframe <- dataframe[-(1:6), ]
  dataframe[ , price] <- as.numeric(paste(dataframe[ , price]))
  dataframe$date1 <- as.POSIXlt(as.character(dataframe[ , date]), format = "%d-%m-%Y")
  year.price <- as.numeric(dataframe[which(dataframe$date1$mon == 2), price])
  year.change <- year.price[1:length(year.price) - 1] - year.price[2:length(year.price)]
  dates <- dataframe$date1[which(dataframe$date1$mon == 2)]
  names(year.change) <- dates[1:length(dates) - 1]
  return(year.change)
}

convert.year.percent <- function(dataframe, date, price){
  dataframe <- dataframe[-(1:6),  ]
  dataframe[ , price] <- as.numeric(paste(dataframe[ , price]))
  dataframe$date1 <- as.POSIXlt(as.character(dataframe[ , date]), format = "%d-%m-%Y")
  year.price <- as.numeric(dataframe[which(dataframe$date1$mon == 2), price])
  year.percent <- (year.price[1:length(year.price)-1] - year.price[2:length(year.price)]) / abs(year.price[1:length(year.price) - 1])
  dates <- dataframe$date1[which(dataframe$date1$mon == 2)]
  names(year.percent) <- dates[1:length(dates) - 1]
  return(year.percent)
}


# Converting Quarterly data
# convert to dataframe
dollar <- as.data.frame(dollar)
# deleting unwanted rows
dollar <- dollar[-(1:6), ]
dollar <- dollar[-nrow(dollar), ]
nrow(dollar)
dollar$Security <- as.POSIXlt(as.character(dollar[ , 1]), format = "%d-%m-%Y")
dollar.quart <- dollar$BBDXY.Index
dollar.quart.change <- dollar$X
dollar.quart.percent <- dollar$X.1

length(dollar.quart)

FDTR <- as.data.frame(FDTR)
FDTR <- FDTR[-(1:6), ]
nrow(FDTR)
FDTR.quart <- FDTR$FDTR.....Index
FDTR.quart.change <- FDTR$X
FDTR.quart.percent <- FDTR$X.1

CPI <- as.data.frame(CPI)
CPI <- CPI[-(1:7), ]
nrow(CPI)
CPI.quart <- CPI$CPI.YOY..Index
CPI.quart.change <- CPI$X
CPI.quart.percent <- CPI$X.1

CAD <- as.data.frame(CAD)
CAD <- CAD[-(1:7), ]
nrow(CAD)
CAD.quart <- CAD$USNCBAL..Index
CAD.quart.change <- CAD$X
CAD.quart.percent <- CAD$X.1

debt <- as.data.frame(debt)
debt <- debt[-(1:6), ]
nrow(debt)
debt.quart <- debt$DEBPTOTL.Index
debt.quart.change <- debt$X
debt.quart.percent <- debt$X.1

GDP <- as.data.frame(GDP)
GDP <- GDP[-(1:6), ]
nrow(GDP)
GDP.quart <- GDP$GDP.CURY.Index
GDP.quart.change <- GDP$X
GDP.quart.percent <- GDP$X.1

date <- dollar$Security
# combining the data to a dataframe
quarterly.data <- data.frame(dollar.quart.percent, FDTR.quart.change, FDTR.quart, 
                                   CPI.quart.percent, CPI.quart, CAD.quart.percent, debt.quart.percent,
                                   GDP.quart.percent, GDP.quart)
quarterly.data
rownames(quarterly.data) <- c("	30-03-2019", "31-12-2018", "28-09-2018", "29-06-2018", "30-03-2018",
                              "29-12-2017	", "29-09-2017", "30-06-2017", "31-03-2017", "30-12-2016",
                              "30-09-2016", "30-06-2016", "31-03-2016", "31-12-2015", "30-09-2015",
                              "30-06-2015", "31-03-2015", "31-12-2014", "30-09-2014", "30-06-2014",
                              "31-03-2014", "31-12-2013", "30-09-2013", "28-06-2013", "29-03-2013",
                              "31-12-2012", "28-09-2012", "29-06-2012", "30-03-2012", "30-12-2011",
                              "30-09-2011", "30-06-2011", "31-03-2011", "31-12-2010", "30-09-2010",
                              "30-06-2010", "31-03-2010", "31-12-2009", "30-09-2009", "30-06-2009",
                              "31-03-2009", "31-12-2008", "30-09-2008", "30-06-2008", "31-03-2008",
                              "31-12-2007", "28-09-2007", "29-06-2007", "30-03-2007", "29-12-2006",
                              "29-09-2006", "30-06-2006", "31-03-2006", "30-12-2005", "30-09-2005",
                              "30-06-2005", "31-03-2005")

# Converting the data
dollar.change <- convert.year.change(dollar, 1, 2)
dollar.percent <- convert.year.percent(dollar, 1, 2)

debt.year <- convert.year(debt, 1, 2)
debt.year <- debt.year[-(length(debt.year))]
debt.change <- convert.year.change(debt, 1, 2)
debt.percent <- convert.year.percent(debt, 1, 2)

CAD.year <- convert.year(CAD, 1, 2)
CAD.year <- CAD.year[-(length(CAD.year))]
CAD.change <- convert.year.change(CAD, 1, 2)
CAD.percent <- convert.year.percent(CAD, 1, 2)

GDP.year <- convert.year(GDP,1,2)
GDP.year <- GDP.year[-length(GDP.year)]
GDP.change <- convert.year.change(GDP,1,2)
GDP.percent <- convert.year.percent(GDP,1,2)

CPI.year <- convert.year(CPI,1,2)
CPI.year <- CPI.year[-length(CPI.year)]
CPI.change <- convert.year.change(CPI,1,2)
CPI.percent <- convert.year.percent(CPI,1,2)

FDTR.year <- convert.year(FDTR,1,2)
FDTR.year <- FDTR.year[-length(FDTR.year)]
FDTR.change <- convert.year.change(FDTR,1,2)


# combining data to a data frame
# currency.data <- data.frame(cbind(dollar.percent,debt.year,debt.change,debt.percent,
#                                   CAD.year,CAD.change,CAD.percent,GDP.year,GDP.change,
#                                   GDP.percent,CPI.year,CPI.change,CPI.percent,FDTR.year,
#                                   FDTR.change))

# combining data to a data frame
yearly.data <- data.frame(cbind(dollar.percent, FDTR.change, FDTR.year, CPI.percent, CPI.year,
                                CAD.percent, debt.percent, GDP.percent, GDP.year))
yearly.data 

# Here, Change in dollar price is the response variable and change in federal rate, 
# change in inflation rate, change in current account, change in public debt and 
# change in GDP are the explanatory variables.


# HISTOGRAM
# distribution of Change in Dollar Rate
skewness(yearly.data$dollar.percent)
# the skewness for Change in Dollar Rates is Positive, so the distribution is right skewed.
par(mfrow = c(1,2))

hist(yearly.data$dollar.percent, main = "Histogram for Change in Dollar Rate",
     xlab = "Change in Dollar Rate", col = "grey")
# the histogram shows the distribution is not normal. The distribution is right skewed.
qqnorm(yearly.data$dollar.percent, main = "QQ Plot of Dollar Rate")
# the qqnorm does not show a linear relationship but shows a positive incrementing relation.
# There are residuals present towards the right top.

# distribution of Change in Federal Rate
skewness(yearly.data$FDTR.change)
# the skewness for Change in Federal Rates is Negative, so the distribution is left skewed.
hist(yearly.data$FDTR.change, main = "Histogram for Change in Federal Rate",
     xlab = "Change in Federal Rate", col = "grey")
# the histogram shows the distribution is not normal. The distribution is heavy tailed and
# left skewed.
qqnorm(yearly.data$FDTR.change)
# the qqnorm does not show a linear relationship but shows a positive incrementing relation.
# There are residuals present towards the right top.

# distribution of Federal Rate
skewness(yearly.data$FDTR.year)
# the skewness for Federal Rates is Positive, so the distribution is right skewed.
hist(yearly.data$FDTR.year, main = "Histogram for Federal Rate",
     xlab = "Federal Rate", col = "grey")
# the histogram shows the distribution is not normal. The distribution is heavy tailed and
# right skewed.
qqnorm(yearly.data$FDTR.year)
# the qqnorm does not show a linear relationship but shows a positive incrementing relation.
# There are residuals present towards the right top.

# distribution of Percentage Change in Inflation
skewness(yearly.data$CPI.percent)
# the skewness for inflation is also negative It means the distribution is left skewed.
hist(yearly.data$CPI.percent, main = "Histogram Percentage Change in Inflation",
     xlab = "Percentage Change in Inflation", col = "grey")
# the distribution for inflation is not normally distributed. It is slightly skewed towards
# the left
qqnorm(yearly.data$CPI.percent)
# the plot shows few unusual values towards the start and end period bottom left which are residuals.

# distribution of Inflation Rate
skewness(yearly.data$CPI.year)
# the skewness for inflation is also negative It means the distribution is left skewed.
hist(yearly.data$CPI.year, main = "Histogram for Inflation",
     xlab = "Inflation Rate", col = "grey")
# the distribution for inflation is not normally distributed. It is slightly skewed towards
# the left
qqnorm(yearly.data$CPI.year)
# the plot shows few unusual values towards the start and end period ie the top right and 
# bottom left which are residuals.

# distribution of Percentage Change in Current Account
skewness(yearly.data$CAD.percent)
# the skewness is positive for current account which shows it is right skewed.
hist(yearly.data$CAD.percent, main = "Histogram for Change in Current Account",
     xlab = "Percentage Change in Current Account", col = "grey")
# the histogram for current account shows that there are higher values towards the right.
# the tail is towards the left so the distribution is right skewed. The current account had
# more deficit 
qqnorm(yearly.data$CAD.percent)
# there are residuals or extreme or unusal values present which are the quarters where the 
# current account is in extreme surplus or deficit.

# distribution of percentage Change in Public Debt
skewness(yearly.data$debt.percent)
# the skewness for change in public debt is also positive which means it is right skewed.
hist(yearly.data$debt.percent, main = "Histogram for Change in Public Debt",
     xlab = "Percentage Change in Public Debt", col = "grey")
# the histogram shows the distribution for percentage change in debt is right skewed.
qqnorm(yearly.data$debt.percent)
# it shows a plot where values initially are linear then become unusual.

# distribution of Percentage Change in GDP
skewness(yearly.data$GDP.percent)
# the change in GDP is left skewed as the distribution is negative
hist(yearly.data$GDP.percent, main = "Histogram for Percentage Change in GDP",
     xlab = "Percantage Change in GDP", col = "lightgrey")
# the histogram for change in GDP is slightly left skewed and close to normal but not 
# perfectly distributed.
qqnorm(yearly.data$GDP.percent)
# the plot shows resuiduals like other explanatory variables. It is slightly right skewed 
# with a tail towards right.

# distribution of GDP
skewness(yearly.data$GDP.year)
# the GDP is left skewed as the distribution is negative
hist(yearly.data$GDP.year, main = "Histogram for GDP",
     xlab = "GDP Growth Rate", col = "grey")
# the histogram for change in GDP is slightly left skewed and close to normal but not 
# perfectly distributed.
qqnorm(yearly.data$GDP.year)
# the plot shows resuiduals like other explanatory variables. It is slightly right skewed 
# with a tail towards right.

# plotting the matrix of all variables
# matrix plot helps to visualize relationship between all variables
plot(yearly.data, pch = 16, col = "blue",
     main = "Matrix Scatterplot of Factors affecting exchange rate")
# the plot shows other than the variable Change in CAC and Change in Debt have an
# increasing relationship. there is some visible linearity.

# SCATTERPLOTS
colnames(yearly.data)
# scatterplot each variable and dollar price
plot(dollar.percent, dollar.percent)

par(mfrow = c(2,4))

plot(FDTR.change, dollar.percent, xlab = "Change in Interest Rate", ylab = "Dollar Percent Change", 
     main = "Interest rate & Dollar")
plot(FDTR.year, dollar.percent, xlab = "Interest Rate", ylab = "Dollar Percent Change",
     main = "Interest rate & Dollar")
plot(CPI.percent, dollar.percent,xlab = "Change in Inflation", ylab = "Dollar Percent Change",
     main = "Inflation & Dollar")
plot(CPI.year, dollar.percent, xlab = "Inflation Rate", ylab = "Dollar Percent Change", 
     main = "Inflation & Dollar")
plot(CAD.percent, dollar.percent, xlab = "Change in CAD Balance", ylab = "Dollar Percent Change",
     main = "CAD Change & Dollar")
plot(debt.percent, dollar.percent,xlab = "Change in Public Debt Balance", ylab = "Dollar Percent Change",
     main = "Debt Change & Dollar")
plot(GDP.percent, dollar.percent,xlab = "Change in GDP growth rate", ylab = "Dollar Percent Change", 
     main = "GDP & Dollar")
plot(GDP.year, dollar.percent,xlab = "GDP growth rate", ylab = "Dollar Percent Change", main = "GDP & Dollar")

# plot(CAD.change[1:length(year.change)],year.change)
# plot(debt.change[1:length(year.change)],year.change)
# plot(CPI.year[1:length(year.change)],year.change)

# CORRELATION
# correlation for dollar price and other factors
# for yearly data
corr.yearly <- cor(dollar.percent, yearly.data)                            
corr.yearly




# Plot a correlation graph
yearly.cor <- cor(yearly.data[1:9, ])
corrplot(yearly.cor, method = "number")
# the plot shows a high correlation between Change in Inflation and Change in GDP than other 
# variables. the plot also shows there is less evidence of relation between variables 
# Change in Debt and Change in CAC
# plotting individual correlation between variables
cor(yearly.data$FDTR.change, yearly.data$CPI.percent)   # 0.2403311
cor(yearly.data$FDTR.change, yearly.data$CPI.year)  # 0.1018759
cor(yearly.data$FDTR.change, yearly.data$CAD.percent) # -0.5707275
cor(yearly.data$FDTR.change, yearly.data$debt.percent)  # -0.3126245
cor(yearly.data$FDTR.change, yearly.data$GDP.percent)  # 0.4010789
cor(yearly.data$FDTR.change, yearly.data$GDP.year)  # 0.6523382
cor(yearly.data$FDTR.year, yearly.data$CPI.percent)   #  0.2379687
cor(yearly.data$FDTR.year, yearly.data$CPI.year)  # 0.543204
cor(yearly.data$FDTR.year, yearly.data$CAD.percent) # -0.2136807
cor(yearly.data$FDTR.year, yearly.data$debt.percent)  # -0.2363627
cor(yearly.data$FDTR.year, yearly.data$GDP.percent)  # -0.06593114
cor(yearly.data$FDTR.year, yearly.data$GDP.year)  # 0.4462805
cor(yearly.data$CPI.percent, yearly.data$CAD.percent)  # -0.3275693
cor(yearly.data$CPI.percent, yearly.data$debt.percent) # -0.05384543
cor(yearly.data$CPI.percent, yearly.data$GDP.percent)  # 0.326877
cor(yearly.data$CPI.percent, yearly.data$GDP.year) # 0.2392059
cor(yearly.data$CPI.year, yearly.data$CAD.percent)  # -0.4248138
cor(yearly.data$CPI.year, yearly.data$debt.percent) # -0.1415322
cor(yearly.data$CPI.year, yearly.data$GDP.percent)  # 0.4414353
cor(yearly.data$CPI.year, yearly.data$GDP.year) # 0.5320583
cor(yearly.data$CAD.percent, yearly.data$debt.percent)  # 0.5168109
cor(yearly.data$CAD.percent, yearly.data$GDP.percent) # -0.7432977
cor(yearly.data$CAD.percent, yearly.data$GDP.year)  # -0.797435
cor(yearly.data$debt.percent, yearly.data$GDP.percent) # -0.2212427
cor(yearly.data$debt.percent, yearly.data$GDP.year) # -0.6520298
# this shows there is high correlation between Change in Inflation and Change in GDP and Change in
# Federal Rate and GDP and a negative correlation bewteen Change in CAD an Change in GDP. 


# LINEAR REGRESSION MODEL
colnames(yearly.data)
currency.lm <- lm(dollar.percent ~ ., data = yearly.data)
summary(currency.lm)
# the linear regression model gives an Adjusted R-squared of 0.8233 and Multiple R-squared of 0.9518
null.model <- lm(dollar.percent ~ 1, data = yearly.data)
full.model.formula <- (dollar.percent ~ FDTR.change + FDTR.year + CPI.percent  + CPI.year +
                         CAD.percent + debt.percent + GDP.percent + GDP.year)


currency.lm1 <- step(null.model, full.model.formula, direction = "both")
summary(currency.lm1)
# the model shows inflation rate as the most significant factor affecting the change in dollar price.
# other variables are important but they contribute to the change in dollar price not as predictors.




#############################################QUARTERLY DATA##########################################
# converting the data to numeric

quarterly.data$dollar.quart.percent <- as.numeric(as.character(dollar.quart.percent))
quarterly.data$FDTR.quart <- as.numeric(as.character(FDTR.quart))
quarterly.data$FDTR.quart.change <- as.numeric(as.character(FDTR.quart.change))
quarterly.data$CPI.quart <- as.numeric(as.character(CPI.quart))
quarterly.data$CPI.quart.percent <- as.numeric(as.character(CPI.quart.percent))
quarterly.data$CAD.quart.percent <- as.numeric(as.character(CAD.quart.percent))
quarterly.data$debt.quart.percent <- as.numeric(as.character(debt.quart.percent))
quarterly.data$GDP.quart.percent <- as.numeric(as.character(GDP.quart.percent))
quarterly.data$GDP.quart <- as.numeric(as.character(GDP.quart))
colnames(quarterly.data)

# HISTOGRAM
# distribution of Change in Dollar Rate
skewness(quarterly.data$dollar.quart.percent)
# the skewness for Change in Dollar Rates is Negative, so the distribution is left skewed.

par(mfrow = c(1,2))

hist(quarterly.data$dollar.quart.percent, main = "Histogram for Change in Dollar Rate",
     xlab = "Change in Dollar Rate", col = "grey")
# the histogram shows the distribution is not normal. It is equally distributed but slightly 
# left skewed.
qqnorm(quarterly.data$dollar.quart.percent)
# the qqnorm does show a linear relationship but shows a positive incrementing relation.
# There are residuals present towards the right top.

# distribution of Change in Federal Rate
skewness(quarterly.data$FDTR.quart.change)
# the skewness for Change in Federal Rates is highly negativr, so the distribution is left skewed.
hist(quarterly.data$FDTR.quart.change, main = "Histogram for Change in Federal Rate",
     xlab = "Change in Federal Rate", col = "grey")
# the histogram shows the distribution is not normal. The distribution is heavy tailed and
# left skewed.
qqnorm(quarterly.data$FDTR.quart.change)
# the qqnorm does not show a linear relationship but there are constatnt values at certain level.
# There are residuals present towards the right top.

# distribution of Federal Rate
skewness(quarterly.data$FDTR.quart)
# the skewness for Federal Rates is Positive, so the distribution is right skewed.
hist(quarterly.data$FDTR.quart, main = "Histogram for Federal Rate",
     xlab = "Federal Rate", col = "grey")
# the histogram shows the distribution is not normal. The distribution is heavy tailed and
# right skewed.
qqnorm(quarterly.data$FDTR.quart)
# the qqnorm does not show a linear relationship but shows a positive incrementing relation 
# after a point. There are residuals present towards the right top.

# distribution of Percentage Change in Inflation
skewness(quarterly.data$CPI.quart.percent)
# the skewness for inflation is also negative It means the distribution is left skewed.
hist(quarterly.data$CPI.quart.percent, main = "Histogram Percentage Change in Inflation",
     xlab = "Percentage Change in Inflation", col = "grey")
# the distribution for inflation is not normally distributed. It is slightly skewed towards
# the left
qqnorm(quarterly.data$CPI.quart.percent)
# the plot shows few unusual values towards the start and end period bottom left which are residuals.

# distribution of Inflation Rate
skewness(quarterly.data$CPI.quart)
# the skewness for inflation is positive It means the distribution is right skewed.
hist(quarterly.data$CPI.quart, main = "Histogram for Inflation Rate",
     xlab = "Change in Inflation Rate", col = "grey")
# the distribution for inflation is not normally distributed. It is slightly skewed towards
# the right
qqnorm(quarterly.data$CPI.quart)
# the plot shows few unusual values towards the start and end period ie the top right and 
# bottom left which are residuals.

# distribution of Percentage Change in Current Account
skewness(quarterly.data$CAD.quart.percent)
# the skewness is negative for current account which shows it is left skewed.
hist(quarterly.data$CAD.quart.percent, main = "Histogram Change in Current Account",
     xlab = "Percentage Change in Current Account", col = "grey")
# the histogram for current account shows that there are almost equally distributed. 
# the tail is towards the right so the distribution is left skewed. 
qqnorm(quarterly.data$CAD.quart.percent)
# there are residuals or extreme or unusal values present which are the quarters where the 
# current account is in extreme surplus or deficit.

# distribution of percentage Change in Public Debt
skewness(quarterly.data$debt.quart.percent)
# the skewness for change in public debt is also negative which means it is left skewed.
hist(quarterly.data$debt.quart.percent, main = "Histogram for Change in Public Debt",
     xlab = "Percentage Change in Public Debt", col = "grey")
# the histogram shows the distribution for percentage change in debt is slightly left skewed.
qqnorm(quarterly.data$debt.quart.percent)
# it shows a plot where values are increasing in an almost linear relationship

# distribution of Percentage Change in GDP
skewness(quarterly.data$GDP.quart.percent)
# the change in GDP is right skewed as the distribution is positive
hist(quarterly.data$GDP.quart.percent, main = "Histogram for Percentage Change in GDP",
     xlab = "Percantage Change in GDP", col = "grey")
# the histogram for change in GDP is right skewed and close to normal but not perfectly distributed.
qqnorm(quarterly.data$GDP.quart.percent)
# the plot shows resuiduals like other explanatory variables.

# distribution of GDP
skewness(quarterly.data$GDP.quart)
# the GDP is left skewed as the distribution is negative
hist(quarterly.data$GDP.quart, main = "Histogram for GDP",
     xlab = "GDP Growth Rate", col = "grey")
# the histogram for change in GDP is slightly left skewed and close to normal but not 
# perfectly distributed.
qqnorm(quarterly.data$GDP.quart)
# the plot shows resuiduals like other explanatory variables. there is an increasing relationship

# plotting the matrix of all variables
# matrix plot helps to visualize relationship between all variables
plot(quarterly.data, pch = 16, col = "blue",
     main = "Matrix Scatterplot of Factors affecting exchange rate")
# the plot shows other than the variable Change in CAC and Change in Debt have an
# increasing relationship. there is some visible linearity.

dollar.quart.percent <- as.numeric(as.character(dollar.quart.percent))
FDTR.quart <- as.numeric(as.character(FDTR.quart))
FDTR.quart.change <- as.numeric(as.character(FDTR.quart.change))
CPI.quart <- as.numeric(as.character(CPI.quart))
CPI.quart.percent <- as.numeric(as.character(CPI.quart.percent))
CAD.quart.percent <- as.numeric(as.character(CAD.quart.percent))
debt.quart.percent <- as.numeric(as.character(debt.quart.percent))
GDP.quart.percent <- as.numeric(as.character(GDP.quart.percent))
GDP.quart <- as.numeric(as.character(GDP.quart))

# SCATTERPLOTS
colnames(quarterly.data)
# scatterplot each variable and dollar price

par(mfrow = c(2,4))
plot(quarterly.data$FDTR.quart.change, quarterly.data$dollar.quart.percent, xlab = "Change in Interest Rate", ylab = "Dollar Percent Change", 
     main = "Interest rate & Dollar")
plot(quarterly.data$FDTR.quart, quarterly.data$dollar.quart.percent, xlab = "Interest Rate", ylab = "Dollar Percent Change", 
     main = "Interest rate & Dollar")
plot(quarterly.data$CPI.quart.percent, quarterly.data$dollar.quart.percent, xlab = "Change in Inflation", ylab = "Dollar Percent Change", 
     main = "Inflation & Dollar")
plot(quarterly.data$CPI.quart, quarterly.data$dollar.quart.percent, xlab = "Inflation Rate", ylab = "Dollar Percent Change", 
     main = "Inflation rate & Dollar")
plot(quarterly.data$CAD.quart.percent, quarterly.data$dollar.quart.percent, xlab = "Change in CAD", ylab = "Dollar Percent Change", 
     main = "CAD & Dollar")
plot(quarterly.data$debt.quart.percent, quarterly.data$dollar.quart.percent, xlab = "Change in Public Debt", ylab = "Dollar Percent Change", 
     main = "Public Debt & Dollar")
plot(quarterly.data$GDP.quart.percent, quarterly.data$dollar.quart.percent, xlab = "Change in GDP", ylab = "Dollar Percent Change", 
     main = "GDP & Dollar")
plot(quarterly.data$GDP.quart, quarterly.data$dollar.quart.percent, xlab = "GDP Growth Rate ", ylab = "Dollar Percent Change", 
     main = "GDP & Dollar")

# plot(CAD.change[1:length(year.change)],year.change)
# plot(debt.change[1:length(year.change)],year.change)
# plot(CPI.year[1:length(year.change)],year.change)

# CORRELATION
# correlation for dollar price and other factors
# for quarterly data
corr.quart <- cor(dollar.quart.percent, quarterly.data)                            
corr.quart


# Plot a correlation graph
quarterly.cor <- cor(quarterly.data[1:9, ])
corrplot(quarterly.cor, method = "number")
# the plot shows a high correlation between Change in Inflation and Change in GDP than other 
# variables. the plot also shows there is less evidence of relation between variables 
# Change in Debt and Change in CAC
# plotting individual correlation between variables
cor(quarterly.data$FDTR.quart.change, quarterly.data$CPI.quart.percent)   # -0.1339619
cor(quarterly.data$FDTR.quart.change, quarterly.data$CPI.quart)  # -0.004220988
cor(quarterly.data$FDTR.quart.change, quarterly.data$CAD.quart.percent) #  -0.1242346
cor(quarterly.data$FDTR.quart.change, quarterly.data$debt.quart.percent)  # -0.04748893
cor(quarterly.data$FDTR.quart.change, quarterly.data$GDP.quart.percent)  # 0.03828662
cor(quarterly.data$FDTR.quart.change, quarterly.data$GDP.quart)  # 0.4176527
cor(quarterly.data$FDTR.quart, quarterly.data$CPI.quart.percent)   #  0.05315152
cor(quarterly.data$FDTR.quart, quarterly.data$CPI.quart)  # 0.5880619
cor(quarterly.data$FDTR.quart, quarterly.data$CAD.quart.percent) # -0.07890701
cor(quarterly.data$FDTR.quart, quarterly.data$debt.quart.percent)  # -0.09834771
cor(quarterly.data$FDTR.quart, quarterly.data$GDP.quart.percent)  # -0.105782
cor(quarterly.data$FDTR.quart, quarterly.data$GDP.quart)  # 0.5854542
cor(quarterly.data$CPI.quart.percent, quarterly.data$CAD.quart.percent)  # 0.02431944
cor(quarterly.data$CPI.quart.percent, quarterly.data$debt.quart.percent) # -0.03187675
cor(quarterly.data$CPI.quart.percent, quarterly.data$GDP.quart.percent)  # 0.1151523
cor(quarterly.data$CPI.quart.percent, quarterly.data$GDP.quart) # -0.03535404
cor(quarterly.data$CPI.quart, quarterly.data$CAD.quart.percent)  # -0.1965706
cor(quarterly.data$CPI.quart, quarterly.data$debt.quart.percent) # -0.006075476
cor(quarterly.data$CPI.quart, quarterly.data$GDP.quart.percent)  # 0.06190924
cor(quarterly.data$CPI.quart, quarterly.data$GDP.quart) # 0.3557915
cor(quarterly.data$CAD.quart.percent, quarterly.data$debt.quart.percent)  # 0.2171592
cor(quarterly.data$CAD.quart.percent, quarterly.data$GDP.quart.percent) # -0.06102681
cor(quarterly.data$CAD.quart.percent, quarterly.data$GDP.quart)  # -0.07718229
cor(quarterly.data$debt.quart.percent, quarterly.data$GDP.quart.percent) # -0.005081998
cor(quarterly.data$debt.quart.percent, quarterly.data$GDP.quart) # -0.3136438
# this shows there is high correlation between Change in Inflation and Change in GDP and Change in
# Federal Rate and GDP and a negative correlation bewteen Change in CAD an Change in GDP. 


# LINEAR REGRESSION MODEL
colnames(quarterly.data)
currency.lm2 <- lm(dollar.quart.percent ~ ., data = quarterly.data)
summary(currency.lm2)
# the linear regression model gives an Adjusted R-squared of 0.136 and Multiple R-squared of 0.2594
null.model1 <- lm(dollar.quart.percent ~ 1, data = quarterly.data)
full.model.formula1 <- (dollar.quart.percent ~ FDTR.quart.change + FDTR.quart + CPI.quart.percent  
                       + CPI.quart + CAD.quart.percent + debt.quart.percent + GDP.quart.percent
                       + GDP.quart)

currency.lm4 <- step(null.model1, full.model.formula1, direction = "both")
summary(currency.lm4)

# the model shows inflation rate as the most significant factor affecting the change in dollar price.
# other variables are important but they contribute to the change in dollar price not as predictors.


