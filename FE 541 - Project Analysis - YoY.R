# Load the data

dollar <- read.csv("Dollar Price Movement.csv")
dollar1 <- read.csv("Dollar Price Movement v3.csv")

debt <- read.csv("Public Data.csv")
CAD <- read.csv("Current Account Balance.csv")
CPI <- read.csv("CPI.csv")
FDTR <- read.csv("FDTR.csv")
GDP <- read.csv("GDP Performance.csv")


# Writing Functions to convert quarterly data to yearly data

convert.year <- function(dataframe,date,price){
  dataframe <- dataframe[-(1:6),]
  dataframe[,price] <- as.numeric(paste(dataframe[,price]))
  dataframe$date1 <- as.POSIXlt(as.character(dataframe[,date]),format = "%d-%m-%Y")
  year.price <- as.numeric(dataframe[which(dataframe$date1$mon == 2),price])
  dates <- dataframe$date1[which(dataframe$date1$mon == 2)]
  names(year.price) <- dates[1:length(dates)-1]
  return(year.price)
}

convert.year.change <- function(dataframe,date,price){
  dataframe <- dataframe[-(1:6),]
  dataframe[,price] <- as.numeric(paste(dataframe[,price]))
  dataframe$date1 <- as.POSIXlt(as.character(dataframe[,date]),format = "%d-%m-%Y")
  year.price <- as.numeric(dataframe[which(dataframe$date1$mon == 2),price])
  year.change <- year.price[1:length(year.price)-1] - year.price[2:length(year.price)]
  dates <- dataframe$date1[which(dataframe$date1$mon == 2)]
  names(year.change) <- dates[1:length(dates)-1]
  return(year.change)
}

convert.year.percent <- function(dataframe,date,price){
  dataframe <- dataframe[-(1:6),]
  dataframe[,price] <- as.numeric(paste(dataframe[,price]))
  dataframe$date1 <- as.POSIXlt(as.character(dataframe[,date]),format = "%d-%m-%Y")
  year.price <- as.numeric(dataframe[which(dataframe$date1$mon == 2),price])
  year.percent <- (year.price[1:length(year.price)-1] - year.price[2:length(year.price)])/abs(year.price[2:length(year.price)])
  dates <- dataframe$date1[which(dataframe$date1$mon == 2)]
  names(year.percent) <- dates[1:length(dates)-1]
  return(year.percent)
}


# Converting the data

dollar.change <- convert.year.change(dollar,1,2)
dollar.percent <- convert.year.percent(dollar,1,2)

dollar1.change <- convert.year.change(dollar1,1,2)
dollar1.percent <- convert.year.percent(dollar1,1,2)

length(dollar1.percent)

debt.year <- convert.year(debt,1,2)
debt.year <- debt.year[-(length(debt.year))]
debt.change <- convert.year.change(debt,1,2)
debt.percent <- convert.year.percent(debt,1,2)

CAD.year <- convert.year(CAD,1,2)
CAD.year <- CAD.year[-(length(CAD.year))]
CAD.change <- convert.year.change(CAD,1,2)
CAD.percent <- convert.year.percent(CAD,1,2)

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


currency.data <- data.frame(cbind(dollar1.percent,debt.year,debt.change,debt.percent,
                                  CAD.year,CAD.change,CAD.percent,GDP.year,GDP.change,
                                  GDP.percent,CPI.year,CPI.change,CPI.percent,FDTR.year,
                                  FDTR.change))

nrow(currency.data)
cor(dollar1.percent,currency.data)                            


plot(GDP.percent,dollar.change)
plot(FDTR.year,dollar.change)
plot(CAD.percent,dollar.change)
plot(debt.percent,dollar.change)
plot(CPI.year,dollar.change)

hist(FDTR.change)



plot(CAD.change[1:length(year.change)],year.change)
plot(debt.change[1:length(year.change)],year.change)
plot(CPI.year[1:length(year.change)],year.change)

currency.lm <- lm(dollar.change ~ ., data = currency.data)

summary(currency.lm)

null.model <- lm(dollar.change ~ 1, data = currency.data)
full.model.formula <- dollar.change ~ CAD.change + GDP.percent + CPI.year  + FDTR.year

currency.lm1 <- step(null.model, full.model.formula, direction = "both")


summary(currency.lm1)

