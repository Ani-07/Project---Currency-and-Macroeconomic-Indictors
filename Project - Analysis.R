

proj.data <- read.csv("Stats Project Data (Extract 2).csv", stringsAsFactors = F)

# For our project, the change in dollar price is the response variable and the 
# other variables change in federal rate, inflation rate, current account,public debt and 
# GDP are the explanatory variables.

# Part (a): Using numerical and graphical summaries, describe the distribution of each 
# explanatory variable. Are there any unsusal observations that should be monitored?
# distribution of Change in Federal Rate

library(moments)

# Let us first ensure that all the data is in numeric form

summary(proj.data)

names(proj.data)

proj.data$Change.in.Federal.Rate <- as.numeric(proj.data$Change.in.Federal.Rate)


proj.data$FDTR <- ifelse(proj.data$Change.in.Federal.Rate == 0, 0,
                         ifelse(proj.data$Change.in.Federal.Rate == 0.25,1,
                                ifelse(proj.data$Change.in.Federal.Rate == 0.50, 2,
                                       ifelse(proj.data$Change.in.Federal.Rate > 0.50, 3,
                                              ifelse(proj.data$Change.in.Federal.Rate == -0.25,-1,
                                                     ifelse(proj.data$Change.in.Federal.Rate == -0.50,-2,
                                                            ifelse(proj.data$Change.in.Federal.Rate < -0.50,-3,4)))))))

summary(is.na(proj.data))


hist(proj.data$Change.in.Dollar.Price)
hist(proj.data$Federal.Rate)
proj.data$Change.in.Federal.Rate <- as.factor(proj.data$Change.in.Federal.Rate)
proj.data$FDTR <- as.factor(proj.data$FDTR)
plot(proj.data$Change.in.Federal.Rate)
plot(proj.data$FDTR)

plot(proj.data$`% Change in Inflation`, xlim = c(-50,50))

hist(proj.data$X..Change.in.CAD)
hist(proj.data$X..Change.in.Debt)
hist(proj.data$GDP.Performance)
hist(proj.data$Change.in.GDP)
hist(proj.data$X..Change.1)



# Now let us plot the explanatory variables with response variable

plot(proj.data$Change.in.Dollar.Price,proj.data$Federal.Rate)
plot(proj.data$Change.in.Dollar.Price,proj.data$Inflation)
plot(proj.data$Change.in.Dollar.Price,proj.data$`% Change in Inflation`)
plot(proj.data$Change.in.Dollar.Price,proj.data$X..Change.in.CAD)
plot(proj.data$Change.in.Dollar.Price,proj.data$X..Change.in.Debt)
plot(proj.data$Change.in.Dollar.Price,proj.data$GDP.Performance)
plot(proj.data$Change.in.Dollar.Price,proj.data$Change.in.GDP)
plot(proj.data$Change.in.Dollar.Price,proj.data$X..Change.1)

cor(proj.data)

names(proj.data)

currency.lm <- lm(Change.in.Dollar.Price ~., data = proj.data[,2:12])

summary(currency.lm)

cor(proj.data$Change.in.Dollar.Price,proj.data$Federal.Rate)

cor(proj.data$Change.in.Dollar.Price,proj.data$Inflation)

cor(proj.data$Change.in.Dollar.Price,proj.data$Change.in.CAD)

cor(proj.data$Change.in.Dollar.Price,proj.data$Change.in.Debt)
cor(proj.data$Change.in.Dollar.Price,proj.data$GDP.Performance)
cor(proj.data$Change.in.Dollar.Price,proj.data$Change.in.GDP)
cor(proj.data$Change.in.Dollar.Price,proj.data$Percent.Change.in.GDP)

# Stepwise

names(proj.data)

null.model <- lm(Change.in.Dollar.Price ~ 1, data = proj.data)
full.model.formula <- Change.in.Dollar.Price ~ Federal.Rate + Change.in.Federal.Rate + 
  Inflation + Change.in.INF + Change.in.CAD + Change.in.Debt + GDP.Performance + 
  Change.in.GDP + Percent.Change.in.GDP

currency.lm1 <- step(null.model, full.model.formula, direction = "both")

summary(currency.lm1)



