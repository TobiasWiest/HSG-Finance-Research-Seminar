library(readxl)
library(tidyverse)
library(plm)
library(reshape2)
library(forecast)
library(fBasics)
library(AER)
#### Importing the monthly dataset
Data <- read_excel("Data.xlsx")
str(Data)
Data <- Data %>% 
  mutate(Date = as.Date(Data$Date, format = "%Y-%m"))

LongData <- Data %>% 
  melt(value.name = "PriceIndex", id.vars = "Date", variable.name = "ID") 

LongData %>% 
  distinct(ID) %>% 
  count()
#6133 Stocks 

B <- LongData %>% 
  group_by(Date) %>% 
  count()

# Filter1 <- LongData %>% 
#   filter(PriceIndex != "NA") %>% 
#   group_by(ID) %>% 
#   add_tally() %>% 
#   filter(n>40) %>% 
#   distinct(ID) %>% 
#   mutate(FilterDummy = 1)

# Filter1Data <- LongData %>% 
#   left_join(Filter1, by = "ID") %>% 
#   filter(FilterDummy == 1)


#4537 Stocks


PanelData <- pdata.frame(LongData, c("ID", "Date"))
PanelData$PriceIndex <- as.numeric(as.character(PanelData$PriceIndex))
PanelData$PriceIndex[PanelData$PriceIndex == 0.00000 ] <- NA
PanelData$Return <- (diff(PanelData$PriceIndex) / lag(PanelData$PriceIndex)) * 100
PanelData$Return[PanelData$Return == 0.00000 ] <- NA
PanelData <- data.frame(PanelData)

ReturnCalc <- PanelData %>% 
  select("ID", "Date", "Return", "PriceIndex") %>% 
  arrange(Return)  %>% 
  mutate(quartile = ntile(Return, 200)) 


NoReturns <- ReturnCalc[is.na(ReturnCalc$Return), ]
Returns <- ReturnCalc[!is.na(ReturnCalc$Return),]

Returns$Return[Returns$quartile == 1] <- NA
Returns$Return[Returns$quartile == 200] <- NA

ReturnCalc <- rbind(Returns, NoReturns)

Test <- ReturnCalc[!is.na(ReturnCalc$Return), ]
basicStats(Test$Return) 

AvgMonthlyReturns <- ReturnCalc %>%
  mutate(Date = as.Date(Date)) %>% 
  group_by(Date) %>% 
  mutate(EW_Month_Return = mean(Return, na.rm = TRUE)) %>% 
  distinct(date, .keep_all = TRUE) %>% 
  arrange(Date) 

AvgMonthlyReturns <- AvgMonthlyReturns[-1,]

MoodMonthIdentifier <- AvgMonthlyReturns %>% 
  mutate(month = format(Date, "%m")) %>% 
  group_by(month) %>% 
  mutate(EW_Return = mean(EW_Month_Return, na.rm = TRUE)) %>% 
  distinct(month, .keep_all = TRUE) %>% 
  arrange(EW_Return)


RecurrenceReg <- ReturnCalc %>% 
  mutate(Date = as.Date(Date)) %>%
  mutate(month = format(Date, "%m"))

September <- RecurrenceReg[RecurrenceReg$month == "09",]
January <- RecurrenceReg[RecurrenceReg$month == "01",]

RecurrenceReg  <- rbind(January, September)  %>% 
  arrange(ID, Date) %>% 
  select(ID, Date, Return, month)

#### Mood Recurrence
#### Lag1 

RecurrenceLag1 <- RecurrenceReg %>% 
  group_by(ID) %>% 
  mutate(Lag1 = c(NA, NA, head(Return, -2)))
library(lubridate)

RecurrenceLag1 <- RecurrenceLag1 %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

RecurrenceLag1 <- RecurrenceLag1[RecurrenceLag1$year != 1979,]

the.dates1 <- unique(RecurrenceLag1$Date)
a.formula1 <- Return ~ Lag1


first.step1 <-  lapply(the.dates1, function(a.date) {
  temp.data <- RecurrenceLag1[RecurrenceLag1$Date == a.date, ]
  an.lm <- lm(a.formula1, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df1 <- do.call('rbind', first.step1)
coeftest(lm(first.step.df1$Lag1 ~ 1), 
         vcov = NeweyWest(lm(first.step.df1$Lag1 ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model1 <- pmg(Return ~ Lag1, data = RecurrenceLag1, index = c("Date", "ID"))
summary(model1)

#### Lag2-5

RecurrenceLag25 <- RecurrenceReg %>% 
  group_by(ID) %>% 
  mutate(Lag2 = c(NA, NA, NA, NA, head(Return, -4))) %>% 
  mutate(Lag3 = c(NA, NA, NA, NA, NA, NA, head(Return, -6))) %>% 
  mutate(Lag4 = c(NA, NA, NA, NA, NA, NA, NA, NA,  head(Return, -8))) %>% 
  mutate(Lag5 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, head(Return, -10)))
         
RecurrenceLag25$Lag25 <- rowMeans(RecurrenceLag25[, 5:8])

RecurrenceLag25 <- RecurrenceLag25 %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

RecurrenceLag25 <- RecurrenceLag25[RecurrenceLag25$year > 1983 ,]

the.dates25 <- unique(RecurrenceLag25$Date)
a.formula25 <- Return ~ Lag25


first.step25 <-  lapply(the.dates25, function(a.date) {
  temp.data <- RecurrenceLag25[RecurrenceLag25$Date == a.date, ]
  an.lm <- lm(a.formula25, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df25 <- do.call('rbind', first.step25)
coeftest(lm(first.step.df25$Lag25 ~ 1), 
         vcov = NeweyWest(lm(first.step.df25$Lag25 ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model25 <- pmg(Return ~ Lag25, data = RecurrenceLag25, index = c("Date", "ID"))
summary(model25)

#### Lag6-10

RecurrenceLag610 <- RecurrenceReg %>% 
  group_by(ID) %>% 
  mutate(Lag6 = c(rep(NA, 12), head(Return, -12))) %>% 
  mutate(Lag7 = c(rep(NA, 14), head(Return, -14))) %>% 
  mutate(Lag8 = c(rep(NA, 16),  head(Return, -16))) %>% 
  mutate(Lag9 = c(rep(NA, 18), head(Return, -18))) %>% 
  mutate(Lag10 = c(rep(NA, 20), head(Return, -20))) 
                           

RecurrenceLag610$Lag610 <- rowMeans(RecurrenceLag610[, 5:9])

RecurrenceLag610 <- RecurrenceLag610 %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

RecurrenceLag610 <- RecurrenceLag610[RecurrenceLag610$year > 1988 ,]

the.dates610 <- unique(RecurrenceLag610$Date)
a.formula610 <- Return ~ Lag610


first.step610 <-  lapply(the.dates610, function(a.date) {
  temp.data <- RecurrenceLag610[RecurrenceLag610$Date == a.date, ]
  an.lm <- lm(a.formula610, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df610 <- do.call('rbind', first.step610)
coeftest(lm(first.step.df610$Lag610 ~ 1), 
         vcov = NeweyWest(lm(first.step.df610$Lag610 ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model610 <- pmg(Return ~ Lag610, data = RecurrenceLag610, index = c("Date", "ID"))
summary(model610)


#### Mood Reversal

ReturnRev <- RecurrenceReg %>% 
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  mutate(month = as.numeric(month)) %>% 
  arrange(ID, year, -month) %>% 
  select(Return)

ReversalReg <-  RecurrenceReg %>% 
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  arrange(ID, year, month) %>% 
  mutate(RevReturn = ReturnRev$Return) %>% 
  arrange(ID, Date)

#### Lag 1

ReversalLag1 <- ReversalReg %>% 
  mutate(RevLag1 = c(NA, NA, head(RevReturn, -2))
)                           

ReversalLag1 <- ReversalLag1 %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

ReversalLag1 <- ReversalLag1[ReversalLag1$year != 1979,]

the.dates1.rev <- unique(ReversalLag1$Date)
a.formula1.rev <- Return ~ RevLag1


first.step1.rev <-  lapply(the.dates1.rev, function(a.date) {
  temp.data <- ReversalLag1[ReversalLag1$Date == a.date, ]
  an.lm <- lm(a.formula1.rev, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df1.rev <- do.call('rbind', first.step1.rev)
coeftest(lm(first.step.df1.rev$RevLag1 ~ 1), 
         vcov = NeweyWest(lm(first.step.df1.rev$RevLag1 ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model1rev <- pmg(Return ~ RevLag1, data = ReversalLag1, index = c("Date", "ID"))
summary(model1rev)



#### Lag 2-5

ReversalLag25 <-  ReversalReg %>% 
  mutate(RevLag2 = c(NA, NA, NA, NA, head(RevReturn, -4))) %>% 
  mutate(RevLag3 = c(NA, NA, NA, NA, NA, NA, head(RevReturn, -6))) %>% 
  mutate(RevLag4 = c(rep(NA, 8), head(RevReturn, -8))) %>% 
  mutate(RevLag5 = c(rep(NA, 10), head(RevReturn, -10)))

ReversalLag25$RevLag25 <- rowMeans(ReversalLag25[, 7:10])

ReversalLag25 <- ReversalLag25 %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

ReversalLag25 <- ReversalLag25[ReversalLag25$year > 1983 ,]

the.dates25.rev <- unique(ReversalLag25$Date)
a.formula25.rev <- Return ~ RevLag25


first.step25.rev <-  lapply(the.dates25.rev, function(a.date) {
  temp.data <- ReversalLag25[ReversalLag25$Date == a.date, ]
  an.lm <- lm(a.formula25.rev, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df25.rev <- do.call('rbind', first.step25.rev)
coeftest(lm(first.step.df25.rev$RevLag25 ~ 1), 
         vcov = NeweyWest(lm(first.step.df25.rev$RevLag25 ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model25rev <- pmg(Return ~ RevLag25, data = ReversalLag25, index = c("Date", "ID"))
summary(model25rev)


#### Lag 6-10

ReversalLag610 <- ReversalReg %>% 
  mutate(RevLag6 = c(rep(NA, 12), head(RevReturn, -12))) %>% 
  mutate(RevLag7 = c(rep(NA, 14), head(RevReturn, -14))) %>% 
  mutate(RevLag8 = c(rep(NA, 16), head(RevReturn, -16))) %>% 
  mutate(RevLag9 = c(rep(NA, 18), head(RevReturn, -18))) %>% 
  mutate(RevLag10 = c(rep(NA, 20), head(RevReturn, -20)))                           


ReversalLag610$RevLag610 <- rowMeans(ReversalLag610[, 7:11])

ReversalLag610 <- ReversalLag610 %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

ReversalLag610 <- ReversalLag610[ReversalLag610$year > 1988 ,]

the.dates610.rev <- unique(ReversalLag610$Date)
a.formula610.rev <- Return ~ RevLag610


first.step610.rev <-  lapply(the.dates610.rev, function(a.date) {
  temp.data <- ReversalLag610[ReversalLag610$Date == a.date, ]
  an.lm <- lm(a.formula610.rev, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df610.rev <- do.call('rbind', first.step610.rev)
coeftest(lm(first.step.df610.rev$RevLag610 ~ 1), 
         vcov = NeweyWest(lm(first.step.df610.rev$RevLag610 ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model610rev <- pmg(Return ~ RevLag610, data = ReversalLag610, index = c("Date", "ID"))
summary(model610rev)


#### Realized High and Low Mood Months
RealizedHighMoodMonths <-  AvgMonthlyReturns  %>%
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  group_by(year) %>%
  slice(which.max(EW_Month_Return)) %>% 
  mutate(High = 1) %>% 
  ungroup()

RealizedLowMoodMonths <-  AvgMonthlyReturns  %>%
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  group_by(year) %>%
  slice(which.min(EW_Month_Return)) %>% 
  mutate(High = 0) %>% 
  ungroup()

RealizedMonths <- rbind(RealizedHighMoodMonths, RealizedLowMoodMonths) %>% 
  select(Date, High) 

RecurrenceRegRealInd <- ReturnCalc %>% 
  mutate(Date = as.Date(Date)) %>%
  mutate(month = format(Date, "%m")) %>% 
  left_join(RealizedMonths, by = "Date") %>% 
  arrange(ID, Date) %>% 
  select(ID, Date, High, Return)  

RecurrenceRegRealInd <- RecurrenceRegRealInd %>% 
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  rename(ReturnReal = Return) %>% 
  rename(DateReal = Date)
  
RecurrenceRegReal <- RecurrenceReg %>% 
  mutate(High = ifelse(month == "09", 0, 1)) %>% 
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  left_join(RecurrenceRegRealInd, by = c("ID", "year", "High"))

#### Real Recurrence

#### Lag1 


RecurrenceLag1Real <- RecurrenceRegReal %>% 
  mutate(RealLag1 = c(NA, NA, head(ReturnReal, -2)))                           


RecurrenceLag1Real <- RecurrenceLag1Real %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

RecurrenceLag1Real <- RecurrenceLag1Real[RecurrenceLag1Real$year != 1979,]

the.dates1.real <- unique(RecurrenceLag1Real$Date)
a.formula1.real <- Return ~ RealLag1


first.step1.real <-  lapply(the.dates1.real, function(a.date) {
  temp.data <- RecurrenceLag1Real[RecurrenceLag1Real$Date == a.date, ]
  an.lm <- lm(a.formula1.real, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df1.real <- do.call('rbind', first.step1.real)
coeftest(lm(first.step.df1.real$RealLag1 ~ 1), 
         vcov = NeweyWest(lm(first.step.df1.real$RealLag1 ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model1real <- pmg(Return ~ RealLag1, data = RecurrenceLag1Real, index = c("Date", "ID"))
summary(model1real)

#### Lag2-5


RecurrenceLag25Real <-  RecurrenceRegReal %>% 
  mutate(RealLag2 = c(NA, NA, NA, NA, head(ReturnReal, -4))) %>% 
  mutate(RealLag3 = c(NA, NA, NA, NA, NA, NA, head(ReturnReal, -6))) %>% 
  mutate(RealLag4 = c(NA, NA, NA, NA, NA, NA, NA, NA,  head(ReturnReal, -8))) %>% 
  mutate(RealLag5 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, head(ReturnReal, -10)))
                       

RecurrenceLag25Real$RealLag25 <- rowMeans(RecurrenceLag25Real[, 9:12])

RecurrenceLag25Real <- RecurrenceLag25Real %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

RecurrenceLag25Real <- RecurrenceLag25Real[RecurrenceLag25Real$year > 1983 ,]

the.dates25.real <- unique(RecurrenceLag25Real$Date)
a.formula25.real <- Return ~ RealLag25


first.step25.real <-  lapply(the.dates25.real, function(a.date) {
  temp.data <- RecurrenceLag25Real[RecurrenceLag25Real$Date == a.date, ]
  an.lm <- lm(a.formula25.real, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df25.real <- do.call('rbind', first.step25.real)
coeftest(lm(first.step.df25.real$RealLag25 ~ 1), 
         vcov = NeweyWest(lm(first.step.df25.real$RealLag25 ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model25real <- pmg(Return ~ RealLag25, data = RecurrenceLag25Real, index = c("Date", "ID"))
summary(model25real)

#### Lag6-10

RecurrenceLag610Real <- RecurrenceRegReal %>% 
  mutate(RealLag6 = c(rep(NA, 12), head(ReturnReal, -12))) %>% 
  mutate(RealLag7 = c(rep(NA, 14), head(ReturnReal, -14))) %>% 
  mutate(RealLag8 = c(rep(NA, 16), head(ReturnReal, -16))) %>% 
  mutate(RealLag9 = c(rep(NA, 18), head(ReturnReal, -18))) %>% 
  mutate(RealLag10 = c(rep(NA, 20),head(ReturnReal, -20)))


RecurrenceLag610Real$RealLag610 <- rowMeans(RecurrenceLag610Real[, 9:13])

RecurrenceLag610Real <- RecurrenceLag610Real %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

RecurrenceLag610Real <- RecurrenceLag610Real[RecurrenceLag610Real$year > 1988 ,]

the.dates610.real <- unique(RecurrenceLag610Real$Date)
a.formula610.real <- Return ~ RealLag610


first.step610.real <-  lapply(the.dates610.real, function(a.date) {
  temp.data <- RecurrenceLag610Real[RecurrenceLag610Real$Date == a.date, ]
  an.lm <- lm(a.formula610.real, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df610.real <- do.call('rbind', first.step610.real)
coeftest(lm(first.step.df610.real$RealLag610 ~ 1), 
         vcov = NeweyWest(lm(first.step.df610.real$RealLag610 ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model610real <- pmg(Return ~ RealLag610, data = RecurrenceLag610Real, index = c("Date", "ID"))
summary(model610real)

#### Realized Mood Reversal

ReturnRevReal <- RecurrenceRegReal %>% 
  arrange(ID, year, High) %>% 
  select(ReturnReal)

ReversalRegReal <-  RecurrenceRegReal %>% 
  arrange(ID, year, month) %>% 
  mutate(RevReturnReal = ReturnRevReal$ReturnReal) %>% 
  arrange(ID, Date)

#### Lag 1

ReversalLag1Real <- ReversalRegReal %>% 
  mutate(RevLag1Real = c(NA, NA, head(RevReturnReal, -2)))
                         

ReversalLag1Real <- ReversalLag1Real %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

ReversalLag1Real <- ReversalLag1Real[ReversalLag1Real$year != 1979,]

the.dates1.rev.real <- unique(ReversalLag1Real$Date)
a.formula1.rev.real <- Return ~ RevLag1Real


first.step1.rev.real <-  lapply(the.dates1.rev.real, function(a.date) {
  temp.data <- ReversalLag1Real[ReversalLag1Real$Date == a.date, ]
  an.lm <- lm(a.formula1.rev.real, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df1.rev.real <- do.call('rbind', first.step1.rev.real)
coeftest(lm(first.step.df1.rev.real$RevLag1Real ~ 1), 
         vcov = NeweyWest(lm(first.step.df1.rev.real$RevLag1Real ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model1revreal <- pmg(Return ~ RevLag1Real, data = ReversalLag1Real, index = c("Date", "ID"))
summary(model1revreal)


#### Lag 2-5

ReversalLag25Real <-   ReversalRegReal %>% 
  mutate(RevLag2Real = c(NA, NA, NA, NA, head(RevReturnReal, -4))) %>% 
  mutate(RevLag3Real = c(NA, NA, NA, NA, NA, NA, head(RevReturnReal, -6))) %>% 
  mutate(RevLag4Real = c(rep(NA, 8), head(RevReturnReal, -8))) %>% 
  mutate(RevLag5Real = c(rep(NA, 10), head(RevReturnReal, -10)))                           


ReversalLag25Real$RevLag25Real <- rowMeans(ReversalLag25Real[, 10:13])

ReversalLag25Real <- ReversalLag25Real %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

ReversalLag25Real <- ReversalLag25Real[ReversalLag25Real$year > 1983 ,]

the.dates25.rev.real <- unique(ReversalLag25Real$Date)
a.formula25.rev.real <- Return ~ RevLag25Real


first.step25.rev.real <-  lapply(the.dates25.rev.real, function(a.date) {
  temp.data <- ReversalLag25Real[ReversalLag25Real$Date == a.date, ]
  an.lm <- lm(a.formula25.rev.real, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df25.rev.real <- do.call('rbind', first.step25.rev.real)
coeftest(lm(first.step.df25.rev.real$RevLag25Real ~ 1), 
         vcov = NeweyWest(lm(first.step.df25.rev.real$RevLag25Real ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model25revreal <- pmg(Return ~ RevLag25Real, data = ReversalLag25Real, index = c("Date", "ID"))
summary(model25revreal)


#### Lag 6-10

ReversalLag610Real <- ReversalRegReal %>% 
  mutate(RevLag6Real = c(rep(NA, 12), head(RevReturnReal, -12))) %>% 
  mutate(RevLag7Real = c(rep(NA, 14), head(RevReturnReal, -14))) %>% 
  mutate(RevLag8Real = c(rep(NA, 16), head(RevReturnReal, -16))) %>% 
  mutate(RevLag9Real = c(rep(NA, 18), head(RevReturnReal, -18))) %>% 
  mutate(RevLag10Real = c(rep(NA, 20), head(RevReturnReal, -20)))                           


ReversalLag610Real$RevLag610Real <- rowMeans(ReversalLag610Real[, 10:14])

ReversalLag610Real <- ReversalLag610Real %>%
  mutate(year = as.numeric(format(Date, "%Y"))) 

ReversalLag610Real <- ReversalLag610Real[ReversalLag610Real$year > 1988 ,]

the.dates610.rev.real <- unique(ReversalLag610Real$Date)
a.formula610.rev.real <- Return ~ RevLag610Real


first.step610.rev.real <-  lapply(the.dates610.rev.real, function(a.date) {
  temp.data <- ReversalLag610Real[ReversalLag610Real$Date == a.date, ]
  an.lm <- lm(a.formula610.rev.real, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df610.rev.real <- do.call('rbind', first.step610.rev.real)
coeftest(lm(first.step.df610.rev.real$RevLag610Real ~ 1), 
         vcov = NeweyWest(lm(first.step.df610.rev.real$RevLag610Real ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

model610revreal <- pmg(Return ~ RevLag610Real, data = ReversalLag610Real, index = c("Date", "ID"))
summary(model610revreal)

#### Importing the daily dataset

DailyData <- read.csv("DailyData.csv", header = T, sep = ";")
str(DailyData)
DailyData <- DailyData %>% 
  mutate(Date = as.Date(DailyData$Date, format = "%d.%m.%Y"))
gc()
LongDailyData <- DailyData %>% 
  melt(value.name = "PriceIndex", id.vars = "Date", variable.name = "ID") 

remove(DailyData)
gc()
LongDailyData %>% 
  distinct(ID) %>% 
  count()

PanelDailyData <- LongDailyData %>% 
  group_by(ID)  %>% 
    mutate(PriceIndex = as.numeric(as.character(PriceIndex))) 

remove(LongDailyData)
gc()

PanelDailyData$PriceIndex[PanelDailyData$PriceIndex == 0.00000 ] <- NA

PanelDailyData <- PanelDailyData %>% 
  mutate(PriceIndexLag = c(NA, head(PriceIndex, -1))) %>% 
  mutate(Return = ((PriceIndex-PriceIndexLag)/PriceIndexLag)*100)


PanelDailyData$Return[PanelDailyData$Return == 0.00000 ] <- NA


ReturnCalcDaily <- PanelDailyData %>% 
  select("ID", "Date", "Return", "PriceIndex")

remove(PanelDailyData)
gc()
NoReturns <- ReturnCalcDaily[is.na(ReturnCalcDaily$Return), ]
Returns <- ReturnCalcDaily[!is.na(ReturnCalcDaily$Return),]

Returns$quartile <- ntile(Returns$Return, 200)
Returns$Return[Returns$quartile == 1] <- NA
Returns$Return[Returns$quartile == 200] <- NA


remove(ReturnCalcDaily)

Returns <- Returns[, -5]
ReturnCalcDaily <- rbind(Returns, NoReturns)
remove(NoReturns)
remove(Returns)

basicStats(ReturnCalcDaily$Return)

ReturnCalcDaily <- readRDS(file = "ReturnCalcDaily.Rds")

AvgWeekdayReturns <- ReturnCalcDaily %>%
  mutate(Date = as.Date(Date)) %>% 
  group_by(Date) %>% 
  mutate(EW_Weekday_Return = mean(Return, na.rm = TRUE)) %>% 
  distinct(Date, .keep_all = TRUE) %>% 
  arrange(Date) 


AvgWeekdayReturns <- AvgWeekdayReturns[-1,]

library(lubridate)

MoodWeekdayIdentifier <- AvgWeekdayReturns %>% 
  mutate(weekday = weekdays(Date)) %>% 
  group_by(weekday) %>% 
  mutate(EW_Return = mean(EW_Weekday_Return, na.rm = TRUE)) %>% 
  distinct(weekday, .keep_all = TRUE) %>% 
  arrange(EW_Return)
ReturnCalcDaily <- readRDS(file = "ReturnCalcDaily.Rds")
RecurrenceRegDaily <- ReturnCalcDaily %>% 
  mutate(weekday = weekdays(Date))

saveRDS(ReturnCalcDaily, file = "ReturnCalcDaily.Rds")
remove(ReturnCalcDaily)
gc()

Monday <- RecurrenceRegDaily[RecurrenceRegDaily$weekday == "Montag",]
Friday <- RecurrenceRegDaily[RecurrenceRegDaily$weekday == "Freitag",]
remove(RecurrenceRegDaily)
RecurrenceRegDaily  <- rbind(Monday, Friday)  %>% 
  arrange(ID, Date) %>% 
  select(ID, Date, Return, weekday)


remove(Monday, Friday)
saveRDS(RecurrenceRegDaily, file = "RecurrenceRegDaily.Rds")
gc()

###Lag 1
RecurrenceDailyLag1 <- RecurrenceRegDaily %>% 
  group_by(ID) %>% 
  mutate(Lag1 = c(NA, NA, head(Return, -2)))
remove(RecurrenceRegDaily)

RecurrenceDailyLag1 <- RecurrenceDailyLag1[RecurrenceDailyLag1$Date > as.Date("1979/01/05", format = "%Y/%m/%d"),]

the.dates1 <- unique(RecurrenceDailyLag1$Date)
a.formula1 <- Return ~ Lag1


first.step1 <-  lapply(the.dates1, function(a.date) {
  temp.data <- RecurrenceDailyLag1[RecurrenceDailyLag1$Date == a.date, ]
  an.lm <- lm(a.formula1, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df1 <- do.call('rbind', first.step1)
dailyrec1 <- coeftest(lm(first.step.df1$Lag1 ~ 1), 
         vcov = NeweyWest(lm(first.step.df1$Lag1 ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))
remove(first.step1)
remove(RecurrenceDailyLag1)
saveRDS(dailyrec1, file = "dailyrec1.Rds")


####Lag 2-10
RecurrenceRegDaily <- readRDS(file = "RecurrenceRegDaily.Rds")
RecurrenceDailyLag210 <- RecurrenceRegDaily %>% 
  group_by(ID) %>% 
  mutate(Lag2 = c(rep(NA, 4), head(Return, -4))) %>% 
  mutate(Lag3 = c(rep(NA, 6), head(Return, -6))) %>% 
  mutate(Lag4 = c(rep(NA, 8),  head(Return, -8))) %>% 
  mutate(Lag5 = c(rep(NA, 10), head(Return, -10))) %>% 
  mutate(Lag6 = c(rep(NA, 12), head(Return, -12))) %>% 
  mutate(Lag7 = c(rep(NA, 14), head(Return, -14))) %>% 
  mutate(Lag8 = c(rep(NA, 16),  head(Return, -16))) %>% 
  mutate(Lag9 = c(rep(NA, 18), head(Return, -18))) %>% 
  mutate(Lag10 = c(rep(NA, 20), head(Return, -20))) 
remove(RecurrenceRegDaily)
RecurrenceDailyLag210$Lag210 <- rowMeans(RecurrenceDailyLag210[, 5:13])

RecurrenceDailyLag210 <- RecurrenceDailyLag210[RecurrenceDailyLag210$Date > as.Date("1979/03/09", format = "%Y/%m/%d"),]

the.dates210 <- unique(RecurrenceDailyLag210$Date)
a.formula210 <- Return ~ Lag210


first.step210 <-  lapply(the.dates210, function(a.date) {
  temp.data <- RecurrenceDailyLag210[RecurrenceDailyLag210$Date == a.date, ]
  an.lm <- lm(a.formula210, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df210 <- do.call('rbind', first.step210)
dailyrec210 <- coeftest(lm(first.step.df210$Lag210 ~ 1), 
         vcov = NeweyWest(lm(first.step.df210$Lag210 ~ 1), 
                          lag = 12, prewhite = FALSE,
                          adjust = TRUE))

remove(first.step210)
remove(first.step.df210)
remove(RecurrenceDailyLag210)
saveRDS(dailyrec210, file = "dailyrec210.Rds")

####Lag 11-20
RecurrenceRegDaily <- readRDS(file = "RecurrenceRegDaily.Rds")
RecurrenceDailyLag1120 <- RecurrenceRegDaily %>% 
  group_by(ID) %>% 
  mutate(Lag11 = c(rep(NA, 22), head(Return, -22))) %>% 
  mutate(Lag12 = c(rep(NA, 24), head(Return, -24))) %>% 
  mutate(Lag13 = c(rep(NA, 26),  head(Return, -26))) %>% 
  mutate(Lag14 = c(rep(NA, 28), head(Return, -28))) %>% 
  mutate(Lag15 = c(rep(NA, 30), head(Return, -30))) %>% 
  mutate(Lag16 = c(rep(NA, 32), head(Return, -32))) %>% 
  mutate(Lag17 = c(rep(NA, 34),  head(Return, -34))) %>% 
  mutate(Lag18 = c(rep(NA, 36), head(Return, -36))) %>% 
  mutate(Lag19 = c(rep(NA, 38), head(Return, -38))) %>% 
  mutate(Lag20 = c(rep(NA, 40), head(Return, -40)))

remove(RecurrenceRegDaily)
RecurrenceDailyLag1120$Lag1120 <- rowMeans(RecurrenceDailyLag1120[, 5:14])

RecurrenceDailyLag1120 <- RecurrenceDailyLag1120[RecurrenceDailyLag1120$Date > as.Date("1979/05/18", format = "%Y/%m/%d"),]

the.dates1120 <- unique(RecurrenceDailyLag1120$Date)
a.formula1120<- Return ~ Lag1120


first.step1120 <-  lapply(the.dates1120, function(a.date) {
  temp.data <- RecurrenceDailyLag1120[RecurrenceDailyLag1120$Date == a.date, ]
  an.lm <- lm(a.formula1120, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df1120 <- do.call('rbind', first.step1120)
dailyrec1120 <- coeftest(lm(first.step.df1120$Lag1120 ~ 1), 
         vcov = NeweyWest(lm(first.step.df1120$Lag1120 ~ 1), 
                          lag = 12, prewhite = FALSE,
                          adjust = TRUE))

remove(first.step1120)
remove(first.step.df1120)
remove(RecurrenceDailyLag1120)
saveRDS(dailyrec1120, file = "dailyrec1120.Rds")

###Reversal 

ReversalRegDaily <- RecurrenceRegDaily %>% 
  group_by(ID) %>% 
  mutate(week = row_number()) %>% 
  mutate(week = round((week+0.1)/2))

ReversalRegDaily <- ReversalRegDaily %>% 
  group_by(ID, week) %>% 
  mutate(RevReturn  = rev(Return))

remove(RecurrenceRegDaily)
gc()
saveRDS(ReversalRegDaily, file = "ReversalRegDaily.Rds")
###Lag1
ReversalRegDaily <- readRDS(file = "ReversalRegDaily.Rds")
ReversalDailyLag1 <- ReversalRegDaily %>% 
  group_by(ID) %>% 
  mutate(Lag1 = c(NA, NA, head(RevReturn, -2)))

ReversalDailyLag1 <- ReversalDailyLag1[ReversalDailyLag1$Date > as.Date("1979/01/05", format = "%Y/%m/%d"),]

the.dates1 <- unique(ReversalDailyLag1$Date)
a.formula1 <- Return ~ Lag1


first.step1.rev <-  lapply(the.dates1, function(a.date) {
  temp.data <- ReversalDailyLag1[ReversalDailyLag1$Date == a.date, ]
  an.lm <- lm(a.formula1, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df1.rev <- do.call('rbind', first.step1.rev)
dailyrev1 <- coeftest(lm(first.step.df1.rev$Lag1 ~ 1), 
                      vcov = NeweyWest(lm(first.step.df1.rev$Lag1 ~ 1), 
                                       lag = 12, prewhite = FALSE,
                                       adjust = TRUE))
remove(first.step1.rev)
remove(first.step.df1.rev)
remove(ReversalDailyLag1)
saveRDS(dailyrev1, file = "dailyrev1.Rds")

####Lag 2-10
ReversalRegDaily <- readRDS(file = "ReversalRegDaily.Rds")
ReversalDailyLag210 <- ReversalRegDaily %>% 
  group_by(ID) %>% 
  mutate(Lag2 = c(rep(NA, 4), head(RevReturn, -4))) %>% 
  mutate(Lag3 = c(rep(NA, 6), head(RevReturn, -6))) %>% 
  mutate(Lag4 = c(rep(NA, 8),  head(RevReturn, -8))) %>% 
  mutate(Lag5 = c(rep(NA, 10), head(RevReturn, -10))) %>% 
  mutate(Lag6 = c(rep(NA, 12), head(RevReturn, -12))) %>% 
  mutate(Lag7 = c(rep(NA, 14), head(RevReturn, -14))) %>% 
  mutate(Lag8 = c(rep(NA, 16),  head(RevReturn, -16))) %>% 
  mutate(Lag9 = c(rep(NA, 18), head(RevReturn, -18))) %>% 
  mutate(Lag10 = c(rep(NA, 20), head(RevReturn, -20))) 

remove(ReversalRegDaily)
gc()
ReversalDailyLag210$Lag210 <- rowMeans(ReversalDailyLag210[, 8:16])


ReversalDailyLag210 <- ReversalDailyLag210[ReversalDailyLag210$Date > as.Date("1979/03/09", format = "%Y/%m/%d"),]

the.dates210 <- unique(ReversalDailyLag210$Date)
a.formula210 <- Return ~ Lag210


first.step210.rev <-  lapply(the.dates210, function(a.date) {
  temp.data <- ReversalDailyLag210[ReversalDailyLag210$Date == a.date, ]
  an.lm <- lm(a.formula210, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

   first.step.df210.rev <- do.call('rbind', first.step210.rev)
dailyrev210 <- coeftest(lm(first.step.df210.rev$Lag210 ~ 1), 
                        vcov = NeweyWest(lm(first.step.df210.rev$Lag210 ~ 1), 
                                         lag = 12, prewhite = FALSE,
                                         adjust = TRUE))

remove(first.step210.rev)
remove(first.step.df210.rev)
remove(ReversalDailyLag210)
saveRDS(dailyrev210, file = "dailyrev210.Rds")
?saveRds
####Lag 11-20
ReversalRegDaily <- readRDS(file = "ReversalRegDaily.Rds")
ReversalDailyLag1120 <- ReversalRegDaily %>% 
  group_by(ID) %>% 
  mutate(Lag11 = c(rep(NA, 22), head(RevReturn, -22))) %>% 
  mutate(Lag12 = c(rep(NA, 24), head(RevReturn, -24))) %>% 
  mutate(Lag13 = c(rep(NA, 26),  head(RevReturn, -26))) %>% 
  mutate(Lag14 = c(rep(NA, 28), head(RevReturn, -28))) %>% 
  mutate(Lag15 = c(rep(NA, 30), head(RevReturn, -30))) %>% 
  mutate(Lag16 = c(rep(NA, 32), head(RevReturn, -32))) %>% 
  mutate(Lag17 = c(rep(NA, 34),  head(RevReturn, -34))) %>% 
  mutate(Lag18 = c(rep(NA, 36), head(RevReturn, -36))) %>% 
  mutate(Lag19 = c(rep(NA, 38), head(RevReturn, -38))) %>% 
  mutate(Lag20 = c(rep(NA, 40), head(RevReturn, -40)))

remove(ReversalRegDaily)

ReversalDailyLag1120$Lag1120 <- rowMeans(ReversalDailyLag1120[, 8:17])

ReversalDailyLag1120 <- ReversalDailyLag1120[ReversalDailyLag1120$Date > as.Date("1979/05/18", format = "%Y/%m/%d"),]

the.dates1120 <- unique(ReversalDailyLag1120$Date)
a.formula1120<- Return ~ Lag1120


first.step1120.rev <-  lapply(the.dates1120, function(a.date) {
  temp.data <- ReversalDailyLag1120[ReversalDailyLag1120$Date == a.date, ]
  an.lm <- lm(a.formula1120, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

  first.step.df1120.rev <- do.call('rbind', first.step1120.rev)
dailyrev1120 <- coeftest(lm(first.step.df1120.rev$Lag1120 ~ 1), 
                         vcov = NeweyWest(lm(first.step.df1120.rev$Lag1120 ~ 1), 
                                          lag = 12, prewhite = FALSE,
                                          adjust = TRUE))
remove(first.step1120.rev)
remove(first.step.df1120.rev)
remove(ReversalDailyLag1120)
saveRDS(dailyrev1120, file = "dailyrev1120.Rds")

dailyrev1
dailyrev210
dailyrev1120

####Realized Daily

#### Realized High and Low Mood Days

RealizedHighMoodWeekday <-  AvgWeekdayReturns  %>%
  ungroup() %>% 
  mutate(week = row_number()) %>% 
  mutate(week = ceiling(week/5)) %>% 
  group_by(week) %>%
  slice(which.max(EW_Weekday_Return)) %>% 
  mutate(High = 1) %>% 
  ungroup()

RealizedLowMoodWeekday <-  AvgWeekdayReturns  %>%
  ungroup() %>% 
  mutate(week = row_number()) %>% 
  mutate(week = ceiling(week/5)) %>% 
  group_by(week) %>%
  slice(which.min(EW_Weekday_Return)) %>% 
  mutate(High = 0) %>% 
  ungroup()

RealizedWeekdays <- rbind(RealizedHighMoodWeekday, RealizedLowMoodWeekday) %>% 
  select(Date, High, week) 

remove(AvgWeekdayReturns)
remove(RealizedHighMoodWeekday)
remove(RealizedLowMoodWeekday)
gc()


ReturnCalcDaily <- ReturnCalcDaily %>% 
  ungroup()

ReturnCalcDaily <- ReturnCalcDaily %>% 
  arrange(ID, Date)

ReturnCalcDaily  <- subset(ReturnCalcDaily, Date > as.Date("1978/12/31", format = "%Y/%m/%d"))
RecurrenceRegDailyRealInd <- ReturnCalcDaily %>% 
  left_join(RealizedWeekdays, by = "Date") 

remove(ReturnCalcDaily)



RecurrenceRegDailyRealInd <- RecurrenceRegDailyRealInd %>% 
  select(ID, Date, High, Return, week )  %>% 
  rename(ReturnReal = Return) %>% 
  rename(DateReal = Date) 

RecurrenceRegDaily <- readRDS(file = "RecurrenceRegDaily.Rds")

RecurrenceRegDailyReal <- RecurrenceRegDaily %>% 
  mutate(High = ifelse(weekday == "Freitag", 1, 0)) %>% 
  mutate(week = row_number()) %>% 
  mutate(week = ceiling(week/2))

RecurrenceRegDailyReal <- RecurrenceRegDailyReal  %>% 
  left_join(RecurrenceRegDailyRealInd, by = c("ID", "week", "High"))

saveRDS(RecurrenceRegDailyReal, file = "RecurrenceRegDailyReal.Rds")
remove(RecurrenceRegDaily)
remove(RecurrenceRegDailyRealInd)
####Recurrence
####Lag 1

###Lag 1
RecurrenceDailyRealLag1 <- RecurrenceRegDailyReal %>% 
  group_by(ID) %>% 
  mutate(Lag1 = c(NA, NA, head(ReturnReal, -2)))
remove(RecurrenceRegDailyReal)

RecurrenceDailyRealLag1 <- RecurrenceDailyRealLag1[RecurrenceDailyRealLag1$Date > as.Date("1979/01/05", format = "%Y/%m/%d"),]

the.dates1 <- unique(RecurrenceDailyRealLag1$Date)
a.formula1 <- Return ~ Lag1


first.step1 <-  lapply(the.dates1, function(a.date) {
  temp.data <- RecurrenceDailyRealLag1[RecurrenceDailyRealLag1$Date == a.date, ]
  an.lm <- lm(a.formula1, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df1 <- do.call('rbind', first.step1)
dailyrec1real <- coeftest(lm(first.step.df1$Lag1 ~ 1), 
                      vcov = NeweyWest(lm(first.step.df1$Lag1 ~ 1), 
                                       lag = 12, prewhite = FALSE,
                                       adjust = TRUE))
remove(first.step1)
remove(first.step.df1)
remove(RecurrenceDailyRealLag1)
saveRDS(dailyrec1real, file = "dailyrec1real.Rds")


####Lag 2-10
RecurrenceRegDailyReal <- readRDS(file = "RecurrenceRegDailyReal.Rds")
RecurrenceDailyRealLag210 <- RecurrenceRegDailyReal %>% 
  group_by(ID) %>% 
  mutate(Lag2 = c(rep(NA, 4), head(ReturnReal, -4))) %>% 
  mutate(Lag3 = c(rep(NA, 6), head(ReturnReal, -6))) %>% 
  mutate(Lag4 = c(rep(NA, 8),  head(ReturnReal, -8))) %>% 
  mutate(Lag5 = c(rep(NA, 10), head(ReturnReal, -10))) %>% 
  mutate(Lag6 = c(rep(NA, 12), head(ReturnReal, -12))) %>% 
  mutate(Lag7 = c(rep(NA, 14), head(ReturnReal, -14))) %>% 
  mutate(Lag8 = c(rep(NA, 16),  head(ReturnReal, -16))) %>% 
  mutate(Lag9 = c(rep(NA, 18), head(ReturnReal, -18))) %>% 
  mutate(Lag10 = c(rep(NA, 20), head(ReturnReal, -20))) 
remove(RecurrenceRegDailyReal)

RecurrenceDailyRealLag210$Lag210 <- rowMeans(RecurrenceDailyRealLag210[, 9:17])
RecurrenceDailyRealLag210 <- RecurrenceDailyRealLag210[RecurrenceDailyRealLag210$Date > as.Date("1979/03/09", format = "%Y/%m/%d"),]

the.dates210 <- unique(RecurrenceDailyRealLag210$Date)
a.formula210 <- Return ~ Lag210


first.step210 <-  lapply(the.dates210, function(a.date) {
  temp.data <- RecurrenceDailyRealLag210[RecurrenceDailyRealLag210$Date == a.date, ]
  an.lm <- lm(a.formula210, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df210 <- do.call('rbind', first.step210)
dailyrec210real <- coeftest(lm(first.step.df210$Lag210 ~ 1), 
                        vcov = NeweyWest(lm(first.step.df210$Lag210 ~ 1), 
                                         lag = 12, prewhite = FALSE,
                                         adjust = TRUE))

remove(first.step210)
remove(first.step.df210)
remove(RecurrenceDailyRealLag210)
saveRDS(dailyrec210real, file = "dailyrec210real.Rds")

####Lag 11-20
RecurrenceRegDailyReal <- readRDS(file = "RecurrenceRegDailyReal.Rds")
RecurrenceDailyRealLag1120 <- RecurrenceRegDailyReal %>% 
  group_by(ID) %>% 
  mutate(Lag11 = c(rep(NA, 22), head(ReturnReal, -22))) %>% 
  mutate(Lag12 = c(rep(NA, 24), head(ReturnReal, -24))) %>% 
  mutate(Lag13 = c(rep(NA, 26),  head(ReturnReal, -26))) %>% 
  mutate(Lag14 = c(rep(NA, 28), head(ReturnReal, -28))) %>% 
  mutate(Lag15 = c(rep(NA, 30), head(ReturnReal, -30))) %>% 
  mutate(Lag16 = c(rep(NA, 32), head(ReturnReal, -32))) %>% 
  mutate(Lag17 = c(rep(NA, 34),  head(ReturnReal, -34))) %>% 
  mutate(Lag18 = c(rep(NA, 36), head(ReturnReal, -36))) %>% 
  mutate(Lag19 = c(rep(NA, 38), head(ReturnReal, -38))) %>% 
  mutate(Lag20 = c(rep(NA, 40), head(ReturnReal, -40)))

remove(RecurrenceRegDailyReal)
RecurrenceDailyRealLag1120$Lag1120 <- rowMeans(RecurrenceDailyRealLag1120[, 9:18])

RecurrenceDailyRealLag1120 <- RecurrenceDailyRealLag1120[RecurrenceDailyRealLag1120$Date > as.Date("1979/05/18", format = "%Y/%m/%d"),]

the.dates1120 <- unique(RecurrenceDailyRealLag1120$Date)
a.formula1120<- Return ~ Lag1120


first.step1120 <-  lapply(the.dates1120, function(a.date) {
  temp.data <- RecurrenceDailyRealLag1120[RecurrenceDailyRealLag1120$Date == a.date, ]
  an.lm <- lm(a.formula1120, data = temp.data)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
}) 

first.step.df1120 <- do.call('rbind', first.step1120)
dailyrec1120 <- coeftest(lm(first.step.df1120$Lag1120 ~ 1), 
                         vcov = NeweyWest(lm(first.step.df1120$Lag1120 ~ 1), 
                                          lag = 12, prewhite = FALSE,
                                          adjust = TRUE))

remove(first.step1120)
remove(first.step.df1120)
remove(RecurrenceDailyRealLag1120)
saveRDS(dailyrec1120, file = "dailyrec1120.Rds")
