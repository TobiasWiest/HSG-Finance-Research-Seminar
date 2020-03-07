#### Importing the dataset

library(readxl)
library(tidyverse)
library(plm)
library(reshape2)
library(forecast)
library(fBasics)


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

ReturnCalc <- ReturnCalc %>%
  arrange(ID, Date) %>%
  mutate(Date = as.Date(Date))

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


#### Realized High and Low Mood Months
RealizedHighMoodMonths <-  AvgMonthlyReturns  %>%
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  group_by(year) %>%
  slice(which.max(EW_Month_Return)) %>% 
  mutate(HistRank = 1) %>% 
  ungroup()

RealizedHighMoodMonths2 <-  AvgMonthlyReturns  %>%
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  group_by(year) %>%
  slice(which(EW_Month_Return == max( EW_Month_Return[EW_Month_Return!=max(EW_Month_Return)] ))) %>% 
  mutate(HistRank = 2) %>% 
  ungroup()


RealizedLowMoodMonths <-  AvgMonthlyReturns  %>%
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  group_by(year) %>%
  slice(which.min(EW_Month_Return)) %>% 
  mutate(HistRank = 12) %>% 
  ungroup()

RealizedLowMoodMonths2 <-  AvgMonthlyReturns  %>%
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  group_by(year) %>%
  slice(which(EW_Month_Return == min( EW_Month_Return[EW_Month_Return!=min(EW_Month_Return)] ))) %>% 
  mutate(HistRank = 11) %>% 
  ungroup()

RealizedMonths <- rbind(RealizedHighMoodMonths, RealizedHighMoodMonths2, RealizedLowMoodMonths, RealizedLowMoodMonths2) %>% 
  select(Date, HistRank) 

MoodBetaRealInd <- ReturnCalc %>% 
  mutate(Date = as.Date(Date)) %>%
  mutate(month = format(Date, "%m")) %>% 
  left_join(RealizedMonths, by = "Date") %>% 
  arrange(ID, Date) %>% 
  select(ID, Date, HistRank, Return) 

MoodBetaRealInd <- MoodBetaRealInd %>% 
  mutate(year = as.numeric(format(Date, "%Y"))) 
  

MoodBetaRealInd <- MoodBetaRealInd %>% 
  mutate(Date = as.Date(Date)) %>%
  mutate(month = format(Date, "%m")) 

RecurrenceReg$HistRank <- c(rep("x", 490560))
RecurrenceReg <- RecurrenceReg %>% 
  mutate(year = as.numeric(format(Date, "%Y"))) 

MoodBeta <- rbind(RecurrenceReg, MoodBetaRealInd)
MoodBeta <- arrange(MoodBeta, ID, Date)

MoodBeta <- MoodBeta[-which(MoodBeta$year == 1978),]
Real1 <- MoodBeta[which(MoodBeta$HistRank == 1),]
Real2 <- MoodBeta[which(MoodBeta$HistRank == 2),]
Real11 <- MoodBeta[which(MoodBeta$HistRank == 11),]
Real12 <- MoodBeta[which(MoodBeta$HistRank == 12),]
Pre <- MoodBeta[which(MoodBeta$HistRank == "x"),]

MoodBeta <- rbind(Real1, Real2, Real11, Real12, Pre)
MoodBeta <- arrange(MoodBeta, ID, Date, desc(HistRank))


MoodBeta <- left_join(MoodBeta, AvgMonthlyReturns, by = "Date")
MoodBeta <- MoodBeta[,c(1:6, 11)]

?left_join

library(roll)

memory.limit()
?memory.limit


the.firms <- unique(MoodBeta$ID)

?roll_lm

first.step <-  lapply(the.firms, function(a.firm) {
  temp.data <- MoodBeta[MoodBeta$ID.x == a.firm, ]
  an.lm <- roll_lm(temp.data$EW_Month_Return, temp.data$Return.x, width  =  60, min_obs = 40)
  mood.betas <- an.lm$coef[,2]
  the.results <- as.data.frame(cbind(a.firm, mood.betas))
  the.results
}) 

test <- MoodBeta[MoodBeta$ID.x == "MANV.VI",]
lm <- roll_lm(test$EW_Month_Return, test$Return.x, width  =  60, min_obs = 40)
lm$coefficients

lm2 <- roll_lm(test$EW_Month_Return, test$Return.x, width  =  60, min_obs = 60)
lm2$coefficients

first.step.df <- do.call('rbind', first.step)


# Creating Data Frame with mood betas

MoodBeta <- cbind(MoodBeta, first.step.df)

remove(Pre)

library(plyr)

MoodBeta <- MoodBeta %>%
  group_by(ID.x) %>%
  mutate(mood.betas2 = c(NA, head(mood.betas, -1)))
  
  
MoodBeta <- ddply(
  MoodBeta, .(ID.x), transform, 
  mood.betas2 = c(NA, head(mood.betas, -1))
)



detach(package:plyr)
library(dplyr)
library(lubridate)


MoodBeta <- MoodBeta[which(MoodBeta$HistRank == "x"),]


MoodBeta <- MoodBeta %>%
  group_by(ID.x, year) %>% 
  mutate(mood.betas2 = c(head(mood.betas2,1), -head(mood.betas2,1))) 


library(plyr)
RecurrenceBetaLag1 <- ddply(
  MoodBeta, .(ID.x), transform, 
  MoodBetaLag1 = c(rep(NA, 2), head(mood.betas2, -2)),
  Lag1 = c(rep(NA, 2), head(Return.x, -2))
  
)                           
detach(package:plyr)
library(dplyr)
library(lubridate)

RecurrenceBetaLag1 <- RecurrenceBetaLag1[RecurrenceBetaLag1$year > 1989 ,]

RecurrenceBetaLag1 <- RecurrenceBetaLag1 %>%
  group_by(Date) %>% 
  mutate( Lag1Orth = Lag1 - MoodBetaLag1*(mean((MoodBetaLag1 - mean(MoodBetaLag1, na.rm = T))*(Lag1-mean(Lag1, na.rm=T)), na.rm = T)
                                          / mean((MoodBetaLag1 - mean(MoodBetaLag1, na.rm = T))*(MoodBetaLag1-mean(MoodBetaLag1, na.rm = T)), na.rm =T))) 

RecurrenceBetaLag1 <- RecurrenceBetaLag1 %>%
  group_by(Date) %>% 
  mutate( Lag1Orth2 = Lag1 - MoodBetaLag1*(cov(MoodBetaLag1, Lag1, use = "na.or.complete")
                                          / var(MoodBetaLag1, na.rm=T)))



the.dates1 <- unique(RecurrenceBetaLag1$Date)


first.step1 <-  lapply(the.dates1, function(a.date) {
  temp.data <- RecurrenceBetaLag1[RecurrenceBetaLag1$Date == a.date, ]
  res <- residuals(lm(temp.data$Lag1 ~ temp.data$MoodBetaLag1, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag1 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
  })

first.step.df1 <- do.call('rbind', first.step1)

coeftest(lm(first.step.df1$`temp.data$MoodBetaLag1` ~ 1), 
         vcov = NeweyWest(lm(first.step.df1$`temp.data$MoodBetaLag1` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df1$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df1$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

temp.data <- RecurrenceBetaLag1[RecurrenceBetaLag1$Date == "2010-01-29"	, ]
an.lm1 <- lm(temp.data$Lag1 ~ temp.data$MoodBetaLag1, data = temp.data, na.action = na.exclude)
res <- residuals(lm(temp.data$Lag1 ~ temp.data$MoodBetaLag1, data = temp.data, na.action = na.exclude))
res
an.lm1 <- lm(temp.data$Lag1 ~ temp.data$MoodBetaLag1, data = temp.data)
res

cov(temp.data$MoodBetaLag1, res, use= "na.or.complete")

mean((temp.data$MoodBetaLag1 - mean(temp.data$MoodBetaLag1, na.rm=T))*(temp.data$Lag1Orth-mean(temp.data$Lag1Orth, na.rm = T)), na.rm = T)
mean((temp.data$MoodBetaLag1 - mean(temp.data$MoodBetaLag1, na.rm=T))*(temp.data$Lag1Orth2-mean(temp.data$Lag1Orth2, na.rm = T)), na.rm = T)
cov(temp.data$MoodBetaLag1, temp.data$Lag1Orth2, use = "na.or.complete")
cov()

lm(temp.data$Return.x ~ temp.data$MoodBetaLag1temp.data$Lag1)
lm(temp.data$Return.x ~ temp.data$MoodBetaLag1 + res)
lm(temp.data$Return.x ~  res)

# Lag 2-5

RecurrenceBetaLag25 <- MoodBeta %>%
  group_by(ID.x) %>% 
  mutate(MoodBetaLag2 = c(rep(NA, 4), head(mood.betas2, -4))) %>%
  mutate(MoodBetaLag3 = c(rep(NA, 6), head(mood.betas2, -6))) %>%
  mutate(MoodBetaLag4 = c(rep(NA, 8), head(mood.betas2, -8))) %>%
  mutate(MoodBetaLag5 = c(rep(NA, 10), head(mood.betas2, -10))) %>%
  mutate(Lag2 = c(rep(NA, 4), head(Return.x, -4))) %>%
  mutate(Lag3 = c(rep(NA, 6), head(Return.x, -6))) %>%
  mutate(Lag4 = c(rep(NA, 8), head(Return.x, -8))) %>%
  mutate(Lag5 = c(rep(NA, 10), head(Return.x, -10))) 

RecurrenceBetaLag25$Lag25 <- rowMeans(RecurrenceBetaLag25[, 15:18])
RecurrenceBetaLag25$MoodBetaLag25 <- rowMeans(RecurrenceBetaLag25[, 11:14])

  
RecurrenceBetaLag25 <- RecurrenceBetaLag25[RecurrenceBetaLag25$year > 1993 ,]

the.dates25 <- unique(RecurrenceBetaLag25$Date)


first.step25 <-  lapply(the.dates25, function(a.date) {
  temp.data <- RecurrenceBetaLag25[RecurrenceBetaLag25$Date == a.date, ]
  res <- residuals(lm(temp.data$Lag25 ~ temp.data$MoodBetaLag25, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag25 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df25 <- do.call('rbind', first.step25)

coeftest(lm(first.step.df25$`temp.data$MoodBetaLag25` ~ 1), 
         vcov = NeweyWest(lm(first.step.df25$`temp.data$MoodBetaLag25` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df25$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df25$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

# Lag 6-10

RecurrenceBetaLag610 <- MoodBeta %>%
  group_by(ID.x) %>% 
  mutate(MoodBetaLag6 = c(rep(NA, 12), head(mood.betas2, -12))) %>%
  mutate(MoodBetaLag7 = c(rep(NA, 14), head(mood.betas2, -14))) %>%
  mutate(MoodBetaLag8 = c(rep(NA, 16), head(mood.betas2, -16))) %>%
  mutate(MoodBetaLag9 = c(rep(NA, 18), head(mood.betas2, -18))) %>%
  mutate(MoodBetaLag10 = c(rep(NA, 20), head(mood.betas2, -20))) %>%
  mutate(Lag6 = c(rep(NA, 12), head(Return.x, -12))) %>%
  mutate(Lag7 = c(rep(NA, 14), head(Return.x, -14))) %>%
  mutate(Lag8 = c(rep(NA, 16), head(Return.x, -16))) %>%
  mutate(Lag9 = c(rep(NA, 18), head(Return.x, -18))) %>%
  mutate(Lag10 = c(rep(NA, 20), head(Return.x, -20))) 

RecurrenceBetaLag610$Lag610 <- rowMeans(RecurrenceBetaLag610[, 16:20])
RecurrenceBetaLag610$MoodBetaLag610 <- rowMeans(RecurrenceBetaLag610[, 11:15])


RecurrenceBetaLag610 <- RecurrenceBetaLag610[RecurrenceBetaLag610$year > 1998 ,]

the.dates610 <- unique(RecurrenceBetaLag610$Date)


first.step610 <-  lapply(the.dates610, function(a.date) {
  temp.data <- RecurrenceBetaLag610[RecurrenceBetaLag610$Date == a.date, ]
  res <- residuals(lm(temp.data$Lag610 ~ temp.data$MoodBetaLag610, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag610 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df610 <- do.call('rbind', first.step610)

coeftest(lm(first.step.df610$`temp.data$MoodBetaLag610` ~ 1), 
         vcov = NeweyWest(lm(first.step.df610$`temp.data$MoodBetaLag610` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df610$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df610$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))


