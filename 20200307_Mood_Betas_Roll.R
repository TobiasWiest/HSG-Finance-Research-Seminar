#### Importing the dataset

library(readxl)
library(tidyverse)
library(plm)
library(reshape2)
library(forecast)
library(fBasics)
library(AER)


Data <- read_excel("20200226_Data.xlsx")
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

rfrate <- read_excel("GermanYield.xlsx")
?read.csv

ReturnCalc <- ReturnCalc %>%
  group_by(ID) %>%
  mutate(rf = rfrate$rfrate) %>%
  mutate(XReturn = Return - rf/12)


AvgMonthlyReturns <- ReturnCalc %>%
  mutate(Date = as.Date(Date)) %>% 
  group_by(Date) %>% 
  mutate(EW_Month_Return = mean(XReturn, na.rm = TRUE)) %>% 
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
  select(ID, Date, XReturn, Return, month)

remove(January, September, NoReturns, Returns, Test)


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
  select(ID, Date, HistRank, rf, XReturn, Return) 

MoodBetaRealInd <- MoodBetaRealInd %>% 
  mutate(year = as.numeric(format(Date, "%Y"))) 


MoodBetaRealInd <- MoodBetaRealInd %>% 
  mutate(Date = as.Date(Date)) %>%
  mutate(month = format(Date, "%m")) %>%
  mutate(HistRank = as.character(HistRank))

RecurrenceReg$HistRank <- c(rep("x", 490560))
RecurrenceReg <- RecurrenceReg %>% 
  mutate(year = as.numeric(format(Date, "%Y"))) 

MoodBeta <- rbind(RecurrenceReg, MoodBetaRealInd)
MoodBeta <- arrange(MoodBeta, ID, Date)

MoodBeta <- MoodBeta[-which(MoodBeta$year == 1978),]
Real1 <- MoodBeta[which(MoodBeta$HistRank == "1"),]
Real2 <- MoodBeta[which(MoodBeta$HistRank == "2"),]
Real11 <- MoodBeta[which(MoodBeta$HistRank == "11"),]
Real12 <- MoodBeta[which(MoodBeta$HistRank == "12"),]
Pre <- MoodBeta[which(MoodBeta$HistRank == "x"),]

MoodBeta <- rbind(Real1, Real2, Real11, Real12, Pre)
MoodBeta <- arrange(MoodBeta, ID, Date, desc(HistRank))


MoodBeta <- MoodBeta %>% 
 left_join(AvgMonthlyReturns, by = "Date") %>%
 select(ID.x, Date, year, month, HistRank, rf.x, XReturn.x, Return.x, EW_Month_Return)

colnames(MoodBeta) <- c("ID", "Date", "year", "month", "HistRank", "rf", "XReturn", "Return", "EW_Month_Return")


library(roll)


the.firms <- unique(MoodBeta$ID)

first.step <-  lapply(the.firms, function(a.firm) {
  temp.data <- MoodBeta[MoodBeta$ID == a.firm, ]
  an.lm <- roll_lm(temp.data$EW_Month_Return, temp.data$XReturn, width  =  60, min_obs = 40)
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

MoodBeta <- as.data.frame(MoodBeta)
first.step.df <- as.data.frame(first.step.df)
MoodBeta <- cbind(MoodBeta, first.step.df)


MoodBeta <- MoodBeta %>%
  group_by(ID) %>%
  mutate(mood.betas2 = c(NA, head(mood.betas, -1)))


MoodBeta <- MoodBeta[which(MoodBeta$HistRank == "x"),]


MoodBeta <- MoodBeta %>%
  group_by(ID, year) %>% 
  mutate(mood.betas2 = c(head(mood.betas2,1), -head(mood.betas2,1)))

Real1$month <- "01"
Real12$month <- "09"

Real <- rbind(Real1, Real12)
colnames(Real)[1] <- "ID"


MoodBeta<- MoodBeta %>% 
  left_join(Real, by = c("ID", "year", "month")) %>% 
  arrange(ID, Date.x) 

colnames(MoodBeta)[2] <- "Date"
MoodBeta <- MoodBeta[,-c(13,14, 15, 17)]
colnames(MoodBeta)[13] <- "Return.r"

#Lag1

BetaLag1 <- MoodBeta %>%
  group_by(ID) %>%
  mutate(MoodBetaLag1 = c(rep(NA, 2), head(mood.betas2, -2))) %>%
  mutate (Lag1 = c(rep(NA, 2), head(Return.x, -2)))

BetaLag1 <- BetaLag1 %>%
  group_by(ID, year) %>% 
  mutate(ReturnRev = c(tail(Return.x,-1), head(Return.x,-1)))

BetaLag1 <- BetaLag1 %>%
  group_by(ID) %>%
  mutate(RevLag1 = c(rep(NA, 2), head(ReturnRev, -2)))

BetaLag1 <- BetaLag1 %>%
  group_by(ID) %>%
  mutate (Lag1.r = c(rep(NA, 2), head(Return.r, -2)))

BetaLag1 <- BetaLag1 %>%
  group_by(ID, year) %>% 
  mutate(ReturnRev.r = c(tail(Return.r,-1), head(Return.r,-1)))

BetaLag1 <- BetaLag1 %>%
  group_by(ID) %>%
  mutate(RevLag1.r = c(rep(NA, 2), head(ReturnRev.r, -2)))


BetaLag1$Lag1[is.na(BetaLag1$RevLag1)] <- NA
BetaLag1$RevLag1[is.na(BetaLag1$Lag1)] <- NA
BetaLag1$Lag1.r[is.na(BetaLag1$RevLag1.r)] <- NA
BetaLag1$RevLag1.r[is.na(BetaLag1$Lag1.r)] <- NA
BetaLag1$Lag1.r[is.na(BetaLag1$Lag1)] <- NA
BetaLag1$Lag1[is.na(BetaLag1$Lag1.r)] <- NA
BetaLag1$RevLag1.r[is.na(BetaLag1$RevLag1)] <- NA
BetaLag1$RevLag1[is.na(BetaLag1$RevLag1.r)] <- NA




BetaLag1 <- BetaLag1[BetaLag1$year > 1989 ,]





the.dates1 <- unique(BetaLag1$Date)


first.step1 <-  lapply(the.dates1, function(a.date) {
  temp.data <- BetaLag1[BetaLag1$Date == a.date, ]
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

first.step1rev <-  lapply(the.dates1, function(a.date) {
  temp.data <- BetaLag1[BetaLag1$Date == a.date, ]
  res <- residuals(lm(temp.data$RevLag1 ~ temp.data$MoodBetaLag1, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag1 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df1rev <- do.call('rbind', first.step1rev)

coeftest(lm(first.step.df1rev$`temp.data$MoodBetaLag1` ~ 1), 
         vcov = NeweyWest(lm(first.step.df1rev$`temp.data$MoodBetaLag1` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df1rev$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df1rev$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

first.step1.r <-  lapply(the.dates1, function(a.date) {
  temp.data <- BetaLag1[BetaLag1$Date == a.date, ]
  res <- residuals(lm(temp.data$Lag1.r ~ temp.data$MoodBetaLag1, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag1 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df1.r <- do.call('rbind', first.step1.r)

coeftest(lm(first.step.df1.r$`temp.data$MoodBetaLag1` ~ 1), 
         vcov = NeweyWest(lm(first.step.df1.r$`temp.data$MoodBetaLag1` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df1.r$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df1.r$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))


first.step1rev.r <-  lapply(the.dates1, function(a.date) {
  temp.data <- BetaLag1[BetaLag1$Date == a.date, ]
  res <- residuals(lm(temp.data$RevLag1.r ~ temp.data$MoodBetaLag1, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag1 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df1rev.r <- do.call('rbind', first.step1rev.r)

coeftest(lm(first.step.df1rev.r$`temp.data$MoodBetaLag1` ~ 1), 
         vcov = NeweyWest(lm(first.step.df1rev.r$`temp.data$MoodBetaLag1` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df1rev.r$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df1rev.r$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))



# Lag 2-5

BetaLag25 <- MoodBeta %>%
  group_by(ID) %>% 
  mutate(MoodBetaLag2 = c(rep(NA, 4), head(mood.betas2, -4))) %>%
  mutate(MoodBetaLag3 = c(rep(NA, 6), head(mood.betas2, -6))) %>%
  mutate(MoodBetaLag4 = c(rep(NA, 8), head(mood.betas2, -8))) %>%
  mutate(MoodBetaLag5 = c(rep(NA, 10), head(mood.betas2, -10))) %>%
  mutate(Lag2 = c(rep(NA, 4), head(Return.x, -4))) %>%
  mutate(Lag3 = c(rep(NA, 6), head(Return.x, -6))) %>%
  mutate(Lag4 = c(rep(NA, 8), head(Return.x, -8))) %>%
  mutate(Lag5 = c(rep(NA, 10), head(Return.x, -10))) 

BetaLag25$Lag25 <- rowMeans(BetaLag25[, 18:21])
BetaLag25$MoodBetaLag25 <- rowMeans(BetaLag25[, 14:17])

BetaLag25 <- BetaLag25 %>%
  group_by(ID, year) %>% 
  mutate(ReturnRev = c(tail(Return.x,-1), head(Return.x,-1)))

BetaLag25 <- BetaLag25 %>%
  group_by(ID) %>% 
  mutate(RevLag2 = c(rep(NA, 4), head(ReturnRev, -4))) %>%
  mutate(RevLag3 = c(rep(NA, 6), head(ReturnRev, -6))) %>%
  mutate(RevLag4 = c(rep(NA, 8), head(ReturnRev, -8))) %>%
  mutate(RevLag5 = c(rep(NA, 10), head(ReturnRev, -10))) 

BetaLag25$RevLag25 <- rowMeans(BetaLag25[, 25:28])

BetaLag25 <- BetaLag25 %>%
  group_by(ID) %>% 
  mutate(Lag2.r = c(rep(NA, 4), head(Return.r, -4))) %>%
  mutate(Lag3.r = c(rep(NA, 6), head(Return.r, -6))) %>%
  mutate(Lag4.r = c(rep(NA, 8), head(Return.r, -8))) %>%
  mutate(Lag5.r = c(rep(NA, 10), head(Return.r, -10))) 

BetaLag25$Lag25.r <- rowMeans(BetaLag25[, 30:33])

BetaLag25 <- BetaLag25 %>%
  group_by(ID, year) %>% 
  mutate(ReturnRev.r = c(tail(Return.r,-1), head(Return.r,-1)))

BetaLag25 <- BetaLag25 %>%
  group_by(ID) %>% 
  mutate(RevLag2.r = c(rep(NA, 4), head(ReturnRev.r, -4))) %>%
  mutate(RevLag3.r = c(rep(NA, 6), head(ReturnRev.r, -6))) %>%
  mutate(RevLag4.r = c(rep(NA, 8), head(ReturnRev.r, -8))) %>%
  mutate(RevLag5.r = c(rep(NA, 10), head(ReturnRev.r, -10))) 

BetaLag25$RevLag25.r <- rowMeans(BetaLag25[, 36:39])

BetaLag25$Lag25[is.na(BetaLag25$RevLag25)] <- NA
BetaLag25$RevLag25[is.na(BetaLag25$Lag25)] <- NA
BetaLag25$Lag25.r[is.na(BetaLag25$RevLag25.r)] <- NA
BetaLag25$RevLag25.r[is.na(BetaLag25$Lag25.r)] <- NA
BetaLag25$Lag25.r[is.na(BetaLag25$Lag25)] <- NA
BetaLag25$Lag25[is.na(BetaLag25$Lag25.r)] <- NA
BetaLag25$RevLag25.r[is.na(BetaLag25$RevLag25)] <- NA
BetaLag25$RevLag25[is.na(BetaLag25$RevLag25.r)] <- NA

BetaLag25 <- BetaLag25[BetaLag25$year > 1993 ,]

the.dates25 <- unique(BetaLag25$Date)


first.step25 <-  lapply(the.dates25, function(a.date) {
  temp.data <- BetaLag25[BetaLag25$Date == a.date, ]
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


first.step25rev <-  lapply(the.dates25, function(a.date) {
  temp.data <- BetaLag25[BetaLag25$Date == a.date, ]
  res <- residuals(lm(temp.data$RevLag25 ~ temp.data$MoodBetaLag25, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag25 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df25rev <- do.call('rbind', first.step25rev)

coeftest(lm(first.step.df25rev$`temp.data$MoodBetaLag25` ~ 1), 
         vcov = NeweyWest(lm(first.step.df25rev$`temp.data$MoodBetaLag25` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df25rev$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df25rev$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

first.step25.r <-  lapply(the.dates25, function(a.date) {
  temp.data <- BetaLag25[BetaLag25$Date == a.date, ]
  res <- residuals(lm(temp.data$Lag25.r ~ temp.data$MoodBetaLag25, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag25 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df25.r <- do.call('rbind', first.step25.r)

coeftest(lm(first.step.df25.r$`temp.data$MoodBetaLag25` ~ 1), 
         vcov = NeweyWest(lm(first.step.df25.r$`temp.data$MoodBetaLag25` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df25$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df25$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))


first.step25rev.r <-  lapply(the.dates25, function(a.date) {
  temp.data <- BetaLag25[BetaLag25$Date == a.date, ]
  res <- residuals(lm(temp.data$RevLag25.r ~ temp.data$MoodBetaLag25, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag25 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df25rev.r <- do.call('rbind', first.step25rev.r)

coeftest(lm(first.step.df25rev.r$`temp.data$MoodBetaLag25` ~ 1), 
         vcov = NeweyWest(lm(first.step.df25rev.r$`temp.data$MoodBetaLag25` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df25rev$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df25rev$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))


# Lag 6-10

BetaLag610 <- MoodBeta %>%
  group_by(ID) %>% 
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

BetaLag610$Lag610 <- rowMeans(BetaLag610[, 19:23])
BetaLag610$MoodBetaLag610 <- rowMeans(BetaLag610[, 14:18])

BetaLag610 <- BetaLag610 %>%
  group_by(ID, year) %>% 
  mutate(ReturnRev = c(tail(Return.x,-1), head(Return.x,-1)))

BetaLag610 <- BetaLag610 %>%
  group_by(ID) %>% 
  mutate(RevLag6 = c(rep(NA, 12), head(ReturnRev, -12))) %>%
  mutate(RevLag7 = c(rep(NA, 14), head(ReturnRev, -14))) %>%
  mutate(RevLag8 = c(rep(NA, 16), head(ReturnRev, -16))) %>%
  mutate(RevLag9 = c(rep(NA, 18), head(ReturnRev, -18))) %>%
  mutate(RevLag10 = c(rep(NA, 20), head(ReturnRev, -20))) 

BetaLag610$RevLag610 <- rowMeans(BetaLag610[, 27:31])

BetaLag610 <- BetaLag610 %>%
  group_by(ID) %>% 
  mutate(Lag6.r = c(rep(NA, 12), head(Return.r, -12))) %>%
  mutate(Lag7.r = c(rep(NA, 14), head(Return.r, -14))) %>%
  mutate(Lag8.r = c(rep(NA, 16), head(Return.r, -16))) %>%
  mutate(Lag9.r = c(rep(NA, 18), head(Return.r, -18))) %>%
  mutate(Lag10.r = c(rep(NA, 20), head(Return.r, -20)))

BetaLag610$Lag610.r <- rowMeans(BetaLag610[, 33:37])

BetaLag610 <- BetaLag610 %>%
  group_by(ID, year) %>% 
  mutate(ReturnRev.r = c(tail(Return.r,-1), head(Return.r,-1)))

BetaLag610 <- BetaLag610 %>%
  group_by(ID) %>% 
  mutate(RevLag6.r = c(rep(NA, 12), head(ReturnRev.r, -12))) %>%
  mutate(RevLag7.r = c(rep(NA, 14), head(ReturnRev.r, -14))) %>%
  mutate(RevLag8.r = c(rep(NA, 16), head(ReturnRev.r, -16))) %>%
  mutate(RevLag9.r = c(rep(NA, 18), head(ReturnRev.r, -18))) %>%
  mutate(RevLag10.r = c(rep(NA, 20), head(ReturnRev.r, -20))) 


BetaLag610$RevLag610.r <- rowMeans(BetaLag610[, 40:44])

BetaLag610$Lag610[is.na(BetaLag610$RevLag610)] <- NA
BetaLag610$RevLag610[is.na(BetaLag610$Lag610)] <- NA
BetaLag610$Lag610.r[is.na(BetaLag610$RevLag610.r)] <- NA
BetaLag610$RevLag610.r[is.na(BetaLag610$Lag610.r)] <- NA
BetaLag610$Lag610.r[is.na(BetaLag610$Lag610)] <- NA
BetaLag610$Lag610[is.na(BetaLag610$Lag610.r)] <- NA
BetaLag610$RevLag610.r[is.na(BetaLag610$RevLag610)] <- NA
BetaLag610$RevLag610[is.na(BetaLag610$RevLag610.r)] <- NA



BetaLag610 <- BetaLag610[BetaLag610$year > 1998 ,]

the.dates610 <- unique(BetaLag610$Date)


first.step610 <-  lapply(the.dates610, function(a.date) {
  temp.data <- BetaLag610[BetaLag610$Date == a.date, ]
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

first.step610rev <-  lapply(the.dates610, function(a.date) {
  temp.data <- BetaLag610[BetaLag610$Date == a.date, ]
  res <- residuals(lm(temp.data$RevLag610 ~ temp.data$MoodBetaLag610, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag610 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df610rev <- do.call('rbind', first.step610rev)

coeftest(lm(first.step.df610rev$`temp.data$MoodBetaLag610` ~ 1), 
         vcov = NeweyWest(lm(first.step.df610rev$`temp.data$MoodBetaLag610` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df610rev$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df610rev$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

first.step610.r <-  lapply(the.dates610, function(a.date) {
  temp.data <- BetaLag610[BetaLag610$Date == a.date, ]
  res <- residuals(lm(temp.data$Lag610.r ~ temp.data$MoodBetaLag610, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag610 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df610.r <- do.call('rbind', first.step610.r)

coeftest(lm(first.step.df610.r$`temp.data$MoodBetaLag610` ~ 1), 
         vcov = NeweyWest(lm(first.step.df610.r$`temp.data$MoodBetaLag610` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df610.r$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df610.r$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

first.step610rev.r <-  lapply(the.dates610, function(a.date) {
  temp.data <- BetaLag610[BetaLag610$Date == a.date, ]
  res <- residuals(lm(temp.data$RevLag610.r ~ temp.data$MoodBetaLag610, data = temp.data, na.action = na.exclude))
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag610 + res)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df610rev.r <- do.call('rbind', first.step610rev.r)

coeftest(lm(first.step.df610rev.r$`temp.data$MoodBetaLag610` ~ 1), 
         vcov = NeweyWest(lm(first.step.df610rev.r$`temp.data$MoodBetaLag610` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df610rev.r$res ~ 1), 
         vcov = NeweyWest(lm(first.step.df610rev.r$res ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))




# Market Beta

DataMC <- read_excel("DataMC.xlsx")
DataMC <- DataMC %>% 
  mutate(Date = as.Date(DataMC$Date, format = "%Y-%m"))

LongDataMC <- DataMC %>% 
  melt(value.name = "MarketCap", id.vars = "Date", variable.name = "ID") 

LongDataMC %>% 
  distinct(ID) %>% 
  count()

ReturnCalc <- ReturnCalc %>%
  left_join(LongDataMC, by = c("ID", "Date")) %>% 
  arrange(ID, Date)

ReturnCalc$MarketCap <- as.numeric(as.character(ReturnCalc$MarketCap))
ReturnCalc$MarketCap[is.na(ReturnCalc$Return)] <- NA

ReturnCalc <- ReturnCalc %>%
  group_by(Date) %>%
  mutate(MarketShare = MarketCap / sum(MarketCap, na.rm = T))

#ReturnCalc <- ReturnCalc %>%
# group_by(Date) %>%
#mutate(check = sum(MarketShare, na.rm = T))

ReturnCalc <- ReturnCalc %>%
  group_by(Date) %>%
  mutate(MarketReturn = sum(Return * MarketShare, na.rm = T)) %>%
  mutate(XMarketReturn = (MarketReturn - rf/12))

ReturnCalc <- ReturnCalc[-which(ReturnCalc$Date == as.Date("1978/12/31", format ="%Y/%m/%d")),]



the.firms.m <- unique(ReturnCalc$ID)

first.step.m <-  lapply(the.firms.m, function(a.firm) {
  temp.data <- ReturnCalc[ReturnCalc$ID == a.firm, ]
  an.lm <- roll_lm(temp.data$XMarketReturn, temp.data$XReturn, width  =  60, min_obs = 40)
  market.betas <- an.lm$coef[,2]
  the.results <- as.data.frame(cbind(a.firm, market.betas))
  the.results
}) 


first.step.df.m <- do.call('rbind', first.step.m)
first.step.df.m <- as.data.frame(first.step.df.m)
ReturnCalc <- as.data.frame(ReturnCalc)
ReturnCalc2 <- cbind(ReturnCalc, first.step.df.m)
ReturnCalc2 <- ReturnCalc2 %>%
  group_by(ID) %>%
  mutate(market.betas2 = c(NA, head(market.betas, -1)))

ReturnCalc2 <- ReturnCalc2 %>% 
  mutate(month = format(Date, "%m")) %>% 
  mutate(year = as.numeric(format(Date, "%Y")))

ReturnCalc3 <- ReturnCalc2[which(ReturnCalc2$month == "01"),]
ReturnCalc4 <- ReturnCalc2[which(ReturnCalc2$month == "09"),]

ReturnCalc5 <- rbind(ReturnCalc3, ReturnCalc4)
ReturnCalc5 <- arrange(ReturnCalc5, ID, Date)

MarketBetaLag1 <- ReturnCalc5 %>%
  group_by(ID, year) %>%
  mutate(market.betas2 = c(head(market.betas2, -1), head(market.betas2, -1)))

MarketBetaLag1 <- MarketBetaLag1 %>%
  group_by(ID) %>%
  mutate(MarketBetaLag1 = c(rep(NA, 2), head(market.betas2, -2))) 
  

MarketBetaLag1 <- MarketBetaLag1[ReturnCalc5$year > 1989 ,]

BetaCompLag1 <- BetaLag1 %>%
  left_join(MarketBetaLag1, by = c("ID", "Date")) %>%
  select("ID", "Date",  "Return.x", "MoodBetaLag1","MarketBetaLag1")


the.dates1 <- unique(BetaCompLag1$Date)


first.step1.c <-  lapply(the.dates1, function(a.date) {
  temp.data <- BetaCompLag1[BetaCompLag1$Date == a.date, ]
  an.lm <- lm(temp.data$Return.x ~ temp.data$MoodBetaLag1 + temp.data$MarketBetaLag1)
  the.coefficients <- an.lm$coef
  the.results <- as.data.frame(cbind(a.date, t(the.coefficients)))
  the.results
})

first.step.df1.c <- do.call('rbind', first.step1.c)

coeftest(lm(first.step.df1.c$`temp.data$MoodBetaLag1` ~ 1), 
         vcov = NeweyWest(lm(first.step.df1.c$`temp.data$MoodBetaLag1` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

coeftest(lm(first.step.df1.c$`temp.data$MarketBetaLag1` ~ 1), 
         vcov = NeweyWest(lm(first.step.df1.c$`temp.data$MarketBetaLag1` ~ 1), 
                          lag = 3, prewhite = FALSE,
                          adjust = TRUE))

pmg(Return.x ~ MoodBetaLag1 + MarketBetaLag1, data = BetaCompLag1, index= c("Date", "ID"))
