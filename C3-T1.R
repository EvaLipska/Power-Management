rm(list=ls())

getwd()
setwd("C:/Users/user/Google Drive/WD")
getwd()

library(tidyr)
library(lubridate)
library(ggplot2)
library(gridExtra)

# import of the data: 4 data frames
year2006 <- read.csv("years/year2006.csv")
year2007 <- read.csv("years/year2007.csv")
year2008 <- read.csv("years/year2008.csv")
year2009 <- read.csv("years/year2009.csv")
year2010 <- read.csv("years/year2010.csv")

# initial data exploration
class(year2006)
names(year2006)
attributes(year2006)

str(year2006)
str(year2007)
str(year2008)
str(year2009)
str(year2010)

summary(year2006)
summary(year2007)
summary(year2008)
summary(year2009)
summary(year2010)

head(year2006)
head(year2007)
head(year2008)
head(year2009)
head(year2010)

tail(year2006)
tail(year2007)
tail(year2008)
tail(year2009)
tail(year2010)

# multi year data frame (MYdf)
MYdf <- bind_rows(year2007, year2008, year2009)
class(MYdf)
MYdf <- as_tibble(MYdf)
class(MYdf)
glimpse(MYdf)
summary(MYdf)
head(MYdf)
tail(MYdf)
names(MYdf)

# check 
dim(MYdf)
nrow(year2007)+ nrow(year2008) + nrow(year2009)
View(MYdf)

# combining Date and Time attributes in a new column (11th column) in order to convert them to the correct format
MYdf <- cbind(MYdf, paste(MYdf$Date, MYdf$Time), stringsAsFactors = FALSE)
head(MYdf)
## Give the new DateTime attribute a header name
colnames(MYdf)[11] <-"DateTime"
head(MYdf)
## Move the DateTime attribute within the dataset
MYdf <- MYdf[, c(ncol(MYdf), 1:(ncol(MYdf)-1))]
head(MYdf)
glimpse(MYdf)

# convert DateTime to POSIXct format 
MYdf$DateTime <- as.POSIXct(MYdf$DateTime, "%d/%m/%Y %H:%M:%S")
glimpse(MYdf)
head(MYdf)

# creating "year" attribute with lubridate (quarter, month, week, day, hour, minute also possible)
MYdf$year <- year(MYdf$DateTime)
head(MYdf)
#MYdf$quarter <- quarter(MYdf$DateTime)
#head(MYdf)
#MYdf$month <- month(MYdf$DateTime)
#head(MYdf)

summary(MYdf)

# removing some columns(x, Date, Time)
MYdf <- MYdf[-c(2, 3, 4)]
glimpse(MYdf)
summary(MYdf)

# analysis 
# min and max values
max(MYdf$Sub_metering_1, na.rm = T)
max(MYdf$Sub_metering_2, na.rm = T)
max(MYdf$Sub_metering_3, na.rm = T)
s <- filter(MYdf, Sub_metering_3 > 0 & Sub_metering_2 > 0 & Sub_metering_1 > 0)
min(s$Sub_metering_1, na.rm = T)
min(s$Sub_metering_2, na.rm = T)
min(s$Sub_metering_3, na.rm = T)

# new attribute names 
# MYdf %>% rename(GAP = Global_active_power, GRP = Global_reactive_power, Vol = Voltage, GI = Global_intensity, SM_1 = Sub_metering_1, SM_2 = Sub_metering_2, SM_3 = Sub_metering_3) %>% select(GAP:SM_3) %>% head(3)

# submeters - years 2007-2009, without 0
# year07 <- MYdf %>% filter(year == 2007 & Sub_metering_3 > 0)

SM_1_years <- MYdf %>% filter(Sub_metering_1 > 0) %>% ggplot(aes(Sub_metering_1)) +
  geom_bar(fill = "#BA3B21") + 
  ggtitle("Submeter 1") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), strip.text = element_text(size = 10, colour = "#ba3b21"), axis.title.y = element_blank(), panel.background = element_rect(colour = "#d4d3d9"), panel.grid = element_blank(), plot.background = element_blank(), strip.background = element_blank()) +
  facet_wrap(~ year)
SM_2_years <- MYdf %>% filter(Sub_metering_2 > 0) %>% ggplot(aes(Sub_metering_2)) +
  geom_bar(fill = "#BA3B21") + 
  ggtitle("Submeter 2") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), strip.text = element_text(size = 10, colour = "#ba3b21"), axis.title.y = element_blank(), panel.background = element_rect(colour = "#d4d3d9"), panel.grid = element_blank(), plot.background = element_blank(), strip.background = element_blank()) +
  facet_wrap(~ year)
SM_3_years <- MYdf %>% filter(Sub_metering_3 > 0) %>% ggplot(aes(Sub_metering_3)) +
  geom_bar(fill = "#BA3B21") + 
  ggtitle("Submeter 3") + 
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_blank(), strip.text = element_text(size = 10, colour = "#ba3b21"), axis.title.y = element_blank(), panel.background = element_rect(colour = "#d4d3d9"), panel.grid = element_blank(), plot.background = element_blank(), strip.background = element_blank()) +
  facet_wrap(~ year)
grid.arrange(SM_1_years, SM_2_years, SM_3_years, nrow = 3)


# also: subset(MYdf, year == 2007)
# par(mfrow = c(2, 2))

summary(year07)
FacetWrapsd(year07$Sub_metering_1, na.rm = T)
sd(year07$Sub_metering_2, na.rm = T)
sd(year07$Sub_metering_3, na.rm = T)


SM1_07 <- ggplot(year07, aes(Sub_metering_1)) + geom_bar() + ggtitle("SM1, year 07")
SM1_07
SM2_07 <- ggplot(year07, aes(Sub_metering_2)) + geom_bar() + ggtitle("SM2, year 07")
SM2_07
SM3_07 <- ggplot(year07, aes(Sub_metering_3)) + geom_bar() + ggtitle("SM3, year 07")
SM3_07

year08 <- MYdf %>% filter(year == 2008)
summary(year08)
sd(year08$Sub_metering_1, na.rm = T)
sd(year08$Sub_metering_2, na.rm = T)
sd(year08$Sub_metering_3, na.rm = T)
SM1_08 <- ggplot(year08, aes(Sub_metering_1)) + geom_bar() + ggtitle("SM1, year 08")
SM1_08
SM2_08 <- ggplot(year08, aes(Sub_metering_2)) + geom_bar() + ggtitle("SM2, year 08")
SM3_08 <- ggplot(year08, aes(Sub_metering_3)) + geom_bar() + ggtitle("SM3, year 08")

year09 <- MYdf %>% filter(year == 2009)
summary(year09)
sd(year09$Sub_metering_1, na.rm = T)
sd(year09$Sub_metering_2, na.rm = T)
sd(year09$Sub_metering_3, na.rm = T)
SM1_09 <- ggplot(year09, aes(Sub_metering_1)) + geom_bar() + ggtitle("SM1, year09")
SM2_09 <- ggplot(year09, aes(Sub_metering_2)) + geom_bar() + ggtitle("SM2, year09")
SM3_09 <- ggplot(year09, aes(Sub_metering_3)) + geom_bar() + ggtitle("SM3, year09")

#Sub-meters overall sd, summary
summary(MYdf$Sub_metering_1)
summary(MYdf$Sub_metering_2)
summary(MYdf$Sub_metering_3)

sd(s$Sub_metering_1, na.rm = T)
sd(s$Sub_metering_2, na.rm = T)
sd(s$Sub_metering_3, na.rm = T)

Sub1_2007 <- sum(year07$Sub_metering_1, na.rm = T)
Sub1_2007
Sub2_2007 <- sum(year07$Sub_metering_2, na.rm = T)
Sub2_2007
Sub3_2007 <- sum(year07$Sub_metering_3, na.rm = T)
Sub3_2007

Sum2007 <- Sub1_2007 + Sub2_2007 + Sub3_2007

Sub1_2008 <- sum(year08$Sub_metering_1, na.rm = T)
Sub1_2008
Sub2_2008 <- sum(year08$Sub_metering_2, na.rm = T)
Sub2_2008
Sub3_2008 <- sum(year08$Sub_metering_3, na.rm = T)
Sub3_2008

Sum2008 <- Sub1_2008 + Sub2_2008 + Sub3_2008

Sub1_2009 <- sum(year09$Sub_metering_1, na.rm = T)
Sub1_2009

Sub2_2009 <- sum(year09$Sub_metering_2, na.rm = T)
Sub2_2009
Sub3_2009 <- sum(year09$Sub_metering_3, na.rm = T)
Sub3_2009

Sum2009 <- Sub1_2009 + Sub2_2009 + Sub3_2009 
Sub1_2009 / Sum2009
Sub2_2009 / Sum2009
Sub3_2009 / Sum2009

YearsTotal <- Sum2007 + Sum2008 + Sum2009
perc2007 <- Sum2007 / YearsTotal
perc2007
perc2008 <- Sum2008 /YearsTotal
perc2008
perc2009 <- Sum2009 / YearsTotal
perc2009

S1 <- sum(MYdf$Sub_metering_1, na.rm = T)
S2 <- sum(MYdf$Sub_metering_2, na.rm = T)
S3 <- sum(MYdf$Sub_metering_3, na.rm = T)

S1 / (S1 + S2 + S3)
S2 / (S1 + S2 + S3)
S3 / (S1 + S2 + S3)

# boxplot (0s removed)
par(mfrow = c(1,3))

s <- filter(MYdf, Sub_metering_1 > 0)
boxplot(s$Sub_metering_1,  na.rm = T, horizontal = T, main = "SubM1")

c <- filter(MYdf, Sub_metering_2 > 0)
boxplot(c$Sub_metering_2,  na.rm = T, horizontal = T, main = "SubM2")

g <- filter(MYdf, Sub_metering_3 > 0)
boxplot(g$Sub_metering_3,  na.rm = T, horizontal = T, main = "SubM3")

save.image()


