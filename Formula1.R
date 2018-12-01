#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("forecast")
#install.packages("lubridate")
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)

#setwd("~/Academia/Formula One Data Analytics")

# Reading results dataset
results <- read.csv("datasets/results.csv", sep = ',', header=T, 
                    strip.white = T, na.strings = c("NA","NaN","","?"))

# Examining data
str(results)
summary(results)
head(results)

# Reading races dataset
races <- read.csv("datasets/races.csv", sep = ',', skipNul = T)
str(races)
summary(races)
head(races)

# Removing "Grand Prix" from the race name
races$name <- sub(" Grand Prix", "", races$name)
head(races)

# Joining results and races
results1 <- inner_join(results, races, by = "raceId")

# Dropping columns - time and url
results2 <- results1[,c(-12,-24,-25)]

summary(results2)
head(results2)

# Looking at records with no finish time - milliseconds
sum(is.na(results2$milliseconds))

# Only keeping rows with finish time - milliseconds
results2_finished <- results2[!is.na(results2$milliseconds),]

summary(results2_finished)
str(results2_finished)

# Difference between the first and the last place of each race since 2000
time_to_finish_spread_all <- results2_finished %>%
  filter(year >= 2000) %>%
  group_by(raceId, year, name) %>%
  summarise(first_place = min(milliseconds, na.rm = T), 
            last_place = max(milliseconds, na.rm = T),
            Difference = (max(milliseconds, na.rm = T) 
                          - min(milliseconds, na.rm = T)) / 1000)

str(time_to_finish_spread_all)
summary(time_to_finish_spread_all)

# Plotting median time difference between first and the last position
# Point graph - Median difference
time_to_finish_spread_all %>%
  group_by(year) %>%
  summarise(Median_Difference = median(Difference)) %>%
  ggplot(aes(x = factor(year), y = Median_Difference, color = Median_Difference)) + 
  geom_point() +
  labs(x = 'Year',  y = 'Median Time Difference (in 1000)',
       title = 'Median difference in race fInish time between first and the last position') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculating median difference by year
time_to_finish_spread_all_year <-
  time_to_finish_spread_all %>%
  group_by(year) %>%
  summarise(Median_Difference = median(Difference))


# Convering to a time series
x_all = ts(time_to_finish_spread_all_year[,2], start = c(2000), frequency = 1)
x_all

# Plotting time series
plot(x_all) 

# Phillips-Perron Unit Root Test to check if the data is random walk
# If p-value is > 0.5 that means it is a random walk
PP.test(x_all)

# Plotting ACF and PACF
par(mfrow = c(1,2))
acf(x_all,main='ACF Mean Diff')
pacf(x_all,main='PACF Mean Diff')

# Resetting plot
par(mfrow = c(1,1))

# Calculating ARIMA() to determine AR and MA orders
ARIMAfit_all = auto.arima(x_all, approximation=FALSE,trace=TRUE)
#  Best model: ARIMA(1,1,0)  

summary(ARIMAfit_all)

# Predicting for next 3 years
pred_all = predict(ARIMAfit_all, n.ahead = 3)
pred_all

# Creating time series from the last observation
ts1 = ts(x_all[length(x_all)], start = c(2017))

# Plotting prediction
plot(x_all, type='l', xlim=c(2000,2020), ylim=c(60,110), 
     xlab = 'Year', ylab = 'Median Difference (In Seconds)',
     main = 'Time difference between the first and the last driver')
lines(ts(c(ts1, pred_all$pred), start = c(2017)),col='blue')
lines(ts(c(ts1, pred_all$pred+2*pred_all$se), start = c(2017)),col='orange')
lines(ts(c(ts1, pred_all$pred-2*pred_all$se), start = c(2017)),col='orange')

# Plotting difference between the first and the last place since 2000
time_to_finish_spread_all %>%
  ggplot(aes(x = factor(year), y = Difference, color = Difference)) +
  #  geom_boxplot(alpha=0) +
  geom_jitter(alpha = 0.5, size = 1.5) +
  labs(x = 'Year',  y = 'Finish Time Difference (in Seconds)',
       title = 'Difference in race fInish time between first and the second position') +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Plotting difference between the first and the last place of each race since 2000
time_to_finish_spread_all %>%
  ggplot(aes(x = factor(year), y = Difference, color = Difference)) +
  geom_point(size = 1) +
  facet_wrap(~ name, ncol = 9) +
  labs(x = 'Year',  y = 'Difference (in Seconds)',
       title = 'Time difference between the first and the last driver') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.6)))

##
# Difference between the first and the second place finisher since 2000
time_to_finish_spread_top2 <- results2_finished %>%
  filter(year >= 2000) %>%
  group_by(raceId, year, name) %>%
  top_n(n = -2, wt = positionOrder) %>%
  summarise(first_place = min(milliseconds, na.rm = T), 
            second_place = max(milliseconds, na.rm = T),
            Difference = (max(milliseconds, na.rm = T) 
                          - min(milliseconds, na.rm = T)) / 1000)
  
str(time_to_finish_spread_top2)
summary(time_to_finish_spread_top2)

# Plotting median time difference between first and the second position
# Point graph - Median difference
time_to_finish_spread_top2 %>%
  group_by(year) %>%
  summarise(Median_Difference = median(Difference)) %>%
  ggplot(aes(x = factor(year), y = Median_Difference, color = Median_Difference)) + 
  geom_point() +
  labs(x = 'Year',  y = 'Median Time Difference (in Seconds)',
       title = 'Median difference in race fInish time between first and the second position') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculating median difference by year
time_to_finish_spread_top2_year <-
  time_to_finish_spread_top2 %>%
  group_by(year) %>%
  summarise(Median_Difference = median(Difference))

# Convering to a time series
x_top2 = ts(time_to_finish_spread_top2_year[,2], start = c(2000), frequency = 1)
x_top2

# Plotting time series
plot(x_top2)

# Phillips-Perron Unit Root Test to check if the data is random walk
# If p-value is > 0.5 that means it is a random walk
PP.test(x_top2)

# Plotting ACF and PACF
par(mfrow = c(1,2))
acf(x_top2,main='ACF Mean Diff')
pacf(x_top2,main='PACF Mean Diff')

# Resetting plot
par(mfrow = c(1,1))

# Calculating ARIMA() to determine AR and MA orders
ARIMAfit_top2 = auto.arima(x_top2, approximation=FALSE,trace=TRUE)

# Best model: ARIMA(0,0,1) with non-zero mean 

summary(ARIMAfit_top2)

# Prediction
pred_top2 = predict(ARIMAfit_top2, n.ahead = 3)
pred_top2

# Creating time series from the last observation
ts2 = ts(x_top2[length(x_top2)], start = c(2017))

# Plotting prediction
plot(x_top2, type='l', xlim=c(2000,2020), 
     xlab = 'Year', ylab = 'Median Difference (In Seconds)',
     main = 'Time difference between the first and the second driver')
lines(ts(c(ts2, pred_top2$pred), start = c(2017)),col='blue')
lines(ts(c(ts2, pred_top2$pred+2*pred_top2$se), start = c(2017)),col='orange')
lines(ts(c(ts2, pred_top2$pred-2*pred_top2$se), start = c(2017)),col='orange')

# Plotting difference between the first and the second place since 2000
time_to_finish_spread_top2 %>%
  ggplot(aes(x = factor(year), y = Difference, color = Difference)) +
  #  geom_boxplot(alpha=0) +
  geom_jitter(alpha = 0.5, size = 1.5) +
  labs(x = 'Year',  y = 'Finish Time Difference (in Seconds)',
       title = 'Difference in race fInish time between first and the second position') +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# Plotting difference between the first and the second place for each race since 2000
time_to_finish_spread_top2 %>%
  ggplot(aes(x = factor(year), y = Difference, color = Difference)) +
  geom_point(size = 1) +
  facet_wrap(~ name, ncol = 9) +
  labs(x = 'Year',  y = 'Mean Difference (in Seconds)',
       title = 'Time difference between the first and the second driver') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.6)))

###
# Fastest Lap for each race
# Verifying if there are any records with no fastestLapTime
sum(is.na(results2$fastestLapTime))

# Only keeping rows with fastestLapTime values
results2_FastestLaps <- results2[!is.na(results2$fastestLapTime),]

str(results2_FastestLaps$fastestLapTime)

# Converting Fastest Lap Time to seconds using period_to_seconds()
results2_FastestLaps$fastestLapTime <- 
  as.character(results2_FastestLaps$fastestLapTime)
results2_FastestLaps$fastestLapTime <- 
  period_to_seconds(ms(results2_FastestLaps$fastestLapTime))

str(results2_FastestLaps$fastestLapTime)

# Fastest laps for each race since 2004
results2_FastestLaps %>%
  filter(year >= 2004) %>%
  group_by(raceId) %>%
  top_n(n=-1, wt=fastestLapTime) %>%
  ggplot(aes(x = factor(year), y = fastestLapTime, color = fastestLapTime)) +
  geom_point(size = 1) +
  facet_wrap(~ name, ncol = 9) +
  labs(x = 'Year',  y = 'Fastest Lap Time (in Seconds)',
       title = 'Fastest lap times') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.6)))


###
# Maximum Speed for each race since 2004
# Verifying if there are any records with no fastestLapSpeed
sum(is.na(results2$fastestLapSpeed))

# Only keeping rows with values for fastestLapSpeed
results2_FastestLaps2 <- results2[!is.na(results2$fastestLapSpeed),]

str(results2_FastestLaps2$fastestLapSpeed)

# Looking at the incorrect Fastest Lap Speed row
results2_FastestLaps2 %>%
  filter(fastestLapSpeed == "01:42.6")

# Removing row with incorrect fastes lap speed data
results2_FastestLaps2 <- results2_FastestLaps2 %>%
  filter(fastestLapSpeed != '01:42.6')


# Converting Fastest Lap Speed to numeric
results2_FastestLaps2$fastestLapSpeed <- 
  as.numeric(as.character(results2_FastestLaps2$fastestLapSpeed))

str(results2_FastestLaps2$fastestLapSpeed)

# Fastest lap data present from 2004 onwards
# Plotting median fastest lap - year over year for each race
results2_FastestLaps2 %>%
  filter(year >= 2004) %>%
  group_by(name, year) %>%
  summarise(Median_FastestlapSpeed = median(fastestLapSpeed, na.rm = T)) %>%
  ggplot(aes(x = factor(year), y = Median_FastestlapSpeed, 
             color = Median_FastestlapSpeed)) +
  geom_point(size = 1) +
  facet_wrap(~ name, ncol = 9) +
  labs(x = 'Year', y = 'Median Fastest Lap Speed (in km/hr)',
       title = 'Fatstest Lap Speed') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Plotting median fastest lap - year over year
results2_FastestLaps2 %>%
  filter(year >= 2004) %>%
  group_by(year) %>%
  ggplot(aes(x = factor(year), y = fastestLapSpeed, 
             color = fastestLapSpeed)) +
  geom_boxplot(notch = T, alpha = 0) +
  labs(x = 'Year', y = 'Fastest Lap Speed (in km/hr)',
       title = 'Fatstest Lap Speed') +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


# Plotting median fastest lap for all the races across all the years
# Bar graph
results2_FastestLaps2 %>%
  group_by(name) %>%
  summarise(Median_FastestlapSpeed = median(fastestLapSpeed, na.rm = T)) %>%
  ggplot(aes(x = reorder(factor(name), Median_FastestlapSpeed), 
             y = Median_FastestlapSpeed)) + 
  geom_bar(stat = "identity", aes(fill = Median_FastestlapSpeed)) +
  coord_flip() +
  labs(x = 'Grand Prix', y = 'Maximum Speed (in km/hr)',
       title = 'Fatstest Lap Speeds')


# Box Plot
results2_FastestLaps2 %>%
  group_by(name) %>%
  ggplot(aes(x = reorder(factor(name), fastestLapSpeed), 
             y = fastestLapSpeed)) + 
  geom_boxplot(notch = T, alpha = 0) +
  ylim(140, 260) +
  coord_flip() +
  labs(x = 'Grand Prix', y = 'Maximum Speed (in km/hr)',
       title = 'Fatstest Lap Speeds')


# Reading constructors data
# Reading Races
constructors <- read.csv("datasets/constructors.csv", sep = ',', header=T, 
                    strip.white = T, na.strings = c("NA","NaN","","?"))
str(constructors)
summary(constructors)
head(constructors)

# Joining results, races and constructors
results3 <- inner_join(results2, constructors[,c(-5,-6)], by = "constructorId")

str(results3)
summary(results3)
head(results3)

# Renaming columns
colnames(results3)[colnames(results3) == "name.x"] <- "race"
colnames(results3)[colnames(results3) == "name.y"] <- "constructor"

str(results3)
summary(results3)
head(results3)

# Looking at records with no positionOrder
sum(is.na(results3$positionOrder))

# Plotting maximum number of wins by constructors
# Bar graph
results3 %>%
  filter(positionOrder == 1) %>%
  group_by(constructor) %>%
  summarise(Number_of_Wins = sum(positionOrder)) %>%
  ggplot(aes(x = reorder(factor(constructor), Number_of_Wins), 
             y = Number_of_Wins)) + 
  geom_bar(stat = "identity", aes(fill = Number_of_Wins)) +
  geom_text(aes(label = Number_of_Wins), hjust = -0.5, size = 3) +
  coord_flip() +
  labs(x = 'Constructors', y = 'Number of Wins',
       title = 'Number of wins by constructors')


# Determining constructors champion each year by calculating highest total of points
constructor_championship <- results3 %>%
  group_by(year, constructor) %>%
  summarise(Total_Points = sum(points)) %>%
  top_n(n = 1, wt = Total_Points)

str(constructor_championship)
summary(constructor_championship)

# Plotting maximum number of constructor championship
# Bar graph
constructor_championship %>%
  group_by(constructor) %>%
  summarise(Number_of_Championships = n()) %>%
  ggplot(aes(x = reorder(factor(constructor), Number_of_Championships),
             y = Number_of_Championships)) + 
  geom_bar(stat = "identity", aes(fill = Number_of_Championships)) +
  geom_text(aes(label = Number_of_Championships, hjust = -0.5, size = 1)) +
  labs(x = 'Constructors', y = 'Number of Championships',
       title = 'Number of constructor\'s championships') +
  coord_flip()

##
# 2017 top 3 constructors
results3 %>%
  filter(year == 2017) %>%
  group_by(constructor) %>%
  summarise(Total_Points = sum(points)) %>%
  top_n(n = 3, wt = Total_Points)

# Calculating total points for each round by constructors in 2017 
# for top 3 constructors
results3_totalPoints <- results3 %>%
  filter(year == 2017, constructor %in% c('Ferrari', 'Mercedes', 'Red Bull')) %>%
  arrange(round, constructor) %>%
  group_by(constructor, round, race) %>%
  summarise(Total_Points = sum(points))

# Plotting progression of constructors championship for top 3 constructors in 2017
results3_totalPoints %>%
  arrange(round, constructor) %>%
  group_by(constructor) %>%
  mutate(cumsum_points = cumsum(Total_Points)) %>%
  ggplot(aes(x = reorder(factor(race), round), y = cumsum_points, 
             group = constructor, color = constructor)) + 
  geom_line() + 
  geom_point() +
  labs(x = 'Grand Prix Race', y = 'Constructor Championship Points',
       title = 'Constructor\'s championship progression 2017') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##
# Reading drivers data
# Reading Races
drivers <- read.csv("datasets/drivers.csv", sep = ',', header=T, 
                         strip.white = T, na.strings = c("NA","NaN","","?"," "))
str(drivers)
summary(drivers)
head(drivers)

# Joining results, races and drivers
results4 <- inner_join(results2, drivers[,c(-3,-4,-9)], by = "driverId")

str(results4)
summary(results4)
head(results4)


# Looking at records with missing data
sum(is.na(results4$positionOrder))

# Plotting maximum number of wins by drivers for top 50 drivers
# Bar graph
results4 %>%
  filter(positionOrder == 1) %>%
  group_by(driverRef) %>%
  summarise(Number_of_Wins = sum(positionOrder)) %>%
  top_n(n = 50, wt = Number_of_Wins) %>%
  ggplot(aes(x = reorder(factor(driverRef), Number_of_Wins), 
             y = Number_of_Wins)) + 
  geom_bar(stat = "identity", aes(fill = Number_of_Wins)) +
  geom_text(aes(label = Number_of_Wins), hjust = -0.5, size = 3) +
  labs(x = 'Drivers', y = 'Number of Wins',
       title = 'Number of wins by drivers') +
  coord_flip() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1, size = rel(1)))


# Determining drivers champion each year by calculating highest total of points
driver_championship <- results4 %>%
  group_by(year, driverRef) %>%
  summarise(Total_Points = sum(points)) %>%
  top_n(n = 1, wt = Total_Points)

str(driver_championship)
summary(driver_championship)

# Plotting maximum number of drivers championship
# Bar graph
driver_championship %>%
  group_by(driverRef) %>%
  summarise(Number_of_Championships = n()) %>%
  ggplot(aes(x = reorder(factor(driverRef), Number_of_Championships),
             y = Number_of_Championships)) + 
  geom_bar(stat = "identity", aes(fill = Number_of_Championships)) +
  labs(x = 'Driver', y = 'Number of Championships',
       title = 'Number of driver\'s championships') +
  coord_flip()

##
# 2017 top 3 drivers
results4 %>%
  filter(year == 2017) %>%
  group_by(driverRef) %>%
  summarise(Total_Points = sum(points)) %>%
  top_n(n = 3, wt = Total_Points)

# Calculating total points for each round by top 3 drivers in 2017
results4_totalPoints <- results4 %>%
  filter(year == 2017, driverRef %in% c('hamilton', 'vettel', 'bottas')) %>%
  arrange(round, driverRef) %>%
  group_by(driverRef, round, name) %>%
  summarise(Total_Points = sum(points))

# Plotting progression of drivers championship for top 3 drivers in 2017
results4_totalPoints %>%
  arrange(round, driverRef) %>%
  group_by(driverRef) %>%
  mutate(cumsum_points = cumsum(Total_Points)) %>%
  ggplot(aes(x = reorder(factor(name), round), y = cumsum_points, 
             group = driverRef, color = driverRef)) + 
  geom_line() + 
  geom_point() +
  labs(x = 'Grand Prix Race', y = 'Driver\'s Championship Points',
       title = 'Driver\'s championship progression 2017') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))