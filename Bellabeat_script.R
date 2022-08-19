install.packages("tidyverse")
library(tidyverse)

install.packages("lubridate")
library(lubridate)

installed.packages("dplyr")
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("tidyr")
library(tidyr)


library(readr)

daily_activity <- read.csv("Fitabase/dailyActivity_merged.csv")
view(daily_activity)

daily_calories <- read.csv("Fitabase/dailyCalories_merged.csv")
view(daily_calories)

hourly_calories <- read.csv("Fitabase/hourlyCalories_merged.csv")
view(hourly_calories)

hourly_intensities <- read.csv("Fitabase/hourlyIntensities_merged.csv")
view(hourly_intensities)

sleep_day <- read.csv("Fitabase/sleepDay_merged.csv")
view(sleep_day)


weight_log <- read.csv("Fitabase/weightLogInfo_merged.csv")
view(weight_log)

daily_steps <- read.csv("Fitabase/dailySteps_merged.csv")
view(daily_steps)


head(daily_activity)
head(daily_calories)

# Identifying the number of user in each dataset with the IDs

# daily_activuty

n_distinct(daily_activity$Id)

# daily_calories

n_distinct(daily_calories$Id)

# hourly_calories

n_distinct(hourly_calories$Id)

# hourly_intensity

n_distinct(hourly_intensities$Id)

# sleep_day

n_distinct(sleep_day$Id)

# weight_log 

n_distinct(weight_log$Id) # 8 participants is not enough significant to make any recommendation based on this particular data.

# look at the statistical summary of the data sets

head(daily_calories)

# daily activity

daily_calories %>% 
  select(Calories) %>% 
  summary()

# daily activity
daily_activity %>% 
  select(TotalDistance,
         TotalSteps,
         SedentaryMinutes) %>% 
  summary()

colnames(sleep_day)

sleep_day %>% 
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>% 
  summary()

# Formating the date

daily_activity <- daily_activity %>% 
  rename(date = ActivityDate) %>% 
  mutate(date = as_date(date, format = "%m/%d/%y"))

daily_calories <- daily_calories %>% 
  rename(date = ActivityDay) %>% 
  mutate(date = as_date(date, format = "%m/%d/%y"))


hourly_calories <- hourly_calories %>% 
  rename(date = ActivityHour) %>% 
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S ", tz = Sys.timezone()))

hourly_intensities <- hourly_intensities %>% 
  rename(date = ActivityHour) %>% 
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S ", tz = Sys.timezone()))

sleep_day <- sleep_day %>% 
  rename(date = SleepDay) %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

weight_log <- weight_log %>% 
  rename(date = Date) %>% 
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S ", tz = Sys.timezone()))
  

daily_steps <- daily_steps %>% 
  rename(date = ActivityDay) %>% 
  mutate(date = as_date(date, format = "%m/%d/%y"))

# Merge the daily_activity and the sleep_day data set

merged_activeandsleep <- merge(sleep_day, daily_activity)

merged_actistep <- merge(daily_activity, daily_steps, by = c("Id", "date"))


# type of users per activity level
daily_activity %>% 
  select(Id,
         date,
         TotalSteps,
         TotalDistance,
         VeryActiveDistance,
         ModeratelyActiveDistance,
         LightActiveDistance,
         SedentaryMinutes,
         Calories) %>% 
  summary()


weekday_active <- daily_activity %>% 
  mutate(weekday = weekdays(date))

weekday_active$weekday < ordered(weekday_active$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                  "Friday", "Saturday", "Sunday")) 

weekday_activetab <- weekday_active %>% 
  group_by(weekday) %>% 
  summarize(daily_steps = mean(TotalSteps), daily_active = mean(VeryActiveMinutes))

head(weekday_activetab)

weekday_activetab %>% 
  
  ggplot(data = weekday_activetab)+
  (aes(x="", y=daily_active, fill=weekday))+
  geom_bar(stat = "identity", width=1)+
  coord_polar("y", start = 0)+
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 14, face="bold"))+
  scale_fill_manual(values = c("#808080","#800000","#808000","#008000","#800080","#008080","#000080"))+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  labs(title="Active week day")

ggplot(data=weekday_activetab)+
  geom_col(mapping=aes(x=weekday, y=daily_active, fill=weekday))+
  labs(title="Active days")

ggplot(data=weekday_activetab)+
  geom_col(mapping=aes(x=weekday, y=daily_active, fill=weekday))+
  labs(title="Active days")  

# Sleep day summary

weekday_sleep <- sleep_day %>% 
  mutate(weekday = weekdays(date))

weekday_active$weekday < ordered(weekday_active$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                  "Friday", "Saturday", "Sunday")) 



weekday_sleeptab <- weekday_sleep %>% 
  group_by(weekday) %>% 
  summarize(totalMinuteAsleep = mean(TotalMinutesAsleep), timeInBed = mean(TotalTimeInBed))

arrange(weekday_sleeptab, desc(totalMinuteAsleep))

head(weekday_sleeptab)




ggplot(data=weekday_sleeptab)+
  geom_line(mapping=aes(x=weekday, y=totalMinuteAsleep, group=0))+
  labs(title="Sleep days")


ggplot(data = weekday_sleeptab)+
  geom_bar(mapping=aes(x=totalMinuteAsleep,fill=weekday))




merged_activeandsleep <- merge(weekday_activetab, weekday_sleeptab)





















































































































