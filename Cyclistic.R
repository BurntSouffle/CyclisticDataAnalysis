install.packages("tidyverse")
library(tidyverse)

colnames(Divvy_Trips_2019_Q1)
colnames(Divvy_Trips_2019_Q2)
colnames(Divvy_Trips_2019_Q3)
colnames(Divvy_Trips_2019_Q4)

Q2_cleaned <- Divvy_Trips_2019_Q2 %>%
  rename(
    trip_id = "01 - Rental Details Rental ID",
    start_time = "01 - Rental Details Local Start Time",
    end_time = "01 - Rental Details Local End Time",
    bikeid = "01 - Rental Details Bike ID",
    tripduration = "01 - Rental Details Duration In Seconds Uncapped",
    from_station_id = "03 - Rental Start Station ID",
    from_station_name = "03 - Rental Start Station Name",
    to_station_id  = "02 - Rental End Station ID",
    to_station_name = "02 - Rental End Station Name",
    usertype = "User Type",
    gender  = "Member Gender",
    birthyear = "05 - Member Details Member Birthday Year" 
  )
glimpse(Q2_cleaned)
remove(Divvy_Trips_2019_Q2)

H1_Trips <- rbind(Divvy_Trips_2019_Q1, Q2_cleaned)
remove(Divvy_Trips_2019_Q1)
remove(Q2_cleaned)
H2_Trips <- rbind(Divvy_Trips_2019_Q3, Divvy_Trips_2019_Q4)
remove(Divvy_Trips_2019_Q3)
remove(Divvy_Trips_2019_Q4)

Total_Trips <- rbind(H1_Trips, H2_Trips)
remove(H1_Trips)
remove(H2_Trips)

glimpse(Total_Trips)

Sample_Trips <- sample_n(Total_Trips, 30000)

#identify trends between bike usage between casual riders and members
#average tripduration of customer vs subriber
Avg_Cus_Tripduration <- filter(Total_Trips, usertype == "Customer") %>%
  drop_na() %>%
  group_by(usertype) %>%
  summarise(mean(tripduration), min(tripduration), max(tripduration))

Avg_Sub_Tripduration <- filter(Total_Trips, usertype == "Subscriber") %>%
  drop_na() %>%
  group_by(usertype) %>%
  summarise(mean(tripduration), min(tripduration), max(tripduration))

Avg_Trip_Dur <- rbind(Avg_Cus_Tripduration, Avg_Sub_Tripduration)

df <- data.frame(usertype = c("Customer", "Subsricber"),
                 mean_dur = c(2869.2567, 858.4842))
df_base <- ggplot(data = df, aes(x = usertype, y = mean_dur, fill=usertype))

df_base + geom_bar(stat = "identity") +
  labs(x = "User Type", y = "Average Trip Duration", title = "Average Trip Duration Of Subscribers Vs Customers") +
  theme(legend.position = "none")

#table with station name, count of customers, count of subscribers

#histogram showing usage of customers vs subscibers
df_his <- filter(Total_Trips, usertype == "Customer") %>%
  select(start_time)

df_his_1 <- data.frame(round_date(df_his$start_time, "minute")) %>%
  rename("start_time" = "round_date.df_his.start_time...minute..")

df_his2 = df_his_1 %>% mutate(start_time = strftime(df_his_1$start_time, tz = "GMT", format = "%H:%M:%S"))

cus_his <- ggplot(df_his2, aes(x = start_time)) + geom_histogram(stat = "count") 

cus_his + labs(x = "Time", y = "Count", title = "Histogram Showing Peak Times For Bike Usage For Customers")

his_sub <- filter(Total_Trips, usertype == "Subscriber") %>%
  select(start_time)
colnames(his_sub)
his_sub = his_sub %>%
  rename("start_time" = "round_date.his_sub.start_time...minutes..")

his_sub_2 = his_sub %>%
  mutate(start_time = strftime(his_sub$start_time, tz = "GMT", format = "%H:%M:%S"))

ggplot(his_sub_2, aes(x = start_time)) + geom_histogram(stat = "count") +
  labs(x = "Time", y = "Count", title = "Histogram Showing Peak Times For Bike Usage For Subscribers")


#Bar with with age groups and count for subs and cust

age_group <- Total_Trips %>%
  drop_na() %>%
  mutate(age = 2021 - birthyear) %>%
  select(usertype, age)

age_group$age <- cut(age_group$age,
                     breaks = c(-Inf,
                       18,25,35,45,55,65,
                     Inf),
                     labels = c("0-17 years",
                       "18-24 years", "25-34 years", "35-44 years", "45-54 years","55-64 years",
                     "65+ years"),
                     right = FALSE)
age_bar <- ggplot(data = age_group, aes(x = age)) + geom_bar()
age_bar + facet_wrap(~usertype) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = "Age Groups", y = "Count", title = "Bar Chart Of Age Distribution Between Customer Vs Subscriber")

#most popular stations, facet grid subscirbers vs customer
df_stations <- Total_Trips %>% 
  group_by(from_station_name, usertype) %>%
  count(from_station_name)

ggplot(df_stations, aes(x = from_station_name, y = n, fill = usertype)) + geom_bar(stat = "identity")

#top 5 stations for customers and subscribers 
top_5 <- data.frame(tail(sort(df_stations$n), 5))

stations_cus <- filter(df_stations, usertype == "Customer")

stations_cus <- stations_cus %>% 
  arrange(n) %>% 
  tail(5)

stations_sub <- filter(df_stations, usertype == "Subscriber")

stations_sub <- stations_sub %>%
  arrange(n) %>%
  tail(5)

c_st_plot <- ggplot(stations_cus, aes(x = from_station_name, y = n)) + geom_bar(stat = "identity")
c_st_plot + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "Station Name", y = "Count", title = "Top 5 Most Popular Stations For Customers")


s_st_plot <-ggplot(stations_sub, aes(x = from_station_name, y = n)) + geom_bar(stat = "identity")
s_st_plot + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(x = "Station Name", y = "Count", title = "Top 5 Most Popular Stations For Subscirbers")