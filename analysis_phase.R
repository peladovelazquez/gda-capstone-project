#count n of rides for casual and members
print(count(bike_df, bike_df$member_casual))

#count avg ride length for casual and members
bike_df %>% 
  group_by(member_casual) %>% 
  summarize(avg_trip_duration = mean(trip_duration))

#count nr of weekend rides for casual and members
bike_df %>% 
  group_by(member_casual, type_of_day) %>%  
  summarize(n(), .groups = "drop")

#avg ride length for casual and members based on day of week
bike_df %>%  
  group_by(member_casual, day_of_week) %>% 
  summarize(avg_trip_duration = mean(trip_duration)) %>% 
  arrange(desc(avg_trip_duration))

#n of rides for casual and member based on day of week sorted by n of rides
bike_df %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_trips = n(), .groups = "drop") %>% 
  arrange(desc(number_of_trips)) 

# n of rides by month for memeber and casual
print(bike_df %>% 
        group_by(member_casual, month(started_at, label = TRUE)) %>% 
        summarize(number_of_trips = n(), .groups = "drop") %>% 
        arrange(member_casual,number_of_trips), n=24)


# % by client by month
print(bike_df %>%
  group_by(member_casual) %>% 
  group_by(member_casual, month(started_at, label = TRUE)) %>% 
  summarize(number_of_trips = n(), .groups = "drop") %>% 
  left_join(bike_df %>% 
              group_by(member_casual) %>%
              summarize(trips_by_client = n(), .groups = "drop"), 
            by = "member_casual") %>%  # Unimos el total de viajes por cliente
  mutate(percentage_of_total = (number_of_trips / trips_by_client) * 100) %>% 
  arrange(member_casual, percentage_of_total), n=24)

#number of rides longer than 30' by client
bike_df %>% 
  filter(trip_duration >= 30) %>% 
  group_by(member_casual, type_of_day) %>% 
  summarize(total = n(), .groups = "drop")

bike_df %>% 
  filter(trip_duration >= 30) %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(total = n()) %>% 
  arrange(total)

#type of bike preferred by user

bike_df %>% group_by(member_casual, bike_type) %>% summarize(type_of_bike = n()) %>%  arrange(member_casual, desc(type_of_bike))




