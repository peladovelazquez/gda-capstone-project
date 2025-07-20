# graphs

raw <- bike_df %>%
  group_by(member_casual) %>% 
  group_by(member_casual, month(started_at, label = TRUE)) %>% 
  summarize(number_of_trips = n(), .groups = "drop") %>% 
  left_join(bike_df %>% 
              group_by(member_casual) %>%
              summarize(trips_by_client = n(), .groups = "drop"), 
            by = "member_casual") %>%  # Unimos el total de viajes por cliente
  mutate(percentage_of_total = (number_of_trips / trips_by_client) * 100) %>% 
  arrange(member_casual, percentage_of_total)


# bar graph showing % of use by month, faceted and with curved line
ggplot(data=raw, aes(x = `month(started_at, label = TRUE)`, y = percentage_of_total, fill = member_casual)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  labs(title = "Percentage Of Bike Rides By Month", 
       subtitle= "Comparison between casual riders and annual members",
       fill="type of user",
       x = "month", 
       y = "percentage of rides (%)") + facet_wrap(~member_casual) + geom_smooth(method = "loess", aes(group = member_casual),show.legend = FALSE, se=FALSE) +
  theme_minimal() +
  theme(axis.title.x = element_text(margin = margin(t=20)), axis.title.y = element_text(margin = margin(r=20)))

ggsave("images/graph_percentage_by_month.png", plot = last_plot(), width = 10, height = 6, units = "in")
  

# bar graph showing nr of rides by day of week with curved line
by_day_df <- bike_df %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_trips = n(), .groups = "drop") %>% 
  arrange(desc(number_of_trips)) 

ggplot(data=by_day_df, aes(x=day_of_week, y=number_of_trips, fill=member_casual, group = member_casual)) + 
  geom_bar(stat = "identity") + geom_smooth(method = "loess", se=FALSE, show.legend = FALSE) +
  facet_wrap(~member_casual) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) +
  labs(title = "Number Of Bike Rides By Day Of The Week", subtitle="Comparison between casual riders and annual members", 
       y="number of rides", x="day of the week", fill="type of user") +
  theme_minimal() + 
  theme(axis.title.x = element_text(margin=margin(t=20)), axis.title.y = element_text(margin = margin(r=20)))

ggsave("images/graph_rides_by_day_of_week.png", plot = last_plot(), width = 10, height = 6, units = "in")  

# save the last bar graph as a png file
ggsave("graph_usage_by_month.png", plot = last_plot())

#isolate september so i can then show the value in the graph using geom_text
sep_point <- raw %>% filter(`month(started_at, label = TRUE)` == "Sep")  

# graph showing nr of rides per month
ggplot(data = raw, aes(x=`month(started_at, label = TRUE)`, y=number_of_trips, color= member_casual, group = member_casual)) + 
  geom_point() + geom_line() + scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) + 
  geom_text(data = sep_point, aes(label = number_of_trips), vjust=-1, show.legend = FALSE) +
  labs(x="month", y="number of rides", title = "Number Of Bike Rides By Month", subtitle= "Comparison between casual riders and annual members",color="type of user") +
  theme_minimal() +
  theme(axis.title.x = element_text(margin = margin(t=20)), axis.title.y = element_text(margin = margin(r= 20)))
  
#save graph with specific size
ggsave("images/graph_number_of_rides_by_month.png", plot = last_plot(), width = 10, height = 6, units = "in")


## -------------------- 
## graph number of rides by hour of the day
test <- bike_df

df_by_hour <- test %>%
  mutate(hour = hour(started_at)) %>%     # Extrae la hora (0-23)
  group_by(hour, member_casual) %>%
  summarise(trips = n(), .groups = "drop")  # Cuenta viajes por hora

# Paso 2: Asegurar que el eje X tenga todas las horas (0 a 23)
df_by_hour <- df_by_hour %>%
  complete(hour = 0:23, fill = list(trips = 0))  # Rellena horas faltantes con 0

# Paso 3: Graficar
ggplot(df_by_hour, aes(x = hour, y = trips, fill = member_casual)) +
  geom_col() +
  scale_x_continuous(breaks = 0:23) +  # Mostrar todas las horas en X
  labs(x = "hour of the day", y = "number of bike rides", 
       title = "Number Of Bike Rides By Hour Of The Day",
       fill="type of user",
       subtitle = "Comparison between casual riders and annual members") +
  theme_minimal() + facet_wrap(~member_casual) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k")) + geom_line(color="blue")+
  theme(axis.title.x = element_text(margin = margin(t=20)), axis.title.y = element_text(margin = margin(r=20)))

ggsave("images/graph_use_by_hour_of_the_day.png", plot = last_plot(), width = 10, height = 6, units = "in")


# ------------
# preferences by type of bike 

bike_type_summary <- bike_df %>%
  mutate(bike_type = case_when(
    bike_type == "electric_bike" ~ "Electric Bike",
    bike_type == "classic_bike" ~ "Classic Bike",
    bike_type == "electric_scooter" ~ "Electric Scooter",
    TRUE ~ bike_type
  )) %>%
  mutate(bike_type = factor(bike_type, levels = c("Electric Bike", "Classic Bike", "Electric Scooter"))) %>%
  group_by(member_casual, bike_type) %>%
  summarize(total_rides = n(), .groups = "drop")

ggplot(bike_type_summary, aes(x = bike_type, y = total_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Total Rides by Bike Type and User Type",
    x = "bike type",
    y = "number of rides",
    fill = "user type"
  ) +
  theme_minimal() +
  theme(axis.title.x = element_text(margin = margin(t=20)), axis.title.y = element_text(margin = margin(r=20))) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"))
  

ggsave("images/graph_use_by_type_of_bike.png", plot = last_plot(), width = 10, height = 6, units = "in")


       