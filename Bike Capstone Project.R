# PREPARE PHASE

july <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202407-divvy-tripdata.csv')
august <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202408-divvy-tripdata.csv')
september <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202409-divvy-tripdata.csv')
october <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202410-divvy-tripdata.csv')
november <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202411-divvy-tripdata.csv')
december <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202412-divvy-tripdata.csv')
january <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202501-divvy-tripdata.csv')
february <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202502-divvy-tripdata.csv')
march <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202503-divvy-tripdata.csv')
april <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202504-divvy-tripdata.csv')
may <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202505-divvy-tripdata.csv')
june <- read_csv('/Users/gonzalovelazquez/Desktop/Google Analytics Certificate/Capstone Project - Bikes/12 months datasets/202506-divvy-tripdata.csv')

bike_df <- bind_rows(july, august, september, october, november, december, january, february, march, april, may, june)


# count number of rows with NA value in started_at column
count(bike_df[is.na(bike_df$started_at), ])
# same with ended_at columns
count(bike_df[is.na(bike_df$ended_at), ])

#count missing and NA values in member_casual column
n_distinct(bike_df$member_casual)

count(bike_df[is.na(bike_df$member_casual), ])

#create a column with time diff between end and start time
bike_df <- bike_df %>% mutate(trip_duration = round(as.numeric(difftime(ended_at, started_at, "mins")), digits = 2))

#create a column for the month and for the week day
bike_df <- bike_df %>% mutate(month = month(started_at,label = TRUE))
              
bike_df <- bike_df %>% mutate(day_of_week = wday(started_at,label = TRUE, week_start = 1))

#security copy of the data frame
df_original <- bike_df

# remove trips less than 1min long and longer than 1440 min
bike_df <- bike_df[!bike_df$trip_duration < 1 & !bike_df$trip_duration > 1440, ]

# create weekend or weekday columns based on day_of_week column
bike_df <- bike_df %>% mutate(type_of_day = ifelse(bike_df$day_of_week %in% c("Sat", "Sun"), "weekend", "weekday"))

