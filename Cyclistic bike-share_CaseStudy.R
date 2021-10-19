library(tidyverse) #helps wrangle data
library(lubridate) #helps wrangle date attributes
library(ggplot2) #helps visualize data
getwd() #displays your working directory
path <- "E:/Salva David/Histormatica/Data Analysis/Google Data Analytics Professional Certificate/C08_Complete a Capstone Project/Case Study 1"
setwd(path) #sets your working directory to simplify calls to data

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload Divvy datasets (csv files) here
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Rename columns to make them consistent with q1_2020 
#(as this will be the supposed going-forward table design for Divvy)

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,trip_duration = tripduration
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
                   ,user_gender = gender
                   ,birth_year = birthyear))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,trip_duration = tripduration
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
                   ,user_gender = gender
                   ,birth_year = birthyear))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "01 - Rental Details Rental ID"
                   ,rideable_type = "01 - Rental Details Bike ID"
                   ,started_at = "01 - Rental Details Local Start Time"
                   ,ended_at = "01 - Rental Details Local End Time"
                   ,trip_duration = "01 - Rental Details Duration In Seconds Uncapped"
                   ,start_station_name = "03 - Rental Start Station Name"
                   ,start_station_id = "03 - Rental Start Station ID"
                   ,end_station_name = "02 - Rental End Station Name"
                   ,end_station_id = "02 - Rental End Station ID"
                   ,member_casual = "User Type"
                   ,user_gender = "Member Gender"
                   ,birth_year = "05 - Member Details Member Birthday Year"))

# Add a "trip_duration" (in seconds) calculation to q1_2020 
# and convert it from Factor to numeric
q1_2020$trip_duration <- difftime(q1_2020$ended_at,q1_2020$started_at)
q1_2020$trip_duration <- as.numeric(as.character(q1_2020$trip_duration))
is.numeric(q1_2020$trip_duration)


# Inspect the dataframes and look for incongruencies
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)

# Convert ride_id and rideable_type to character so that they can stack correctly
q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)


# Remove lat and long fields (I, David Mourlot, decided to keep trip_duration, 
# user_gender and birth_year, in hope that they might yield some insights)
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng))
getwd()
write.csv(all_trips,"./all_trips_01.csv", row.names = FALSE)

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

# Load joined dataset (we don't need to go over all the steps again)
all_trips <- read_csv("all_trips_01.csv")

# Inspect the new table that has been created
dim(all_trips) #Dimensions of the data frame?
colnames(all_trips) #List of column names
head(all_trips) #See the first 6 rows of data frame.
tail(all_trips) #See the last 6 rows of data frame.
str(all_trips) #See list of columns and data types (numeric, character, etc)
summary(all_trips) #Statistical summary of data. Mainly for numerics


# Bad and suspicious data

#filter(all_trips, birth_year == 1759)

#Are there trips with a duration of less than 0 seconds? (Yes, there are)
View(filter(select(all_trips, c(started_at,ended_at, trip_duration)), trip_duration < 0))
#Are there trips with a duration of less than 5 minutes? (Yes, there are)
View(filter(select(all_trips, c(started_at,ended_at, trip_duration)), trip_duration < 60 & trip_duration > 0))
#Are there trips with a duration greater than 24h? (Yes, there are)
View(filter(select(all_trips, c(started_at,ended_at, trip_duration)), trip_duration > 86400))


# Reassign to the desired values 
# (we will go with the current 2020 labels: 'member' and 'casual')
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# how many observations fall under each usertype
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride (started)
# This will allow us to aggregate ride data for each month, day, or year... 
# before completing these operations we could only aggregate at the ride level

all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")


# check that the columns were created correctly
select(all_trips, c(started_at, date, month, day, year, day_of_week))


# Remove "bad" data
# Remove entries when bikes were taken out of docks and checked for quality. 
# Remove outliers: trip_duration negative, less than 1 minute or greater than 24h. 
# (This cases are suspicious and, fortunately, rare: roughly 0.24% of total rows)
all_trips_v2 <- subset(all_trips, start_station_name != "HQ QR" 
                       & trip_duration > 60
                       & trip_duration < 86400)

dim(all_trips)
dim(all_trips_v2)

# save second version of dataset as csv
write.csv(all_trips_v2,"./all_trips_02.csv", row.names = FALSE)

#==============================================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS AND BASIC VISUALIZATIONS
#==============================================================

# Load cleaned dataset (so we don't need to go over all the steps again)
all_trips_v2 <- read_csv("all_trips_02.csv")

all_trips_v2 <- subset(all_trips_v2, start_station_name != "HQ QR" 
                       & trip_duration > 60
                       & trip_duration < 86400)

# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$trip_duration)

# Compare members and casual users'  trip duration
aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$trip_duration ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)
# ------------------------------------------------------------------------------
# Visualize number of rides by rider type and day of the week
rides_by_wday <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n())

# Compute daily proportions for members and casual users
rides_by_wday$proportions <- 0
rides_by_wday$proportions[1:7] <- rides_by_wday$number_of_rides[1:7]/sum(rides_by_wday$number_of_rides[1:7])
rides_by_wday$proportions[8:14] <- rides_by_wday$number_of_rides[8:14]/sum(rides_by_wday$number_of_rides[8:14])

ggplot(data = rides_by_wday, aes(x = weekday, y = proportions, fill = member_casual)) +
  geom_col(position = "dodge") +
  facet_wrap(~member_casual) +
  guides(fill= "none") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(name="User Type") +
  labs(
    title = "Rides by day of the week",
    x = "",
    y = "",
  ) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size= .05, color="white" ) 
  )
  
# ------------------------------------------------------------------------------

# Visualize number of rides by rider type and time of day
all_trips_v2 %>%
  mutate(time_of_day = hour(started_at) + minute(started_at)/60) %>%
  ggplot(aes(x=time_of_day, fill = member_casual)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~member_casual) +
  guides(fill= "none") +
  scale_fill_discrete(name="User Type") +
  labs(
    title = "Ride distribution by time of day",
    x = "Time of day\n(binwidth = 1 hour)",
    y = "Number of rides",
  ) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="white" ) 
  )
# ------------------------------------------------------------------------------

# What percentage of rides do members and casual users buy each month?
rides_by_month <- all_trips_v2%>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n())

rides_by_month$proportions <- 0
rides_by_month$proportions[1:12] <- rides_by_month$number_of_rides[1:12]/sum(rides_by_month$number_of_rides[1:12])
rides_by_month$proportions[13:24] <- rides_by_month$number_of_rides[13:24]/sum(rides_by_month$number_of_rides[13:24])

ggplot(data = rides_by_month, aes(x = month, y = proportions, fill = member_casual)) +
  geom_col(position = "dodge") +
  facet_wrap(~member_casual) +
  scale_y_continuous(labels = scales::percent) +
  guides(fill= "none") +
  labs(
    title = "Rides by month",
    x = "",
    y = "",
  ) +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color="white" ) 
  )


# ------------------------------------------------------------------------------

# LOOK FOR DIFFERENCES IN STATION USE AND POPULARITY

#install.packages("igraph")
library("igraph")

# Which stations are most frequented or used by casual users and members respectively?


# Create node list from station_id columns
nodes <- as.data.frame(unique(c(unique(all_trips_v2$start_station_id),
                                 unique(all_trips_v2$end_station_id))))
names(nodes) <- c("station")
nodes <- filter(nodes, station != 'NA') #remove NAs
View(nodes)


# Create edge list for casual users' rides
edges_casual <- all_trips_v2 %>%
  filter(member_casual == 'casual') %>%
  filter(start_station_id != 'NA' & end_station_id != 'NA') %>%
  group_by(start_station_id, end_station_id) %>%
  summarise(weight = n())
head(edges_casual)

# Use igraph package to create the graph (network) of casual users' station use
casual_nw <- graph_from_data_frame(d = edges_casual, vertices = nodes, directed = TRUE)

# Compute number of in and out rides as a rough measure of how "popular"/visited
# each station is.
casual_df <- nodes
casual_df$popularity <- strength(casual_nw)
# Normalize popularity
total_casual_rides <- length(filter(all_trips_v2, member_casual == 'casual')$ride_id)
casual_df$popularity_norm <- casual_df$popularity/total_casual_rides

# Compute In-Degree
casual_df$in_degree <- degree(casual_nw, mode="in", normalized = TRUE)

# betweenness centrality is a measure of centrality in a graph based on shortest
# paths. The betweenness centrality for each vertex is the number of these shortest
# paths that pass through the vertex. It represents the degree to which nodes 
# stand between each other. For example, in a telecommunications network,
# a node with higher betweenness centrality would have more control over the network,
# because more information will pass through that node. 
# (https://en.wikipedia.org/wiki/Betweenness_centrality)
# Betweenness centrality has another major role-it is able to identify boundary spanners
# -people that act as bridges between two or more communities that otherwise would
# not be able to communicate to each other.
casual_df$betweenness <- betweenness(casual_nw, normalized = TRUE)

# Compute a weighted sum of the three metrics
casual_df <- casual_df %>%
  mutate(centrality = popularity_norm*0.45 + in_degree*0.35 + betweenness*0.2)
View(casual_df)

# Use a bubble plot to visualize station popularity among casual users 

top_casual <- arrange(casual_df, desc(centrality))[1:15,]
View(top_casual)
casual_df %>%
  ggplot(aes(x = betweenness, y = in_degree, size = popularity)) +
  geom_point(alpha=0.3) +
  # Highlight Top central stations only
  geom_point(data = top_casual,
             aes(x = betweenness, y = in_degree, size = popularity),
             color= 'red',
             alpha=0.6) +
  geom_text(data = top_casual,
            aes(label=station),
            hjust=0,
            vjust=0,
            alpha=0.8)+
  labs(
    title = "Most popular stations - Casual Users",
    x = "Betweenness",
    y = " In-Degree",
  )
  

# Create edge list for members' rides
edges_member <- all_trips_v2 %>%
  filter(member_casual == 'member') %>%
  group_by(start_station_id, end_station_id) %>%
  filter(start_station_id != 'NA' & end_station_id != 'NA') %>%
  summarise(weight = n())
head(edges_member)

# Use igraph package to create the graph (network) of members' station use
member_nw <- graph_from_data_frame(d = edges_member, vertices = nodes, directed = TRUE)

# Compute Centrality Measures
member_df <- nodes
member_df$popularity <- strength(member_nw)         # Compute number of in and out rides as a rough measure
                                                    #  of how "popular"/visited each station is.
# Normalize popularity
total_member_rides <- length(filter(all_trips_v2, member_casual == 'member')$ride_id)
member_df$popularity_norm <- member_df$popularity/total_member_rides 
member_df$in_degree <- degree(member_nw, mode="in",
                              normalized = TRUE)    # number of stations with rides TO a specific station
member_df$betweenness <- betweenness(member_nw,
                                     normalized = TRUE) # boundary spanners -bridges between two or more communities 
                                                        # that otherwise would not be able to communicate to each other.
# Compute a a weighted sum of the three metrics,
# assigning less importance (weight) to betweenness
member_df <- member_df %>%
  mutate(centrality = popularity_norm*0.45 + in_degree*0.35 + betweenness*0.2)


View(member_df)

# Use a bubble plot to visualize station popularity among members 
top_member <- arrange(member_df, desc(centrality))[1:15,]
member_df %>%
  ggplot(aes(x = betweenness, y = in_degree, size = popularity)) +
  geom_point(alpha=0.3) +
  # Highlight Top central stations only
  geom_point(data = top_member,
             aes(x = betweenness, y = in_degree, size = popularity),
             color= 'green',
             alpha=0.6) +
  geom_text(data = top_member,
            aes(label=station),
            hjust=0,
            vjust=0,
            alpha=0.8)+
  labs(
    title = "Most popular stations - Members",
    x = "Betweenness",
    y = " In-Degree",
  )

top_casual$station
top_member$station
top_casual$station[top_casual$station %in% top_member$station]
# ------------------------------------------------------------------------------
#The following commands will install DescTools and  PropCIs packages
if(!require(DescTools)){install.packages("DescTools")}
if(!require(PropCIs)){install.packages("PropCIs")} 

library("DescTools")
library("PropCIs")


# LOOK FOR DIFFERENCES IN AGE
# create an age column 
# (OJO!! esto tiene que ir en la parte superior, donde se hacen las modificaciones a los datos)
all_trips_v2 <- all_trips_v2 %>%
  mutate(user_age =  year - birth_year)

# replace (hundreds of) instances with age greater than 100 
# (since they are a little too suspicious)
all_trips_v2$user_age[all_trips_v2$user_age > 100] <- NA


# Compare members and casual users'  ages
aggregate(all_trips_v2$user_age ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$user_age ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$user_age ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$user_age ~ all_trips_v2$member_casual, FUN = min)

# Calculate confidence intervals for age means of both members and casual users
casual_ages <- filter(all_trips_v2, member_casual == 'casual' & user_age != "NA")$user_age
MeanCI(casual_ages,
       conf.level=0.99)
member_ages <- filter(all_trips_v2, member_casual == 'member' & user_age != "NA")$user_age
MeanCI(member_ages,
       conf.level=0.99)

all_trips_v2 %>%
  filter(member_casual == "casual" & user_age != 'NA')%>%
  ggplot(aes(x=user_age)) + 
  geom_histogram(aes(y = stat(count) / sum(count)), binwidth = 10, fill = "blue") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Casual users' ride distribution by age",
    x = "Age (binwidth = 10)",
    y = "Percentage of rides",
    caption = "Percentage of rides purchased by casual users in each of the 10-year age bands"
  )

all_trips_v2 %>%
  filter(member_casual == "member" & user_age != 'NA')%>%
  ggplot(aes(x=user_age)) + 
  geom_histogram(aes(y = stat(count) / sum(count)), binwidth = 10, fill = "green") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Member's ride distribution by age",
    x = "Age (binwidth = 10)",
    y = "Percentage of rides",
    caption = "Percentage of rides purchased by members in each of the 10-year age bands",
  )

# ------------------------------------------------------------------------------

# LOOK FOR DIFFERENCES IN GENDER

#Calculate gender proportions (and confidence intervals) for both members and casual users  

# compute male-female counts for members and casual users respectively
gender_props <- all_trips_v2 %>%
  filter(user_gender != "NA") %>%
  group_by(member_casual, user_gender) %>%
  summarise(count = n())


# compute proportion estimates and confidence intervals for causal users
casual_cis <- as.data.frame(BinomCI(gender_props$count[1:2],
                      sum(gender_props$count[1:2]),
                      conf.level = 0.99,
                      method = "clopper-pearson")) #use most conservative method

# compute proportion estimates and confidence intervals for members
member_cis<- as.data.frame(BinomCI(gender_props$count[3:4],
                                   sum(gender_props$count[3:4]),
                                   conf.level = 0.99,
                                   method = "clopper-pearson"))

gender_cis <- rbind(casual_cis,member_cis) #merge into one dataframe

# add relevant columns to our gender dataframe
gender_props$prop_estimate <- gender_cis$est
gender_props$lower_ci <- gender_cis$lwr.ci
gender_props$upper_ci <- gender_cis$upr.ci
gender_props

# plot proportions with error bars
ggplot(gender_props, aes(x = member_casual, y = prop_estimate, fill = user_gender)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(
    title = "Male-Female proportions by user type",
    subtitle = "With error bars computed at a 99% confidence level",
    x = "User Type",
    y = "Percentage of rides",
    caption = "Percentage of rides purchased by men/women within each user category",
  )

# Finally, calculate confidence intervals for difference in proportions
diff_prop_female <- diffscoreci(gender_props$count[1], sum(gender_props$count[1:2]),
                                gender_props$count[3], sum(gender_props$count[3:4]),
                                conf.level=0.99)

diff_prop_female # CI is the same for males, only with an inverted sign
