library(ggplot2)
library(dplyr)
library(pracma)
library(leaflet)
library(magrittr)
library(maps)
library(sp)
library(ggmap)
library(RColorBrewer)

#######################
### Reading in Data ###
#######################

# loaded the citibike data from 12/2015 - 02/2016
dec = read.csv("201512-citibike-tripdata.csv")
jan = read.csv("201601-citibike-tripdata.csv")
feb = read.csv("201602-citibike-tripdata.csv")

cb_df = rbind(dec,jan,feb)

#write.csv(cb_df, file="citibike_dataframe.csv")

#################
### Functions ###
#################

# function for distance taking into account the curvature of earth
cal_dist = function (lat1, lon1, lat2, lon2) {
  dlon = deg2rad(lon2-lon1)
  dlat = deg2rad(lat2-lat1)
  a = (sin(dlat/2))^2 + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) *(sin(dlon/2))^2
  c = 2 * atan2(a^(1/2),(1-a)^(1/2))
  return (3959*c)
}

# function for velocity
vel = function (dist, time) {
  return (dist/time)
}

# Categorizing into Morning, Afternoon, Evening
categorize = function (time) {
  if (time < 12 & time >=4) {
    return ("Morning")
  }
  if (time >= 12 & time < 18) {
    return ("Afternoon")
  }
  if (time >= 18 | time<4) {
    return ("Night")
  }
}

#######################
### Adding features ###
#######################

#create hour and age columns
cb_df = mutate(cb_df, hour = tripduration/3600, age = 2015-birth.year)

#split starttime to date and time
start_df = data.frame(do.call('rbind', strsplit(as.character(cb_df$starttime),' ',fixed=TRUE)))
cb_df$startdate = start_df[,1]
cb_df$starthour = start_df[,2]

#extract hour by splitting the starttime entry
temp_df = data.frame(do.call('rbind', strsplit(as.character(cb_df$starttime),':',fixed=TRUE)))
temporary = data.frame(do.call('rbind', strsplit(as.character(temp_df[,1]), ' ', fixed=TRUE)))
cb_df$timehour = as.numeric(temporary[,2])

#calculate distance
cb_df$distance = mapply(cal_dist, 
                         cb_df$start.station.latitude, 
                         cb_df$start.station.longitude, 
                         cb_df$end.station.latitude, 
                         cb_df$end.station.longitude)

#calculate velocity
cb_df$velocity = mapply(vel,
                         cb_df$distance,
                         cb_df$hour)

#Categorized by time of day
cb_df$timeofday = mapply(categorize,
                         cb_df$timehour)

###########################
### Filtered Dataframes ###
###########################
#Duration less than or equal to 45
cb_45 = filter(cb_df, tripduration <= 2700)

#remove 0
win_df = filter(cb_45, velocity != 0)

#leave 0
zero_df = filter(cb_45, velocity == 0)

#velocity <30
vel_15 = filter(cb_45, velocity < 15)

##############
### Graphs ###
##############

#Histogram for Ride Durations Under an 45 min
ggplot(cb_45, aes(x=velocity)) + 
  geom_histogram(binwidth = .1, fill="darkblue") + 
  theme_bw()+
  labs(title='Velocity (duration<=45 min)',
       x="Velocity (mph)",
       y="Frequency")

#LineGraph for zero_velocity and time of day
ggplot(zero_df, aes(x=timehour, y=hour)) + 
  geom_smooth(aes(color = "darkblue"), se=FALSE) +
  theme_bw()+
  labs(title = "Zero-Velocity",
       x = "Time (24Hr)",
       y = "Duration (hr)")+
  guides(color = "none")

#Bar graph for zero-velocity
ggplot(zero_df, aes(timeofday))+
  geom_bar(fill="darkblue") +
  theme_bw()+
  labs(title = "Zero-Velocity",
       x = "Time of Day",
       y = "Number of Trips")

####################
### Age & Gender ###
####################

# There are ages that exceed 100 but shows that they ride the bikes. 
# Going to set 80 as the cutoff age. Anything higher will be set to NA

avg_age = mean(win_df[win_df$age<=80,]$age, na.rm=T)
win_df$age=ifelse(is.na(win_df$age), sample(15:80), win_df$age)
win_df$age=ifelse(win_df$age>80, avg_age, win_df$age)

#Age Line Graph
ggplot(win_df, aes(x=age, y=velocity)) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = mean(win_df$age), color='red', linetype="longdash") +
  theme_bw()+ 
  labs(title = "Velocity by Age",
       x = "Age",
       y = "Velocity") +
  scale_x_continuous(breaks=c(seq(0,80, by = 10)))

# boxplot for velocity and time of day
ggplot(vel_15, aes(x=factor(timeofday), y=velocity)) +
  geom_boxplot(fill = c('darkgreen', 'navy', 'red')) +
  theme_bw() +
  labs(title = "Velocity by Time of Day",
       x = "Time of Day",
       y = "Velocity")

# boxplot for gender and velocity
ggplot(vel_15, aes(x=factor(gender), y=velocity)) +
  geom_boxplot(fill = c('grey', 'navy', 'red')) +
  theme_bw() +
  labs(title = "Velocity by Gender",
       x = "Gender",
       y = "Velocity") +
  scale_y_continuous(breaks=c(seq(0,26, by = 4))) +
  scale_x_discrete(labels=c('0' = 'Unknown',
                            '1' = 'Male',
                            '2' = 'Female'))

# UserType Histogram
ggplot(vel_15, aes(x=velocity)) +
  geom_histogram(binwidth = .1, aes(fill=vel_15$usertype)) +
  scale_fill_manual(values = c("red", "darkblue"), 
                    name = "UserType") +
  theme_bw() +
  labs(title = "Velocity by Usertype (<= 45 min)",
       x = "Velocity",
       y = "Frequency")

#############################
### Dataframe for Leaflet ###
#############################

temp = win_df[sample(nrow(win_df), nrow(win_df)/1000), ]
#cols = c("start.station.latitude", "start.station.longitude",
#         "end.station.latitude", "end.station.longitude",
#         "velocity", "timeofday")
#write.csv(temp[cols], file="cb_dataframe.csv")

leaf_df = data.frame(group=c("A","B"),
                     lat = c(temp$start.station.latitude, 
                             temp$end.station.latitude),
                     long = c(temp$start.station.longitude,
                              temp$end.station.longitude))
#map for stations
leaflet(temp) %>%
  setView(lng = -73.98928, lat = 40.75042, zoom = 13) %>%
  addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(
    lng= ~ start.station.longitude,
    lat= ~ start.station.latitude,
    radius = 2,
    stroke=FALSE, # Circle stroke
    fillOpacity=0.2) %>%
  addPolylines(data = leaf_df,lng = ~long, lat = ~lat, group = ~group)

#map for zero velocity
leaflet(zero_df) %>%
  setView(lng = -73.98928, lat = 40.75042, zoom = 12) %>%
  addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(
    lng=zero_df$start.station.longitude,
    lat=zero_df$start.station.latitude,
    radius = 1,
    stroke=FALSE, # Circle stroke
    fillOpacity=0.5
  )