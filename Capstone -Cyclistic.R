
#Install the packages I will need to use
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("lubridate")
install.packages("chron")


#load the library of the packages I installed
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(chron)


#Load the last 12 months of data that I previously worked with from Excel. These will have the 13 original columns, 
#plus the two additional that were added for the project, ride_length and day_of_week
June2022_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202206_Tripdata.xlsx")
July2022_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202207_Tripdata.xlsx")
August2022_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202208_Tripdata.xlsx")
September2022_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202209_Tripdata.xlsx")
October2022_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202210_Tripdata.xlsx")
November2022_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202211_Tripdata.xlsx")
December2022_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202212_Tripdata.xlsx")
January2023_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202301_Tripdata.xlsx")
February2023_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202302_Tripdata.xlsx")
March2023_Trip <- read_excel("Cyclistic Data/XCEL Sheets/202303_Tripdata.xlsx")
April2023_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202304_Tripdata.xlsx")
May2023_Tripdata <- read_excel("Cyclistic Data/XCEL Sheets/202305_Tripdata.xlsx")

#Combine all the months into one dataframe
yearly_data_raw <-rbind(June2022_Tripdata, July2022_Tripdata, August2022_Tripdata, September2022_Tripdata, October2022_Tripdata,
                        November2022_Tripdata, December2022_Tripdata, January2023_Tripdata, February2023_Tripdata, March2023_Trip,
                        April2023_Tripdata, May2023_Tripdata)

#Create a dataframe for each season to easily analyze data based on season
spring_data <- rbind(March2023_Trip, April2023_Tripdata, May2023_Tripdata)
summer_data <-rbind(June2022_Tripdata, July2022_Tripdata, August2022_Tripdata)
fall_data <- rbind(September2022_Tripdata, October2022_Tripdata, November2022_Tripdata)
winter_data <-rbind(December2022_Tripdata, January2023_Tripdata, February2023_Tripdata)

################################################################################################################################################################################################################################################
#What kind of bike is used by members and casual members? Find this out by filtering out all but bike type and member type then counting what is left.
yearly_rideable_types = data.frame(yearly_data_raw$rideable_type, yearly_data_raw$member_casual)

member_electric_bike <- yearly_rideable_types %>% subset(yearly_data_raw$member_casual == "member" & yearly_data_raw$rideable_type =="electric_bike") %>% nrow()
member_docked_bike <- yearly_rideable_types %>% subset(yearly_data_raw$member_casual == "member" & yearly_data_raw$rideable_type =="docked_bike") %>% nrow()
member_classic_bike <- yearly_rideable_types %>% subset(yearly_data_raw$member_casual == "member" & yearly_data_raw$rideable_type =="classic_bike") %>% nrow()

casual_electric_bike <- yearly_rideable_types %>% subset(yearly_data_raw$member_casual == "casual" & yearly_data_raw$rideable_type =="electric_bike") %>% nrow()
casual_docked_bike <- yearly_rideable_types %>% subset(yearly_data_raw$member_casual == "casual" & yearly_data_raw$rideable_type =="docked_bike") %>% nrow()
casual_classic_bike <- yearly_rideable_types %>% subset(yearly_data_raw$member_casual == "casual" & yearly_data_raw$rideable_type =="classic_bike") %>% nrow()


#put the values into dataframes for visualization
annual_member_bike_types <- data.frame(
  bike_type = c("Electric", "Docked", "Classic"),
  count = c(member_electric_bike, member_docked_bike, member_classic_bike)
)

annual_casual_bike_types <-data.frame(
  bike_type = c("Electric", "Docked", "Classic"),
  count = c(casual_electric_bike, casual_docked_bike, casual_classic_bike)
)

#Create a visualization to show each
ggplot(data=annual_member_bike_types, aes(x="", y=count, fill=bike_type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#ED7D31", "#F4B183", "#F8CBAD")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Membership Bike Type", ) 

ggplot(data=annual_casual_bike_types, aes(x="", y=count, fill=bike_type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values=c("#4472C4", "#8FAADC", "#B4C7E7")) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Casual Bike Type", ) 


################################################################################################################################################################################################
#Which season has the most casual riders?
spring_casual_riders <- nrow(spring_data[spring_data$member_casual == 'casual', ])
summer_casual_riders <- nrow(summer_data[summer_data$member_casual == 'casual', ])
fall_casual_riders <- nrow(fall_data[fall_data$member_casual == 'casual', ])
winter_casual_riders <- nrow(winter_data[winter_data$member_casual == 'casual', ])

#add this to a df to create a viz
seasonal_casual_riders <- data.frame(
  season=c("Spring", "Summer", "Fall", "Winter"),
  values=c(spring_casual_riders, summer_casual_riders, fall_casual_riders, winter_casual_riders)
)

#create a bar chart to display this data
ggplot(data=seasonal_casual_riders, aes(x=season, y=values))+
  geom_bar(stat="identity", fill = "#4472C4") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Casual Riders per Season")+
  xlab("Season") +
  ylab("Users")



#What season has the most member riders?
spring_member_riders <- nrow(spring_data[spring_data$member_casual == 'member', ])
summer_member_riders <- nrow(summer_data[summer_data$member_casual == 'member', ])
fall_member_riders <- nrow(fall_data[fall_data$member_casual == 'member', ])
winter_member_riders <- nrow(winter_data[winter_data$member_casual == 'member', ])

#add this to a df to create a viz
seasonal_member_riders <- data.frame(
  season=c("Spring", "Summer", "Fall", "Winter"),
  values=c(spring_member_riders, summer_member_riders, fall_member_riders, winter_member_riders)
)

#create a bar chart to display this data
ggplot(data=seasonal_member_riders, aes(x=season, y=values))+
  geom_bar(stat="identity", fill = "#ED7D31") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Member Riders per Season", ) +
  xlab("Season") +
  ylab("Users")


#From this, I can see that the season with the most users, casual and member, is Summer. I will use this to see what 
#stations and what days are most used by casual members.

#What days in summer are casual riders most likely to use the service?
summer_data %>% count(day_of_week)

#With this, Saturday is the day casual riders use the service the most, followed by Wednesday and Friday. 
#The numbers for Friday and Wednesday are very close, so I would choose Friday and Saturday.


#Now that we now what days casual members are more likely to use their service, we should know which stations to 
#put the advertisements at

#export to work more in excel
write.csv(summer_data, "C:\\Users\\Shay\\Documents\\Google Data Analytics\\Capstone- Cyclistic\\Cyclistic Data\\Summer-Data.csv", row.names=FALSE)


  
  
  
  