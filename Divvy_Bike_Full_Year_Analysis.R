#<><><><><><><><><><><> Divvy Bikes Full Year Analysis  <><><><><><><><><><><><>
# Author : Hanani Sidi                                                         
#===============================================================================

# This is a Case study 1 from the Google Data Analytics Certificate (Cyclistic).
# It's originally based on the case study 'Sophisticated, Clear, and Polished’: 
# Divvy and Data Visualization" written by Kevin Hartman
# (found here: https://artscience.blog/home/divvy-dataviz-case-study). 
# The objective of this script is to to consolidate downloaded Divvy data into 
# a single data frame and then conduct simple analysis to help answer the key 
#question:“In what ways do members and casual riders use Divvy bikes differently?”

#=========================
# STEP 0: INSTALL PACKAGES
#=========================

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)    #helps visualize data
library(here)       # helps upload csv files
library(arsenal)    # helps to compare data frames
library(scales)
library(ggpubr)
library(epiDisplay) # helps to makes frequency table
library(gridExtra) # multiple chart on a single page

#getwd() #displays your working directory

#=====================
# STEP 1 : UPLOAD DATA
#=====================

# As we have 12 files to upload, we will separate them in 4 quarters.Every quarter
# contains 3 month trips data.3 quarter from 2020 and 1 quarter from 2021.

#--------
# Q2_2020 
#--------

 May_2020 <- read_csv(here("Case_Study_1_Data","2020_05_trips.csv"))
 Jun_2020 <- read_csv(here("Case_Study_1_Data","2020_06_trips.csv"))
 Jul_2020 <- read_csv(here("Case_Study_1_Data","2020_07_trips.csv"))

#---------
# Q3_2020 
#---------

 Aug_2020 <- read_csv(here("Case_Study_1_Data","2020_08_trips.csv"))
 Sep_2020 <- read_csv(here("Case_Study_1_Data","2020_09_trips.csv"))
 Oct_2020 <- read_csv(here("Case_Study_1_Data","2020_10_trips.csv"))
 
#--------
# Q4_2020 
#--------

# Jan_2021 will be included in the Quarter 4 of 2020 
 Nov_2020 <- read_csv(here("Case_Study_1_Data","2020_11_trips.csv"))
 Dec_2020 <- read_csv(here("Case_Study_1_Data","2020_12_trips.csv"))
 Jan_2021 <- read_csv(here("Case_Study_1_Data","2021_01_trips.csv"))
 
#--------
# Q1_2021
#--------
 
 Feb_2021 <- read_csv(here("Case_Study_1_Data","2021_02_trips.csv"))
 Mar_2021 <- read_csv(here("Case_Study_1_Data","2021_03_trips.csv"))
 Apr_2021 <- read_csv(here("Case_Study_1_Data","2021_04_trips.csv")) 
 
#==============================================================
# STEP 2: WRANGLING AND COMBINING DATA INTO A SINGLE DATA FRAME
#==============================================================

# Comparing column names in each quarter to make sure that all of them perfectly match
# and could be joined into one single data frame.To do so we use "comparedef"
# function to detect any difference

#--------
# Q2_2020 
#--------
# Checking columns consistency 
 May_Var<- data.frame( May_col = c(colnames(May_2020) ))
 Jun_Var<- data.frame(jun_col =  c(colnames(Jun_2020) ))
 Jul_Var<- data.frame( Jul_col =  c(colnames(Jul_2020) ))
 
 comparedf(May_Var,Jun_Var) # Helps to compare column names data frame in Jun and May data
 comparedf(May_Var,Jul_Var) # Helps to compare column names in July and May data
 summary(comparedf(May_Var,Jun_Var)) # Gives a detail summary of the comparison
 summary(comparedf(May_Var,Jul_Var)) # Gives a detail summary of the comparison
 
# Joining all 3 data sets to make a quarter 2 of 2020 (Q2_2020)
#``````````````````````````````````````````````````````````````````````````````
   Q2_2020 <- rbind(May_2020,Jun_2020,Jul_2020 )
   
#--------
# Q3_2020 
#--------
# Checking columns consistency 
#```````````````````````````````````````````````````````````````````````````````
 Aug_Var<- data.frame(Aug_col =  c(colnames(Aug_2020) ))
 Sep_Var<- data.frame(Sep_col =  c(colnames(Sep_2020) ))
 Oct_Var<- data.frame(Oct_col =  c(colnames(Oct_2020) ))
 
 comparedf(Aug_Var,Sep_Var) # compare column names in August and September data
 comparedf(Aug_Var,Oct_Var) # compare column names in August and October data
 summary(comparedf(Aug_Var,Sep_Var)) # Gives a detail summary of the comparison
 summary(comparedf(Aug_Var,Oct_Var)) # Gives a detail summary of the comparison
 
# Joining all 3 data sets to make quarter 3 of 2020  (Q3_2020)
#```````````````````````````````````````````````````````````````````````````````
  Q3_2020 <- rbind(Aug_2020,Sep_2020,Oct_2020 )
 
#--------
# Q4_2020 
#--------
# Checking columns consistency 
#```````````````````````````````````````````````````````````````````````````````
 Nov_Var<- data.frame(Aug_col =  c(colnames(Nov_2020) ))
 Dec_Var<- data.frame(Sep_col =  c(colnames(Dec_2020) ))
 Jan_Var<- data.frame(Oct_col =  c(colnames(Jan_2021) ))
 
 comparedf(Nov_Var,Dec_Var) # compare column names in November and December data
 comparedf(Nov_Var,Jan_Var) # compare column names in November and January-2021
 summary(comparedf(Nov_Var,Dec_Var)) # Gives a detail summary of the comparison
 summary(comparedf(Nov_Var,Jan_Var)) # Gives a detail summary of the comparison
 
# Joining all 3 data frames to make last quarter of 2020 (Q4_2020)
# Note that Jan_2021 was added to this quarter
#```````````````````````````````````````````````````````````````````````````````
   Q4_2020 <- rbind(Nov_2020, Dec_2020, Jan_2021 )
 
#--------
# Q1_2021 
#-------- 
# Checking columns consistency 
#```````````````````````````````````````````````````````````````````````````````
 Feb_Var<- data.frame(Aug_col =  c(colnames(Feb_2021) ))
 Mar_Var<- data.frame(Sep_col =  c(colnames(Mar_2021) ))
 Apr_Var<- data.frame(Oct_col =  c(colnames(Apr_2021) ))
 
 comparedf(Feb_Var,Mar_Var) # compare column names in February and March data
 comparedf(Feb_Var,Apr_Var) # compare column names in February and March data
 summary(comparedf(Feb_Var,Mar_Var)) # Gives a detail summary of the comparison
 summary(comparedf(Feb_Var,Apr_Var)) # Gives a detail summary of the comparison
 
# Joining all 3 data frames into Q1_2021 (quarter 1 of 2021 )
#```````````````````````````````````````````````````````````````````````````````
  Q1_2021 <- rbind(Feb_2021, Mar_2021, Apr_2021 )
  
# Checking the data structure
#```````````````````````````````````````````````````````````````````````````````
  
  str(Q2_2020)
  str(Q3_2020)
  str(Q4_2020)
  str(Q1_2021)

# Converting start_station_id and end_station_id from  Q2_2020, Q3_2020 , 
# and Q4_2020 quarters to character, so they match Q1_2021 start_station_id
#```````````````````````````````````````````````````````````````````````````````
 
 Q2_2020 <- mutate( Q2_2020, start_station_id = as.character(start_station_id), 
                                  end_station_id= as.character(end_station_id))
 Q3_2020 <- mutate( Q3_2020, start_station_id = as.character(start_station_id), 
                                  end_station_id= as.character(end_station_id))
 Q4_2020 <- mutate( Q4_2020, start_station_id = as.character(start_station_id), 
                                  end_station_id= as.character(end_station_id))

# finally we join all quarters into one single data  "all_data"
#```````````````````````````````````````````````````````````````````````````````
  all_data <- bind_rows(Q2_2020, Q3_2020 ,Q4_2020, Q1_2021)
 

#==================================
# STEP3: PREPARE DATA FOR ANALYSIS
#==================================
 
 colnames(all_data) # It returns the all column names
 head(all_data)     # It returns the first 6 rows of the data frame
 tail(all_data)     # It returns the last 6  rows of the data frame
 nrow(all_data)     # It gives the number of rows
 ncol(all_data)     # It gives the number of columns
 dim(all_data)      # It gives the dimension of the data frame
 summary(all_data)  # It gives a statistical summary of the data
 
# Add columns such as date, Month, Day, Year, and day_of_week for each ride,
# further we can aggregate ride data at different levels
#``````````````````````````````````````````````````````````````````````````````` 
 all_data$date <- as.Date(all_data$started_at) #The default format is yyyy-mm-dd
 all_data$Month <-format(as.Date( all_data$date), "%m")# Months as numbers(1-12)
 all_data$Months <-format(as.Date( all_data$date), "%B")# Months names
 all_data$Day <- format(as.Date( all_data$date), "%d")  # Days as numbers 
 all_data$day_of_week <- format(as.Date(all_data$date), "%A") # Day names 
 all_data$Year <- format(as.Date( all_data$date), "%Y")     # Extract the years
 all_data$Hour <- format(all_data$started_at, format = "%H")# Extract the hours
 

#Add ride_length as a new column
#```````````````````````````````````````````````````````````````````````````````
 all_data_v2 <-all_data %>% 
   mutate( ride_length = difftime(ended_at,started_at), .keep = "all")
 
# convert ride_length to numerical values
#``````````````````````````````````````````````````````````````````````````````
  all_data_v3 <-all_data_v2 %>% 
    mutate( ride_length = as.numeric(ride_length))
  
 
# Some ride_lengths are negative so we have to remove them 
# There are two methods that we can use
#``````````````````````````````````````````````````````````````````````````````
  sum(all_data_v3$ride_length<=0) # ride_length that are <0
  sum(all_data_v3$ride_length>0) #  ride_length that are >=0

  all_data_v4 <- all_data_v3 %>%  # it keeps only ride_length =>0
     filter( !(ride_length <=0))
   
# Check the structure of columns
#```````````````````````````````````````````````````````````````````````````````
   str(all_data_v4)
   
# looking for any duplicate, to do so we apply the "unique function" to ride_id
# and then we compare its number of rows with the "all_data_v4" rows number
#``````````````````````````````````````````````````````````````````````````````
   
 unique_Rows_num<-NROW(unique( all_data_v4$ride_id))#the total of unique ride_id
 all_rows_num<- nrow(all_data_v4)                  # The total rows of our data
 all.equal(all_rows_num, unique_Rows_num) 
   
# Checking  missing values
#``````````````````````````````````````````````````````````````````````````````
 sum(is.na(all_data_v4$ride_length)) # Gives the sum of missing values
 
   
#=============================
# STEP 4: DESCRIPTIVE ANALYSIS
#=============================

# Let's see user types frequency and percent
#```````````````````````````````````````````````````````````````````````````````
#Frequency table
#:::::::::::::::
 
 fre_tab<- all_data_v4 %>% 
   group_by(member_casual) %>% 
   summarize(count= n()) %>% 
   mutate(prop = count/ sum(count), percent = percent(prop)) %>% 
   arrange(count)
 fre_tab 
 
# Bar chart
#::::::::::
 
b<- fre_tab %>% 
    ggplot(aes(x=  member_casual, y= count, fill = member_casual))+
    geom_col(position = "dodge") +
    theme_classic()+ labs(x= NULL, y= NULL, title = "User Count: Member vs Casuals" )

# Pie chart
#::::::::::

p<- fre_tab %>% 
    ggplot(aes(x= "", y= count, fill = member_casual))+
    geom_col() +
    coord_polar(theta ="y")+
    geom_text(aes(label=  percent), position = position_stack(vjust=0.5), size= 8 )+
    theme_void() + theme(legend.position = "none")
    
grid.arrange(b,p, nrow=1)


# Users by type of bikes 
#```````````````````````````````````````````````````````````````````````````````
# Frequency Of Bike Types 
#::::::::::::::::::::::::

bike_tab<- all_data_v4 %>% 
   group_by(member_casual, rideable_type) %>% 
   summarize(count= n(), .groups ='drop') %>% 
   mutate( prop = count/ sum(count), percent = percent(prop)) %>% 
   arrange(member_casual, rideable_type)
bike_tab

# Bar chart:
#:::::::::::

bike_tab %>% 
  ggplot(aes(x= member_casual, y= count , fill = rideable_type))+
  geom_bar(position = "dodge", stat= "identity")+
  theme_classic()+ ylim(0,1600000)+
  ggtitle("Users Count By Type Of Bikes")
#==============
# Ride duration
#==============
# Statistical summary of  ride_length (in seconds) 
#```````````````````````````````````````````````````````````````````````````````
  all_data_v4 %>% 
    dplyr::select(ride_length) %>% 
    summary(ride_length)

# Ride length density for all users
#::::::::::::::::::::::::::::::::::
all_data_v4 %>% 
  ggplot(aes(x = ride_length )) + 
  geom_histogram(aes(y = ..density..), colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = 4, fill = 4, alpha = 0.25)+
  theme_classic()+ labs(x= " ride length", title = "Ride Length Density")

# Ride length density for all users with log transformation
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

all_data_v4 %>% 
  ggplot(aes(x = log(ride_length ))) + 
  geom_histogram(aes(y = ..density..),colour = 1, fill = "white") +
  geom_density(lwd = 1, colour = "black",fill = "grey", alpha = 0.70)+
  theme_classic()
 
# Ride length summary by group of user
#``````````````````````````````````````````````````````````````````````````````
 all_data_v4 %>% 
   dplyr::select(ride_length, member_casual) %>% 
   group_by(member_casual) %>% 
   summarize(min = min(ride_length),q1= quantile(ride_length, probs=0.25),
                      mean= mean(ride_length), median=median(ride_length),
            q3= quantile(ride_length, probs=0.75), max = max(ride_length))
 
# As the data are right skewed we will transform the the ride length to log
#: Density:
#::::::::::
d<-all_data_v4 %>%
   ggplot(aes(x = log(ride_length) ,color = member_casual ,fill= member_casual)) +
   geom_histogram(aes( y = ..density..),color =1, fill = "white") + 
   theme_classic()+ geom_density(alpha=0.5)+ 
   facet_wrap(~member_casual)+
   theme(legend.position = "none")+
   labs(x= NULL, y= NULL, title = "Ride Length By User Types")
 
#:Violin Plot
#::::::::::::: 
vp<-all_data_v4 %>%
   ggplot(aes(x = member_casual , y= log(ride_length) , fill= member_casual) ) +
   theme_classic()+ geom_violin() + theme(legend.position = "none")+
   geom_boxplot(width=0.1)+ labs(x= NULL, y= NULL)

grid.arrange(d,vp,nrow=2)
#------------------------------------------------------------------------------
# Ride duration by hour
#-------------------------------------------------------------------------------

 #Box Plot:
 #:::::::::
 all_data_v4 %>%
   ggplot(aes(x = Hour, y = log(ride_length ), fill = member_casual) ) +  # bar graphs
   geom_boxplot()+ 
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+ coord_flip()+
   theme_classic()

# Let's see ride duration average and Total by hour and user types 
#-------------------------------------------------------------------------------
# Bar plots  
#::::::::::
 
# Average ride duration by hour
#```````````````````````````````````````````````````````````````````````````````
 A<-all_data_v4 %>%
   group_by(member_casual, Hour) %>%
   summarize(Average = mean(ride_length),.groups = "drop") %>%  
   arrange(member_casual, Hour) %>%                 # Sorts the data
   ggplot(aes(x = Hour, y = Average , fill= member_casual) ) + 
   geom_col(position= "dodge")+
   ggtitle(" Ride Average Duration by Hour")+ 
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+ coord_flip()+
   theme_classic() +theme(legend.position = "none")
 
# Total ride duration by hour 
#```````````````````````````````````````````````````````````````````````````````
 B<-all_data_v4 %>%
   group_by(member_casual, Hour) %>%
   summarize( Total = sum(ride_length), .groups = "drop") %>%  
   arrange(member_casual, Hour) %>%                 # Sorts the data
   ggplot(aes(x = Hour, y = Total , fill= member_casual) ) +   # bar graphs
   geom_col(position= "dodge")+
   ggtitle(" Total Ride  Duration By Hour")+ 
   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
   coord_flip()+
   theme_classic() + theme(legend.position = "none")
 
 grid.arrange(A,B, nrow=1)  

#-------------------------------------------------------------------------------
# Ride length by day of the week
#-------------------------------------------------------------------------------
# Reorder the day of week 
#``````````````````````````````````````````````````````````````````````````````
  all_data_v4$day_of_week <- ordered(all_data_v4$day_of_week,
  levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
            "Saturday"))

# Statistical summary for the ride length by user type and day of the week
#``````````````````````````````````````````````````````````````````````````````
all_data_v4 %>% 
  dplyr::select(ride_length,member_casual,day_of_week) %>%               
  group_by(day_of_week,member_casual) %>%      # group by casuals and members
  summarise(mean= mean(ride_length), median=median(ride_length),  
  max= max(ride_length),min =min(ride_length)) %>% 
  arrange(day_of_week)                          # Sort the data

#Box Plot:
#:::::::::
# The data are right skewed so we use log(ride_length )
#```````````````````````````````````````````````````````````````````````````````
 all_data_v4 %>%
  ggplot(aes(x = day_of_week, y = log(ride_length ), fill = member_casual) ) +   # bar graphs
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_classic()+labs(x= NULL, title = "Ride length By Day And User Type")+
  theme(legend.position = "bottom")+
  theme(legend.title =  element_blank())

# Let's see the average ride duration by day for rider type (casual v member)
#```````````````````````````````````````````````````````````````````````````````
 avg_ride_len<- all_data_v4 %>% 
    dplyr::select(ride_length,member_casual,day_of_week) %>% 
    group_by(member_casual,day_of_week) %>% 
    summarize( average = mean(ride_length), sum = sum(ride_length), .groups ="drop") %>% 
    arrange(member_casual,day_of_week)

# Bar plots :
#::::::::::::

# Total of ride length by day of the week and user types
#```````````````````````````````````````````````````````````````````````````````
ra<-avg_ride_len %>%                 # Sorts the data
    ggplot(aes(x = day_of_week, y = sum , fill= member_casual) ) + # bar graphs
    geom_col(position= "dodge") +  labs( x= NULL, y= NULL, title=" Ride Total Duration by Day") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    coord_flip()+theme_classic()+ theme(legend.position = "none")
    
# Average of ride length by day of the week and user types
#```````````````````````````````````````````````````````````````````````````````
rt<-avg_ride_len %>%                 # Sorts the data
    ggplot(aes(x = day_of_week, y = average , fill= member_casual) ) + # bar graphs
    geom_col(position= "dodge") + labs( x= NULL, y= NULL, title=" Ride Average Duration by Day") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    coord_flip()+theme_classic()+
    theme(legend.position = "bottom")+
    theme(legend.title= element_blank())
grid.arrange(ra,rt, nrow=2)              
#-------------------------------------------------------------------------------  
# Average of ride length by month and user types
#-------------------------------------------------------------------------------
# Statistical summary 
summary_month<- all_data_v4 %>%
  group_by(member_casual, Months) %>%
  summarize(sum = sum(ride_length), average = mean(ride_length),.groups= "drop") %>%  
  arrange(member_casual, Months)

#Box Plot:
#:::::::::
all_data_v4 %>%
  ggplot(aes(x = Months, y = log(ride_length ), fill = member_casual) ) + # bar graphs
  geom_boxplot()+ coord_flip()+theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x=NULL, title= " Ride Duration by Month And User Types")+
  theme(legend.position="bottom")+
  theme(legend.title= element_blank())

# Bar plotS :
#::::::::::::
# Total of ride lengths by month and user types
#```````````````````````````````````````````````````````````````````````````````

m1<- summary_month %>%     
  ggplot(aes(x = Months, y = sum , fill= member_casual) ) +
  geom_col(position= "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x=NULL, y=NULL, title = "Ride's Total Duration by Month")+
  theme_classic()+
  coord_flip() +
  theme(legend.position = "none")
  

# Average of ride lengths by month and user types
#```````````````````````````````````````````````````````````````````````````````

m2<-  summary_month %>%     
  ggplot(aes(x = Months, y = average , fill= member_casual) ) + 
  geom_col(position= "dodge") +
  theme( axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "none")+
  labs(x=NULL, y=NULL, title = "Ride's Average Duration by Month")+
  theme_classic()+ 
  coord_flip() + theme(legend.position = "none")
grid.arrange(m1,m2, nrow=1)
#-------------------------------------------------------------------
# Heat map 
#:::::::::
# Ride length duration by hour and day of the week
all_data_v4 %>% 
  dplyr::select(ride_length,day_of_week,Hour, member_casual) %>% 
  group_by(member_casual,day_of_week, Hour) %>% 
  summarize(count=sum(ride_length)) %>% 
  ggplot(aes(x= day_of_week , y= Hour , fill= count ))+
  geom_tile()+ facet_wrap(~member_casual)

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
   
write.csv(avg_ride_len, file = 'C:/Users/MAIN/Desktop/Google Cert/Google_Capstone_Project/Case_study_1_Data/avg_ride_len.csv')
write.csv(avg_ride_len, file = 'C:/Users/MAIN/Desktop/Google Cert/Google_Capstone_Project/Case_study_1_Data/all_data.csv')
write.csv(avg_ride_len, file = 'C:/Users/MAIN/Desktop/Google Cert/Google_Capstone_Project/Case_study_1_Data/all_data_v4.csv')
   
   
   str(all_data_v4)
   