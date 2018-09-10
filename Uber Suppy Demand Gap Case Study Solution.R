#setwd("/Users/rahushukla/Desktop/Trainings/Upgrad/7. Uber Demand Supply Case Study")

uber_df <- read.csv("Uber Request Data.csv",stringsAsFactors = F)

library("ggplot2")
library("lubridate")
library(dplyr)
library(tidyr)

View(uber_df)

##DATA CLEANING 

#Check for duplicate values
nrow(uber_df) #6745
nrow(uber_df %>% unique) #6745

#Check for NA values
sum(is.na(uber_df$Request.id)) # 0 
sum(is.na(uber_df$Pickup.point))# 0 
sum(is.na(uber_df$Status))# 0 
sum(is.na(uber_df$Request.timestamp))# 0 


#Make the time separator consistent
uber_df$Request.timestamp<- parse_date_time(uber_df$Request.timestamp,orders = 
                                                  c("d/m/Y H:M","d-m-Y H-M-S","%d-%m-%y %H:%M:%S","%d-%m-%y %H:%M"))


uber_df$Drop.timestamp<- parse_date_time(uber_df$Drop.timestamp,orders = 
                                              c("d/m/Y H:M","d-m-Y H-M-S","%d-%m-%y %H:%M:%S","%d-%m-%y %H:%M"))

###DATE CAN BE FOUND OUT IF NEEDED 
uber_df$date <- (as.Date(uber_df$Request.timestamp ))

###
uber_df$Request.hour <- hour(uber_df$Request.timestamp)

#Defining function to compute time slot based on Request.hour
compute_timeslot<-function(Request.hour){
  if(Request.hour<=3){
    return("Late Night")
  }else if(Request.hour<=7){
    return("Early Morning")
  }else if(Request.hour<=12){
    return("Morning")
  }else if(Request.hour<=16){
    return("Afternoon")
  }else if(Request.hour<=20){
    return("Evening")
  }else if(Request.hour<=23){
    return("Night")
  }else{
    return(NA)
  }
}

# Creating a new variable Request.timeslot to assign a valid time slot to each request
uber_df$Request.timeslot<-sapply(uber_df$Request.hour,compute_timeslot)
# The below will help to always order the plots in the order mentioned below, and not in alphabetical order
uber_df$Request.timeslot <- factor(uber_df$Request.timeslot, c("Late Night", "Early Morning", "Morning","Afternoon","Evening","Night"))


## This field determines the time taken for the journey
uber_df$Duration <- difftime(uber_df$Drop.timestamp,uber_df$Request.timestamp)
#Univariate analysis

#plotting the frequency of cancelled, trip completed and No cars available by date and pickup point
ggplot(uber_df,aes(x=factor(Status)))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1)

##NOTE : Abbrevations used : Cancelled - Can ; No Cars Available (NCA) ; Trip Completed (TC)
###################################################################################################################
##### Visually identify the most pressing problems for Uber. 

#1 Quick plot by Status to see the gravity of problem
# Bar chart with count as label
ggplot(uber_df,aes(x=Status))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1)
## OBSERVATION: 1264 Can, 2650 NCA, 2831 TC . Hence, Can & NCA is a valid problem that needs addressal.

#2 Plot by pickup point to see the area of concern
# Stacked bar chart with approp label

ggplot(uber_df,aes(x=Pickup.point,fill=Status))+geom_bar()+geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5))+labs(x = "Pickup location",y="Total Request")
##OBS: Can is not major for airport pickups, but it is for city picksups
##OBS: NCA is major concern for airport pickups, and so for city pickups

#3 Plot by time slot to identify time specific behavior by keeping status on x axis
ggplot(uber_df,aes(x=(Status),fill=Request.timeslot))+geom_bar()+geom_text(stat='count',aes(label=..count..),position = position_stack(vjust = 0.5))+labs(x = "Status",y="Total Request")
###OBS: Can. is more in Early Morning and Night. NCA is more in Eve & Night.

##3.1 Breaking down the above plot further by pickup point
ggplot(uber_df,aes(x=(Status),fill=Request.timeslot))+geom_bar()+facet_wrap(~Pickup.point)+labs(x = "Status",y="Total Request")
###OBS: From airport: More Can,NCA in Evening & Night. From city: More Can,NCA in Morning & Early Morning

##3.2 Alternative way to plot by keeping timeslot on x-axis
ggplot(uber_df,aes(x=(Request.timeslot),fill=Status))+geom_bar()+facet_wrap(~Pickup.point)+labs(x = "Time Slot",y="Total Request") # removing label on bar graph for clarity
###OBS: Same as above. Confirms our theory.


ggplot(uber_df,aes(x=Pickup.point,fill=Status))+geom_bar(position = "dodge",col="black")+geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))+labs(x = "Pickup location",y="Total Request")

###################################################################################################################
##### Find out the gap between supply and demand and show the same using plots.

#1 Understanding the demand by timeslot to airport and city 
ggplot(uber_df,aes(x=Request.timeslot,fill=Pickup.point))+geom_bar(position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))+labs(x = "Pickup location",y="Total Request")
##OBS: During early morning and morning, pickup from city to airport is high (As most prefer early morning flights)
###    Have already seen above that Can,NCA is more in early morning and morning for city (Possibly as the demand is high) 
##OBS: During evening and night, pickup from airport to city is high (As most people prefer to land during this time)
###    Have already seen above that Can,NCA is more in  evening and night for airport (Possibly as the demand is high) 

### Create Deamnd,Supply and Gap for calculation and better understanding

uber_df$Supply = ifelse(uber_df$Status=="Cancelled" | uber_df$Status=="No Cars Available" , "gap", "supply")
uber_df$Demand = ifelse(uber_df$Status=="Cancelled"|uber_df$Status=="No Cars Available"|uber_df$Status=="Trip Completed", "demand", "NA")


#2 Plot the supply by time slots to understand the time slot when the highest gap exists
ggplot(uber_df,aes(x=(Request.timeslot),fill=Supply))+geom_bar(position = "dodge")+ geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))+labs(x = "Time Slot",y="Total Request")

#Demand can be added to above graph to visualize the relative percentage of supply and gap against demand better
ggplot(uber_df,aes(x=Supply,fill=Supply))+geom_bar()+geom_bar(aes(x=Demand),fill="green4")+geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))+facet_grid(~uber_df$Request.timeslot)

###OBS: Cleary the gap is highest in the evening. But this analysis needs to be broken down further by pickup point

#Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
#Plot showing demand and supply gap for different pickup requests on various timeslots
ggplot(uber_df,aes(x=Supply,fill=Supply))+geom_bar()+geom_bar()+facet_grid(Pickup.point~Request.timeslot)+geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))

###OBS: For airport pickup: Gap is very high in Evening and Night
#       For city pickup: Gap is very high for Early morning and morning pickup

###################################################################################################################
ggplot(uber_df,aes(x=Supply,fill=Supply))+geom_bar()+geom_bar()+facet_grid(Pickup.point~Request.timeslot)+geom_text(stat='count',aes(label=..count..),vjust=-1,position = position_dodge(width = 1))

#Lets combine the analysis from the above 2 sections together in words
##OBS: During early morning and morning, pickup from city to airport is high (As most prefer early morning flights)
###    Can,NCA is more in early morning and morning for city
###    For city pickup: Gap is very high for Early morning and morning pickup


##OBS: During evening and night, pickup from airport to city is high (As most people prefer to land during this time)
###    Have already seen above that Can,NA is more in  evening and night for airport (Understandbly as the demand is high) 
###    For airport pickup: Gap is very high in Evening and Night

#####  What do you think is the reason for this issue for the supply-demand gap?
##### Write the answer in less than 100 words. You may accompany the write-up with plot(s).

# The arrival and departure timing preferrances of the passengers are different. The demand for city pickup is high in morning. 
# However, the pickup demand from airport is more in the evening only. 
# Hence, the wait time for most drivers going to the airport in the morning would be very high, and they don’t have any incentive to do so. 
# So, the cancellation and cab availability are low. Similarly, in evening, there aren’t enough cabs at the airport to meet the demand. 
# High wait time and no incentives to take the trip is resulting in the gap between demand and supply. 
