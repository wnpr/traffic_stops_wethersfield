library(tidyr)
library(dplyr)

#import data
stops_2018 <- read.csv("wethersfield-r5.csv", header = T, stringsAsFactors = F)
officer_names <- read.csv("officer_names.csv", header = T, stringsAsFactors = F)
stops_2019 <- read.csv("2019WethersfieldJan-Mar.csv", header = T, stringsAsFactors = F)

#tidy data
#make $ReportingOfficerIdentificationID numeric 
stops_2018 <- stops_2018 %>%  
  mutate(ReportingOfficerIdentificationID = as.numeric(ReportingOfficerIdentificationID))

stops_2019 <- stops_2019 %>%  
  mutate(ReportingOfficerIdentificationID = as.numeric(ReportingOfficerIdentificationID))

officer_names <- officer_names %>% 
  mutate(ReportingOfficerIdentificationID = as.numeric(ReportingOfficerIdentificationID))

#left join stops to officer name for both years and select columns

master_2018 <- stops_2018 %>% 
  left_join(officer_names, by = "ReportingOfficerIdentificationID") %>% 
  select("ReportingOfficerIdentificationID", "Name", "ReasonForStop", 
         "InterventionDate", "Month")

master_2019 <- stops_2019 %>% 
  left_join(officer_names, by = "ReportingOfficerIdentificationID")%>% 
  select("ReportingOfficerIdentificationID", "Name", "ReasonForStop", 
         "InterventionDate", "Month")

#r bind two dataframes
total <- rbind(master_2018, master_2019) 

save(total, file = "traffic_reason_data.rdata")

##Create Dataframe for Stops That Were Recorded Just Since Euilizer Was Hired 
##August 2018 through March 2019

library(stringr)

#add column year
total_year <- total %>% 
  mutate(Year = 
           if_else(str_detect(InterventionDate, "2019"), "2019", "2018")
  )

#create new DB all stops since August 2018
all_stops_since_aug_2018 <- total_year %>% 
  filter(Month == "August" | Month == "September" | Month == "October" | Month == "November" | 
           Month == "December" | Year == "2019")

#save all_stops_since_aug_2018 to R file
save(all_stops_since_aug_2018, file = "all_stops_since_aug_2018.rdata")




