library(tidyr)
library(dplyr)
library(DT)

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
         "InterventionDate", "Month", "SubjectRaceCode", "SubjectEthnicityCode", "InterventionLocationName",
         "InterventionLocationDescriptionText")


master_2019 <- stops_2019 %>% 
  left_join(officer_names, by = "ReportingOfficerIdentificationID")%>% 
  select("ReportingOfficerIdentificationID", "Name", "ReasonForStop", 
         "InterventionDate", "Month", "SubjectRaceCode", "SubjectEthnicityCode", "InterventionLocationName",
         "InterventionLocationDescriptionText")

#r bind two dataframes
total <- rbind(master_2018, master_2019) 

#save data to R file
save(total, file = "traffic_race_location.rdata")

