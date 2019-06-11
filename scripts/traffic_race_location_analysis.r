library(tidyr)
library(dplyr)

#setwd()

load("traffic_race_location.rdata")

#mutate df "total" to add H to new $Race

total <- total %>% 
  mutate(Race = 
           ifelse(total$SubjectEthnicityCode == "N", total$SubjectRaceCode, total$SubjectEthnicityCode)
  )

##Count of Race for Stop For Department, Salvatore, and Eulizier

#Department

department_stops_by_race <- total %>% 
  group_by(Race) %>% 
  count(Race) %>% 
  arrange(desc(n))%>% 
  mutate(percent_race_of_stops = (n/sum(department_stops_by_race$n))*100)

#Salvatore
salvatore_575 <- total %>% 
  filter(Name == "Ofc. Salvatore" | ReportingOfficerIdentificationID == "575") %>% 
  group_by(Race) %>% 
  count(Race) %>% 
  arrange(desc(n)) %>% 
  mutate(percent_race_of_stops = (n/sum(salvatore_575$n))*100)

#Eulizier
eulizier_250044 <- total %>% 
  filter(Name == "Ofc. Eulizier" | ReportingOfficerIdentificationID == "250044") %>% 
  group_by(Race) %>% 
  count(Race) %>% 
  arrange(desc(n))%>% 
  mutate(percent_race_of_stops = (n/sum(eulizier_250044$n))*100)


#Verify Methodology For 'Hispanic' Count Against Barone Report CRPR Numbers 
#page 28 of 2017 report for statewide by race
#http://sue.apps-1and1.com/wp-content/uploads/2018/08/November-2017-Connecticut-Racial-Profiling-Report.pdf

df <- read.csv("connecticut-r3.csv", header = T, stringsAsFactors = F)

df <- df %>% 
  mutate(Race = 
           ifelse(df$SubjectEthnicityCode == "N", df$SubjectRaceCode, df$SubjectEthnicityCode)
  )

stops_by_race <- df %>% 
  group_by(Race) %>% 
  count(Race) %>% 
  arrange(desc(n))%>% 
  mutate(percent_race_of_stops = (n/sum(stops_by_race$n))*100)


##Count of Location for Stop For Department, Salvatore, and Eulizier

library(stringr)

#clean data to create $new_location all lower case
total_lower_case <- total %>%
  mutate(location = 
           str_to_lower(total$InterventionLocationDescriptionText)
  )

#mutate new column that records if stop was on Silas Deane
total_lower_case <- total_lower_case %>% 
  mutate(silas_deane_stop = 
           if_else(str_detect(location, "silas"), "Y", "N")
           )

#count stops on silas compared to all other stops for Department, Salvatore, and Eulizier

#Department
department_silas <- total_lower_case %>% 
  group_by(silas_deane_stop) %>% 
  count(silas_deane_stop) %>% 
  arrange(desc(n)) %>% 
  mutate(percent_of_all_stops = (n/sum(department_silas$n))*100)

#Salvatore
salvatore_silas <- total_lower_case %>% 
  filter(Name == "Ofc. Salvatore" | ReportingOfficerIdentificationID == "575") %>% 
  group_by(silas_deane_stop) %>% 
  count(silas_deane_stop) %>% 
  arrange(desc(n)) %>% 
  mutate(percent_of_all_stops = (n/sum(salvatore_silas$n))*100)
  
#Eulizier  
eulizier_silas <- total_lower_case %>% 
  filter(Name == "Ofc. Eulizier" | ReportingOfficerIdentificationID == "250044") %>% 
  group_by(silas_deane_stop) %>% 
  count(silas_deane_stop) %>% 
  arrange(desc(n)) %>% 
  mutate(percent_of_all_stops = (n/sum(eulizier_silas$n))*100)


##Where Are Cars Being Stopped On Silas Deane Highway?         
#filter for 'silas' on original 'total' df
department_silas_location <- total_lower_case %>% 
      filter(str_detect(location, "silas")) %>% 
  group_by(location) %>% 
  count(location) %>% 
  arrange(desc(n)) %>% 
  mutate(percent_location_of_silas_stops = (n/sum(department_silas_location$n))*100)


##note duplications/multiple names for same place in output DF
#filtered to address duplicates, if needed
#example maple location search

department_silas_maple <- department_silas_location %>% 
  filter(str_detect(location, "maple"))

maple_n_per <- sum(department_silas_maple$percent_location_of_silas_stops)

#write.csv(department_silas_location, "department_silas_location.csv")


