library(tidyr)
library(dplyr)

#setwd()

load("traffic_reason.rdata")

#2018 and 2019 Ranked, Based on ReportingOfficerIdentificationID
by_officer_total <- total %>% group_by(ReportingOfficerIdentificationID) %>% 
  count(ReportingOfficerIdentificationID) %>% 
  arrange(desc(n))

#divide each officers stops by number of months in data set (15 going up to April 1, 2019)

by_officer_monthly <- by_officer_total %>% 
    mutate(monthy_average = (n/15))

#Median Stops for Department
PD_median <- median(by_officer_total$n)

#Average Stops for Department
PD_average <- mean(by_officer_total$n)

#Count of Reason for Stop For Salvatore, Eulizier, and Department

#Salvatore
salvatore_575 <- total %>% 
  filter(Name == "Ofc. Salvatore" | ReportingOfficerIdentificationID == "575") %>% 
  group_by(ReasonForStop) %>% 
  count(ReasonForStop) %>% 
  arrange(desc(n)) %>% 
  mutate(percent_of_stops = (n/sum(salvatore_575$n))*100)

#Eulizier
eulizier_250044 <- total %>% 
  filter(Name == "Ofc. Eulizier" | ReportingOfficerIdentificationID == "250044") %>% 
  group_by(ReasonForStop) %>% 
  count(ReasonForStop) %>% 
  arrange(desc(n))%>% 
  mutate(percent_of_stops = (n/sum(eulizier_250044$n))*100)

#Department
department_reason <- total %>% 
  group_by(ReasonForStop) %>% 
  count(ReasonForStop) %>% 
  arrange(desc(n))%>% 
  mutate(percent_of_stops = (n/sum(department_reason$n))*100)


##Stops That Were Recorded Just Since Euilizer Was Hired In August 2018 through March 2019

load("all_stops_since_aug_2018.rdata")

#Salvatore Since August 2018
salvatore_575_since_2018 <- all_stops_since_aug_2018 %>% 
  filter(Name == "Ofc. Salvatore" | ReportingOfficerIdentificationID == "575") %>% 
  group_by(ReasonForStop) %>% 
  count(ReasonForStop) %>% 
  arrange(desc(n)) %>% 
  mutate(percent_of_stops = (n/sum(salvatore_575_since_2018$n))*100)

#Eulizier
eulizier_250044_since_2018 <- all_stops_since_aug_2018 %>% 
  filter(Name == "Ofc. Eulizier" | ReportingOfficerIdentificationID == "250044") %>% 
  group_by(ReasonForStop) %>% 
  count(ReasonForStop) %>% 
  arrange(desc(n)) %>% 
  mutate(percent_of_stops = (n/sum(eulizier_250044_since_2018$n))*100)

#Department
department_reason_since_2018 <- all_stops_since_aug_2018 %>% 
  group_by(ReasonForStop) %>% 
  count(ReasonForStop) %>% 
  arrange(desc(n)) %>% 
  mutate(percent_of_stops = (n/sum(department_reason_since_2018$n))*100)


