library(tidyr)
library(dplyr)

load("traffic_reason_data.rdata")

#2018 and 2019 Ranked, Based on 10 Officer Names Obtained By WNPR
by_officer <- total %>% group_by(Name) %>% 
  filter(!is.na(Name)) %>% 
  count(Name) %>% 
  arrange(desc(n))

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

