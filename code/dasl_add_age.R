library(tidyverse)
dbc <- FUDBAccess::FU_db_connect()$IR

dob <- tbl(dbc,"Person") %>% 
  select(RaterID = FurmanID, DOB = Birth.Date) %>% 
  collect() %>% 
  mutate(RaterYear = as.integer(substr(DOB,1,4)))

dasl <- read_csv("data/dasl.csv") %>% 
  select(-RaterAge,-RaterYear) %>% 
  left_join(dob) %>%
  mutate(RaterAge = as.integer(substr(Term,1,4)) - RaterYear) %>% 
  select(-DOB)

write_csv(dasl,"data/dasl.csv")
