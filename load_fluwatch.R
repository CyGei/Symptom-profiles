


# FLUWATCH: POSITIVE PARTICIPANTS -----------------------------------------
fluwatch <-
  read_dta("data/covidrespvirussymptoms.dta") %>%
  mutate(
    ARI = ifelse(cough == 1 | sore_throat == 1 | runny_nose == 1, 1, 0),
    ILI = ifelse(confirmedfever == 1 & cough == 1, 1, 0)
  ) %>% 
  select(!c(starts_with("first_"))) %>%
  mutate(
    newvirustype = as.character(newvirustype),
    virustype = case_when(
      newvirustype == "1" ~ "influenza",
      newvirustype == "2" ~ "RSV",
      newvirustype == "4" ~ "seasonal CoV",
      newvirustype == "7" ~ "rhinovirus",
      newvirustype == "8" ~ "COVID-19"
    ),
    nVar = virustype,
    agegp6 = as.character(agegp6),
    age =  case_when(
      agegp6 == "1" ~ "1",
      agegp6 == "2" ~ "1",
      agegp6 == "3" ~ "2",
      agegp6 == "4" ~ "3",
      agegp6 == "5" ~ "4",
      agegp6 == "6" ~ "4"
    ),
    age = ifelse(is.na(age) &
                    !is.na(age3), age3, age) #recoding age, age3 has more observations but sometimes agegp6 has an obs where age3 is missing so we recode agegp6 into the same no of categories as age3 so that if age3 is missing we input age6 info
  ) %>% #age3 has more info than agegp6, so use age3 info when agegp6 is missing
  drop_na(virustype) %>%
  select(-c(age3, agegp6, newvirustype, swaboutcome)) %>% 
  mutate_all(function(x) 
    as.character(x)) %>%
  mutate_all(function(x)
    ifelse(x == "", NA, x)) %>%  #consider empty "" cells as NA
  filter(virustype != "COVID-19") %>%  #remove covid as we use the viruswatch data for that
  relocate(illnessid,p_id, virustype) %>% 
  rename(individual_id = p_id) %>% 
  mutate(age3 = case_when(age == "1" ~ "0-15",
                          age == "2" ~ "16-44",
                          age == "3" ~ "45-64",
                          age == "4" ~ "65+"))
