
# DATA --------------------------------------------------------------------


#variant dates
variants <- data.frame(nVar = c("Wild Type", "Alpha", "Delta", "Omicron BA1", "Omicron BA2", "Omicron BA5"),
                       start = as.Date(c("2020-06-22", "2020-12-10", "2021-06-03", "2021-12-14", "2022-03-06", "2022-07-04")),
                       end = as.Date(c("2020-12-08", "2021-05-03", "2021-12-13", "2022-03-05", "2022-05-27", "2022-10-16")))

#lockdown dates
lockdowns <- data.frame(lockdown = c("2nd", "3rd"),
                        start_date = as.Date(c("2020-11-04","2021-01-06" )),
                        end_date = as.Date(c("2020-12-02","2021-03-29" )))


vw_pos <- vw_pos %>% 
  mutate(lockdown = case_when(between(start_dt,lockdowns$start_date[1], lockdowns$end_date[1]) ~ "Lockdown",
                              between(start_dt,lockdowns$start_date[2], lockdowns$end_date[2]) ~ "Lockdown",
                              TRUE ~ "No Lockdown" )) 

#vaccine
vaccine_file <- "S:/CoronaWatch/Working/data_cleaning_cleaned/HES/3_Output/NIMS_VW_vaccination_data.csv"
vaccine_data <- read.csv(vaccine_file) %>% 
  select(all_of(c("individual_id","vaccine_nhs_dose_when_1_min", "vaccine_nhs_dose_when_2_min"))) %>%  
  filter(individual_id %in% unique(vw_pos$individual_id)) %>% 
  rename(dose1 = vaccine_nhs_dose_when_1_min ,
         dose2 = vaccine_nhs_dose_when_2_min ) %>% 
  pivot_longer(cols = -individual_id, names_to = "dose", values_to = "date") %>% 
  mutate(date = na_if(date, ""))


# PROPORTION OF TESTED ILLNESSES --------------------------------------------
#vw_raw =  readRDS("data/2022-10-25/vw_raw.RData")


#proportion % of tested illnesses: 
prop_tested <- vw_raw %>% 
  mutate(vw_sgss_swab = ifelse(is.na(vw_sgss_swab), "0", vw_sgss_swab)) %>%
  drop_na(vw_sgss_swab) %>% 
  group_by(start_dt, vw_sgss_swab) %>% 
  summarise(n = n()) %>% 
  group_by(start_dt) %>% 
  mutate(total_day = sum(n)) %>% 
  ungroup() %>% 
  filter(vw_sgss_swab == "0") %>%
  mutate(prop_n = n/total_day,
         prop_p = 1 - prop_n,
         roll = zoo::rollmean(prop_p, k = 7, fill = NA),
         lockdown = case_when(between(start_dt,lockdowns$start_date[1], lockdowns$end_date[1]) ~ "Lockdown",
                              between(start_dt,lockdowns$start_date[2], lockdowns$end_date[2]) ~ "Lockdown",
                              TRUE ~ "No Lockdown" ))


#plot:
p_prop_tested <- ggplot()+
  geom_col(data = prop_tested, aes(x = start_dt, y = prop_p), col = "grey", alpha = 0.5)+
  geom_line(data = prop_tested, aes(x = start_dt, y = roll, col = lockdown, group = 1))+
  geom_rect(data = variants, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = nVar), alpha = 0.3)+
  scale_fill_manual(values = .VW_pal)+
  scale_color_manual(values = c("#ff6361", "black"))+
  scale_y_continuous(breaks = seq(0,8, by = 0.1), labels = scales::percent)+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%B\n%Y")+
  theme_fira()+
  theme(panel.grid.major.x = element_line(color = "black", size= 0.4, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", size= 0.2, linetype = "dotted"))+
  theme(legend.position = "top",
        legend.box = "horizontal",
        panel.ontop = TRUE)+
  guides(color = guide_legend(nrow = 2, byrow= TRUE))+
  labs(x = "", y = "frequency", fill = "Variant:", col = "")

p_prop_tested

# daily proportion of positive swabs
prop_positive <- 
  raw_data_clean %>% 
  filter(vw_sgss_swab  == "1") %>% 
  mutate(COVID_test = case_when(vw_sgss_result == 0 ~ "negative",
                                vw_sgss_result == 1 ~ "positive",
                                is.na(vw_sgss_result) ~ NA_character_)) %>% 
  drop_na(COVID_test) %>% 
  group_by(start_dt, COVID_test) %>% 
  summarise(n = n()) %>% 
  group_by(start_dt) %>% 
  mutate(total_day = sum(n)) %>% 
  ungroup() %>% 
  filter(COVID_test == "negative") %>% 
  mutate(prop_n = n/total_day,
         prop_p = 1 - prop_n,
         roll = zoo::rollmean(prop_p, k = 7, fill = NA),
         lockdown = case_when(between(start_dt,lockdowns$start_date[1], lockdowns$end_date[1]) ~ "Lockdown",
                              between(start_dt,lockdowns$start_date[2], lockdowns$end_date[2]) ~ "Lockdown",
                              TRUE ~ "No Lockdown" ))


#plot:
p_prop_positive <- ggplot()+
  geom_col(data = prop_positive, aes(x = start_dt, y = prop_p), col = "grey", alpha = 0.5)+
  geom_line(data = prop_positive, aes(x = start_dt, y = roll, col = lockdown, group = 1))+
  geom_rect(data = variants, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = nVar), alpha = 0.3)+
  scale_fill_manual(values = .CoV_pal)+
  scale_color_manual(values = c("#ff6361", "black"))+
  scale_y_continuous(breaks = seq(0,8, by = 0.1), labels = scales::percent)+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%B\n%Y")+
  theme_fira()+
  theme(panel.grid.major.x = element_line(color = "black", size= 0.4, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", size= 0.2, linetype = "dotted"))+
  theme(legend.position = "top",
        panel.ontop = TRUE)+
  labs(x = "", y = "frequency", fill = "Variant:", col = "")+
  guides(color = guide_legend(nrow = 2, byrow= TRUE))



p_prop_positive




# VirusWatch Epidemic Curve -----------------------------------------------

## INCIDENCE
incid <- vw_pos %>% 
  group_by(nVar, lockdown, start_dt) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(start_dt) %>% 
  mutate(cum_n = cumsum(n),
         n_roll = zoo::rollmean(n, k = 7, fill = NA))



#CUMULATIVE INCIDENCE
p_cumulative_incid <- ggplot()+
  geom_line(data = incid,  aes(x = start_dt, y =cum_n, col = lockdown, group = 1), size = 0.7)+
  geom_rect(data = variants, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = nVar), alpha = 0.4)+
  scale_color_manual(values = c("red", "black"))+
  scale_fill_manual(values = .VW_pal)+
  scale_y_continuous(breaks = seq(0,6000, 1000))+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%B\n%Y")+
  theme_fira()+
  theme(panel.grid.major.x = element_line(color = "black", size= 0.4, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", size= 0.2, linetype = "dotted"))+
  labs(y = "cumulative count", x = "", fill = "", color = "")+
  theme(legend.position = "top",
        panel.ontop = TRUE)+
  guides(color = guide_legend(nrow = 2, byrow= TRUE))
p_cumulative_incid



#EPIDEMIC CURVE
p_epicurve <- ggplot()+
  
  #daily incidence:
  geom_col(data = incid,  aes(x = start_dt, y =n), col = "grey")+
  
  # 7 day average:
  geom_line(data = incid,  aes(x = start_dt, y = n_roll, col = lockdown, group = 1), size = 0.65)+
  
  geom_rect(data = variants, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = nVar), alpha = 0.3)+
  scale_color_manual(values = c("red", "black"))+
  scale_fill_manual(values = .VW_pal)+
 # scale_y_continuous(breaks = seq(0,150, 25))+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%B\n%Y")+
  theme_fira()+
  theme(panel.grid.major.x = element_line(color = "black", size= 0.4, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", size= 0.2, linetype = "dotted"),
        legend.position = "top",
        panel.ontop = TRUE)+
  labs(y = "daily incidence", x = "", fill = "Variant:", color = "")+
  guides(color = guide_legend(nrow = 2, byrow= TRUE))





# VACCCINE ----------------------------------------------------------------


p_vaccine <- vaccine_data %>% 
  drop_na(date) %>% 
  group_by(date, dose) %>% 
  count() %>% 
  group_by(dose) %>% 
  mutate(cumsum = cumsum(n)) %>% 
  ggplot()+
  geom_line(aes(x = as.Date(date), y = cumsum, group = dose, col = dose), size = 0.8)+
  geom_rect(data = variants, aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = nVar), alpha = 0.3)+
  scale_color_manual(values = c("forestgreen", "purple"))+
  #scale_y_continuous(breaks = seq(0,5000, 1000))+
  scale_fill_manual(values = .VW_pal)+
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 month", date_labels = "%B\n%Y")+
  theme_fira()+
  theme(panel.grid.major.x = element_line(color = "black", size= 0.4, linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "grey", size= 0.2, linetype = "dotted"),
        legend.position = "top")+
  labs(y = "cumulative count", x = "", fill = "Variant:", color = "Vaccine:")+
  guides(color = guide_legend(nrow = 2, byrow= TRUE))






# SAVE --------------------------------------------------------------------

p_l<- list(p_prop_tested,
           p_prop_positive,
           p_cumulative_incid,
           p_epicurve,
           p_vaccine)
names(p_l) <- c("p_prop_tested",
                "p_prop_positive",
                "p_cumulative_incid",
                "p_epicurve",
                "p_vaccine")


for (i in names(p_l)) {
  ggsave(p_l[[i]], file = paste0(save_path, i, ".svg"))
}

