

#vaccine data
vaccinations <- data.table::fread("S:/CoronaWatch/Working/data_cleaning_cleaned/HES/3_Output/NIMS_VW_vaccination_data.csv") %>% 
  mutate(newID = gsub("hh", "", individual_id))

#get vaccine dates
vaccinations <- vaccinations %>% 
  select(all_of(c("newID",grep("when",names(vaccinations), value = TRUE)))) %>% 
  pivot_longer(cols = -newID, names_to = "dose", values_to = "date") %>% 
  select(-dose) %>% 
  drop_na()

#get infection dates
infections <- vw %>% 
  select(newID, start_dt2) %>% 
  mutate(date = as.Date(start_dt2)) %>% 
  select(-start_dt2)

#infections and vaccine dates
immunity <- bind_rows(vaccinations, infections)

#merge to vw illness episode
model_df <- merge(vw, immunity, by = "newID", all.x = TRUE)

#get illness id with immunity
illness_immunity <- model_df %>% 
  mutate(diff = as.Date(start_dt2) - as.Date(date)) %>% 
  filter(diff > 14 & diff < 90) %>% 
  pull(illnessid)


vw_model <- vw %>% 
  mutate(S2_exposed = ifelse(illnessid %in% illness_immunity, 1, 0)) 


clinically_vulnerable <-
  VirusWatch::file_index("S:/CoronaWatch/Working/data_cleaning_cleaned/Other/Comorbidities/output/") %>%
  filter(file_name == "CV_CEV_Status.csv") %>% .$file_path %>%
  read.csv(.) %>%
  select(individual_id, cv_cev_missing_cond, cv_cev_cond) %>%
  mutate(newID = gsub("hh", "", individual_id)) %>% 
  select(-individual_id)


vw_model <- merge(vw_model, clinically_vulnerable, by = "newID")


symp <- c(s_list$s.main_resp, s_list$s.const)
vw_model_final <- vw_model %>% 
  as_tibble() %>% 
  select(newID, illnessid, sex_bin, hh_age_on_entry,
         nVar,S2_exposed, cv_cev_cond, cv_cev_missing_cond,
         symp) %>% 
  mutate(vulnerable = ifelse(cv_cev_missing_cond == "Missing", NA, cv_cev_missing_cond),
         vulnerable = ifelse(vulnerable == "Not clinically vulnerable", 0, 1),
         nVar = relevel(nVar, ref = "Omicron BA5")) %>% 
  rename(sex = sex_bin)
  

models <- future_lapply(symp,
                        function(x) {
                          get_models(
                            dataframe = vw_model_final,
                            symptoms = x,
                            indep.vars = c(
                              "vulnerable",
                              "hh_age_on_entry",
                              "nVar",
                              "S2_exposed",
                              "sex"
                            )
                          )
                        })




r <- future_lapply(symp,
                   function(x) {
                     get_r(
                       dataframe = vw_model_final,
                       symptoms = x,
                       indep.vars = c("vulnerable",
                                      "hh_age_on_entry",
                                      "nVar",
                                      "S2_exposed",
                                      "sex")
                     )
                   })


# models_fig <- future_lapply(models, plot_models)
# 
# p <- ggarrange(plotlist = models_fig, 
#                common.legend = TRUE, 
#                legend = "bottom",
#                labels = c("A", "B", "C"))
# 
# p

models <- lapply(1:length(r), function(x){
 models[[x]] %>% mutate(pseudo_r2 = round(r[[x]], digits = 2))
})



A <- bind_rows(models) %>%
  ungroup() %>% 
  filter(!(term %in% "(Intercept)")) %>% 
  mutate(
    group = case_when(
      dv_name %in% s_list$s.main_resp ~ "main respiratory",
      dv_name %in% s_list$s.const ~ "constitutional"
    ),
    term = ifelse(term == "hh_age_on_entry", "age", term),
    term = ifelse(term == "sexMale", "Male", term),
    term = factor(
      term,
      levels = c(
        "Wild Type",
        "Alpha",
        "Delta",
        "Omicron BA1",
        "Omicron BA2",
        "Omicron BA5",
        "vulnerable",
        "S2_exposed",
        "age",
        "Male"
      )
    )
  ) %>% 
  mutate(col = case_when(odds > 1 & p_val == "significant" ~ "higher odds",
                         odds < 1 & p_val == "significant" ~ "lower odds",
                         p_val == "non-significant" ~ "not significant")) %>% 
  filter(group == "main respiratory") %>% 
  ggplot()+
  aes(odds, term, color = col)+
  geom_point()+
  geom_errorbarh(aes(xmin = lower, xmax = upper))+
  geom_vline(aes(xintercept = 1), lty = "dotted")+
  geom_label(aes(label = format(round(odds, digits = 2), nsmall = 2)),
             fill = "white") +
  # geom_text(aes(x = 1.3, y = 0.7,
  #                label = paste0("p-R2: ", pseudo_r2)),
  #            col = "black") +
  facet_grid(group~dv_name, drop = TRUE)+
  theme_bw()+
  scale_color_manual(values = c( "steelblue", "#d004a8", "darkgrey"))+
  labs(x = "", y = "")+
  labs(col = "")+
  theme(legend.position = "none")

A

B <- bind_rows(models) %>%
  ungroup() %>% 
  filter(!(term %in% "(Intercept)")) %>% 
  mutate(group = case_when(dv_name %in% s_list$s.main_resp ~ "main respiratory",
                           dv_name %in% s_list$s.const ~ "constitutional")) %>% 
  mutate(
    group = case_when(
      dv_name %in% s_list$s.main_resp ~ "main respiratory",
      dv_name %in% s_list$s.const ~ "constitutional"
    ),
    term = ifelse(term == "hh_age_on_entry", "age", term),
    term = ifelse(term == "sexMale", "Male", term),
    term = factor(
      term,
      levels = c(
        "Wild Type",
        "Alpha",
        "Delta",
        "Omicron BA1",
        "Omicron BA2",
        "Omicron BA5",
        "vulnerable",
        "S2_exposed",
        "age",
        "Male"
      )
    )
  ) %>% 
  mutate(col = case_when(odds > 1 & p_val == "significant" ~ "higher odds",
                         odds < 1 & p_val == "significant" ~ "lower odds",
                         p_val == "non-significant" ~ "not significant")) %>% 
  filter(group == "constitutional") %>% 
  ggplot()+
  aes(odds, term, color = col)+
  geom_point()+
  geom_errorbarh(aes(xmin = lower, xmax = upper))+
  geom_vline(aes(xintercept = 1), lty = "dotted")+
  geom_label(aes(label = format(round(odds, digits = 2), nsmall = 2)),
             fill = "white") +
  # geom_text(aes(x = 1.3, y = 0.7,
  #               label = paste0("p-R2: ", pseudo_r2)),
  #           col = "black") +
  facet_grid(group~dv_name, scales = "free")+
  theme_bw()+
  scale_color_manual(values = c( "steelblue", "#d004a8", "darkgrey"))+
  labs(x = "", y = "")+
  labs(col = "")+
  theme(legend.position = "bottom")

B

library(patchwork)
p <- A/B


ggsave(p, 
       width = width,
       height = height,
       unit = "cm", 
       file = paste0(save_path, "/model.svg"))


# 
# g <- glm( cough ~  vulnerable + hh_age_on_entry + nVar + S2_exposed,
#     data = vw_model_final ,
#     family = binomial(link = "logit"))
# summary(g)



out_model<- bind_rows(models) %>%
  ungroup() %>% 
  filter(!(term %in% "(Intercept)")) %>% 
  mutate(group = case_when(dv_name %in% s_list$s.main_resp ~ "main respiratory",
                           dv_name %in% s_list$s.const ~ "constitutional")) %>% 
  mutate(
    group = case_when(
      dv_name %in% s_list$s.main_resp ~ "main respiratory",
      dv_name %in% s_list$s.const ~ "constitutional"
    ),
    term = ifelse(term == "hh_age_on_entry", "age", term),
    term = ifelse(term == "sexMale", "Male", term),
    term = factor(
      term,
      levels = c(
        "Wild Type",
        "Alpha",
        "Delta",
        "Omicron BA1",
        "Omicron BA2",
        "Omicron BA5",
        "vulnerable",
        "S2_exposed",
        "age",
        "Male"
      )
    )
  ) %>% 
  mutate(col = case_when(odds > 1 & p_val == "significant" ~ "higher odds",
                         odds < 1 & p_val == "significant" ~ "lower odds",
                         p_val == "non-significant" ~ "not significant"))


out_model %>% 
  filter(dv_name == "sore_throat" & term %in% c("Alpha", "Delta", "Wild Type","Omicron BA1", "Omicron BA2")) %>% 
  summarise(mean_odds = mean(odds),
            mean_l = mean(lower),
            mean_u = mean(upper)) 
