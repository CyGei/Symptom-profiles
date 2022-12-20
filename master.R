######################################
# comparison of symptoms
######################################
#vaccinaion data: file:///S:\CoronaWatch\Working\data_cleaning_cleaned\HES\3_Output\NIMS_VW_vaccination_data.csv
#antibody data (cols = accesion date + test result covg) file:///S:\CoronaWatch\Working\data_cleaning_cleaned\Lab\Antibody_Thriva\Output\lab_antibody_thriva.csv
# SGGS test results (1 = positive, 2 = negative): file:///S:\CoronaWatch\Working\data_cleaning_cleaned\Lab\PCR\Output\VW_SGSS_Pillar_2.csv
#data dictionary: file:///S:\CoronaWatch\Working\data_cleaning_cleaned\Data_Request_Form_and_Instructions_2021-07-21.xlsx
#follow up : "S:\\CoronaWatch\\Working\\Individual_working_folders\\Vincent\\Participant_Lookup_Logistics"


# SETUP -------------------------------------------------------------------

home_directory <-
  "S:/CoronaWatch/Working/Individual_working_folders/Cy/symptoms3/"
setwd(home_directory)
source("scripts/libraries.R")
source("scripts/symptom_vectors.R")
source("scripts/load_fluwatch.R")
#source("scripts/load_vw.R")
source("scripts/functions.R")

data_update = "2022-10-25"
save_path = paste0("output/", data_update)

vw_pos <- readRDS("data/2022-10-25/vw_raw.RData") %>%                              
  filter(vw_sgss_result == 1) %>% 
  group_by(household_id) %>%
  arrange(start_dt, .by_group = TRUE) %>%
  ungroup() %>% 
  drop_na(nVar) %>% 
  filter(ARI == 1)

fluwatch
vw_pos



table(vw_pos$age3)

table(fluwatch$age3)

# Descriptive table -------------------------------------------------------
source("scripts/table1.R")



# ARI ILI -----------------------------------------------------------------

source("scripts/ari_ili")



# COMPARE FLUWATCH & VW -------------------------------------------------

source("scripts/fw_vw_1.R") #SARS-CoV-2
#source("scripts/fw_vw_2.R") #covid variants

fw_vw

# VIRUSWATCH --------------------------------------------------------------
source("scripts/vw.R")


# Modelling ---------------------------------------------------------------

library(future.apply)
ncores <- availableCores() - 3
plan(multisession, workers = ncores)

source("scripts/model.R")

# models_df <- future_lapply(vector_list[svec],
#        function(x){
#          get_models(dataframe = vwatch, 
#                     symptoms = x,
#                     indep.vars = c("hh_age_on_entry", "nVar", "dose1"))
#        })
# 
# models_fig<- future_lapply(models_df, plot_models)



# 
# # Additional Data -----------------------------------------------------------
# 
# vaccination <- data.table::fread("S:/CoronaWatch/Working/data_cleaning_cleaned/HES/3_Output/NIMS_VW_vaccination_data.csv") %>% 
#   mutate(newID = gsub("hh", "", individual_id))
# 
# model_data <- vwatch %>% filter(nVar != "[0] Unknown") %>% 
#   mutate(nVar = factor(nVar, levels = c("[5] Omicron BA2", "[4] Omicron BA1","[3] Delta", "[2] Alpha", "[1] Wild Type" )))
# 
# model_data.vaccine <- merge(model_data,vaccination, all.x = TRUE)
# nrow(model_data.vaccine)
# nrow(model_data)
# 
# 
# model_data.vaccine <- model_data.vaccine %>% mutate(dose1 = ifelse(vaccine_nhs_dose_when_1_min <= start_dt2 , 1,0),
#                  dose2 = ifelse(vaccine_nhs_dose_when_2_min <= start_dt2 , 1,0),
#                  dose3 = ifelse(vaccine_nhs_dose_when_3_min <= start_dt2 , 1,0),
#                  dose4 = ifelse(vaccine_nhs_dose_when_4_min <= start_dt2 , 1,0)) %>% 
#   mutate(across(starts_with("dose"), ~ifelse(is.na(.x), 0, .x)))
# 
# 
# clinically_vulnerable <-
#   VirusWatch::file_index("S:/CoronaWatch/Working/data_cleaning_cleaned/Other/Comorbidities/output/") %>%
#   filter(file_name == "CV_CEV_Status.csv") %>% .$file_path %>%
#   read.csv(.) %>%
#   select(individual_id, cv_cev_missing_cond, cv_cev_cond) %>%
#   mutate(newID = gsub("hh", "", individual_id))
# 
# model_data.vaccine.clinically_vulnerable <- merge(model_data.vaccine, clinically_vulnerable, all.x = TRUE) %>% mutate(cv = ifelse(is.na(cv_cev_cond),"Not clinically vulnerable" ,cv_cev_cond),
#                           cv_bin = ifelse(cv == "Not clinically vulnerable", "Not CV", "CV"),
#                           cv_bin = factor(cv_bin, levels = c("Not CV", "CV")))
# 
# 
# final_model_data <- model_data.vaccine.clinically_vulnerable
# m1 <- glm( cough ~ hh_age_on_entry + nVar,  data = final_model_data, family = binomial(link = "logit")) 
# m2 <-  glm( cough ~ hh_age_on_entry + nVar + dose1 + dose2 + dose3,  data = final_model_data, family = binomial(link = "logit")) 
# m3 <-  glm( cough ~ hh_age_on_entry + nVar + dose1 + dose2 + dose3 + sex_bin,  data = final_model_data, family = binomial(link = "logit")) 
# m4 <-  glm( cough ~ hh_age_on_entry + nVar + dose1 + dose2 + dose3 + cv_bin,  data = final_model_data, family = binomial(link = "logit")) 
# 
# anova(m1, m2, test = "LR") #keep m2
# anova(m2, m3, test = "LR") #keep m2, sex not signif
# anova(m2, m4, test = "LR") # take m4
# 
# lmtest::lrtest(m2,m3)
# lmtest::lrtest(m2,m4)
# summary(m4)
# exp(coefficients(m4))
# 
# 
# 
# models_df <- future_lapply(vector_list[svec],
#                            function(x){
#                              get_models(dataframe = final_model_data, 
#                                         symptoms = x, 
#                                         indep.vars = c( "hh_age_on_entry", "nVar", "dose1", "dose2", "dose3", "cv_bin"))
#                            })
# 
# 
# models_df <- lapply(models_df, function(x){
#   x %>% 
#     mutate(
#       across(c("estimate", "conf.low", "conf.high"),~exp(.)))
# })
# 
# models_fig <- future_lapply(models_df, plot_models)
# 
# ggarrange(plotlist = models_fig, common.legend = TRUE, legend = "bottom")
# 
