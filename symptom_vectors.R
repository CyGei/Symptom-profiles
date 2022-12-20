
s.fw_vw_main <- c(
  "sore_throat", "cough", "runny_nose", "sneezing", "fever", "headache", "fatigue"
)


s.cov_all <-
  c(
    "cough",
    "runny_nose",
    "sneezing",
    "sore_throat",
    "fatigue",
    "headache",
    "muscle_ache",
    "loss_of_appetite",
    "nausea",
    "diarrhoea",
    "vomiting",
    "fever",
    "night_sweats",
    "bone_ache",
    "chills",
    "concentration",
    "dizzy",
    "not_sleeping",
    "extra_bed",
    "out_of_bed",
    "daily_activities",
    "confusion",
    "blocked_nose",
    "sinus_pain",
    "dry_cough",
    "white_phlegm",
    "green_phlegm",
    "loss_of_smell",
    "loss_of_taste",
    "smell_taste",
    "swollen_tonsils",
    "swollen_glands",
    "ear_pain",
    "ear_fluid",
    "sob",
    "wheezing",
    "chest_pain_nc",
    "chest_pain_br",
    "abdo_pain",
    "redeye",
    "sticky_eye",
    "eye_pain",
    "eye_deter",
    "rash_allover",
    "rash_local"
  )


# symptoms1 <-
#   c(
#     "confirmedfever",
#     "cough",
#     "runny_nose",
#     "sneezing",
#     "sore_throat",
#     "smell_loss",
#     "diarrhoea",
#     "nausea",
#     "vomit",
#     "fatigue",
#     "headache",
#     "muscle_ache",
#     "joint_ache",
#     "appetite_loss",
#     "rash",
#     "ILI",
#     "ARI"
#   )
# 
# 
# all_resp <-
#   c(
#     "cough" ,
#     "runny_nose" ,
#     "sneezing" ,
#     "sore_throat" ,
#     "blocked_nose",
#     "sinus_pain",
#     "dry_cough",
#     "white_phlegm",
#     "green_phlegm"  ,
#     "loss_of_smell"  ,
#     "loss_of_taste" ,
#     "smell_taste",
#     "wheezing")
# 
# 
# 

s.main_resp <-
  c("cough",
    "dry_cough",
    "sore_throat",
    "blocked_nose",
    "runny_nose",
    "sneezing",
    "sinus_pain")

s.other_resp <-
  c(
    "white_phlegm",
    "green_phlegm",
    "loss_of_smell",
    "loss_of_taste",
    "smell_taste")

s.const <-
  c(
    #"confirmedfever",
    "fever",
    "fatigue",
    "headache",
    "muscle_ache",
    #"joint_ache",
    #"bone_ache",
    #"appetite_loss",
   # "loss_of_appetite"
    "loss_of_smell",
    "loss_of_taste"
    #"night_sweats",
    #"chills",
    #"dizzy",
    #"not_sleeping",
    #"abdo_pain",
    #"eye_pain"
  )

s.gastro <-
  c("nausea",
    #"vomit",
    "vomiting",
    "diarrhoea")

s.anosmia <- c("loss_of_smell", "loss_of_taste")


s_list <- mget(ls(pattern = "^s\\."))
rm(list = ls(pattern = "^s\\."))
# 
# 
# 
# fw_vw_demographics <- c("illnessid", "individual_id", "nVar")
# 
# 
# vw_demographics <- c("illnessid", "individual_id", "household_id", "nVar", "age2", "age3", "hh_age_on_entry", "sex_bin", "region")
# 
# cat(cyan("symptoms vectors are stored under: \n"),
#     "vector_list:\n")
# 
# print(vector_list)

