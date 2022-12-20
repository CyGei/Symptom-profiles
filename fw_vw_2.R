
common <- intersect(colnames(fluwatch),#cols from fluwatch
                    colnames(vw_pos)) 

s_list[["s.fw_vw_all"]] <- common %>% 
  setdiff(., c("illnessid", "individual_id", "nVar", "age3"))

fw_vw <- rbind(fluwatch %>% select(common),
               vw_pos %>% select(common)) %>% 
  mutate(across(s_list[["s.fw_vw_all"]], ~ifelse(is.na(.), 0, .) %>% as.numeric(.) ))

f.fw_vw <- get_freq(data = fw_vw,
                    symptoms = s_list[["s.fw_vw_all"]] ,
                    nVar) %>% 
  filter(symptom %in% s_list[["s.fw_vw_main"]]) %>% 
  mutate(nVar = ifelse(nVar == "seasonal coronavirus", "seasonal CoV", nVar ),
         symptom = factor(symptom, 
                          levels = s_list[["s.fw_vw_main"]]))







# PLOT --------------------------------------------------------------------

.FW_VW_pal <- c(
  "Wild Type" = "black",       
  "Alpha" = "#F1A2CF",           
  "Delta" = "#CA58AE",            
  "Omicron BA1" = "#971A82",
  "Omicron BA2" = "#690fad",
  "Omicron BA5" = "red",
  "influenza" = "steelblue",
  "RSV" = "forestgreen",
  "rhinovirus" = "orange",
  "seasonal CoV" = "darkgrey")

.FW_VW_lty <- c(
  "Wild Type" = "dotted",       
  "Alpha" =  "dotted",           
  "Delta" =  "dotted",            
  "Omicron BA1" =  "dashed",
  "Omicron BA2" =  "dashed",
  "Omicron BA5" =  "dashed",
  "influenza" = "solid",
  "RSV" = "solid",
  "rhinovirus" = "solid",
  "seasonal CoV" = "solid")



yaxis <- tibble(y = seq(0,100, 20),
                ylab = paste0(y, "%"))

p <- f.fw_vw %>% 
  ggplot(aes(x = factor(symptom, 
                        levels = s_list[["s.fw_vw_main"]]), 
             y = frequency, 
             group =nVar)) + 
  
  geom_ribbon(aes(
    fill = nVar,
    ymin = lower_ci, 
    ymax = upper_ci), 
    alpha = 0.2, 
    show.legend = FALSE) +
  
  geom_line(aes(
    color = nVar,
    lty = nVar )) +
  
  geom_point(aes(
    color = nVar), 
    show.legend = FALSE) +
  
  
  geom_hline(yintercept = seq(0,100, 20), colour = "gray", lty = "solid")+
  
  geom_vline(xintercept = c(1:length(s_list[["s.fw_vw_main"]])),
             colour = "gray", lty = "solid") +
  
  geom_label(data = yaxis, 
             aes(x = 0.5, 
                 y = y, 
                 group = 1, 
                 label = ylab),
             fill = "white")+
  
  scale_y_continuous(breaks = seq(0,100, 20))+
  scale_color_manual(values = .FW_VW_pal)+
  scale_fill_manual(values =.FW_VW_pal)+
  scale_linetype_manual(values = .FW_VW_lty)+
  
  theme_light()+
  
  ggplot2::coord_polar()+
  .polar_theme+
  labs(colour = "Virus", lty = "Virus")


p

# TABLE -------------------------------------------------------------------

f.fw_vw %>% 
  mutate(across(where(is.numeric), ~round(., digits = 1))) %>% 
  mutate(frequency = paste0(frequency,"(", lower_ci, "-", upper_ci, ")")) %>% 
  select(-c("lower_ci", "upper_ci")) %>% 
  select(-denominator) %>% 
  pivot_wider(names_from = nVar, values_from = frequency) %>% 
  relocate(c("symptom", "influenza", "RSV", "rhinovirus", "seasonal CoV")) %>% 
  flextable() %>% 
  flextable::save_as_docx(path = paste0(save_path, "/freqs_fw_vw_2.docx"))

width = 30
height = (9/16) * width

ggsave(p, 
       width = width,
       height = height,
       unit = "cm", 
       file = paste0(save_path, "/freqs_fw_vw_2.svg"))

