

# BIND DATA ---------------------------------------------------------------

common <- intersect(colnames(fluwatch),#cols from fluwatch
                    colnames(vw_pos)) 

s_list[["s.fw_vw_all"]] <- common %>% 
  setdiff(., c("illnessid", "individual_id", "nVar", "age3"))

fw_vw <- rbind(fluwatch %>% select(common),
               vw_pos %>% select(common)) %>% 
  mutate(across(s_list[["s.fw_vw_all"]], ~ifelse(is.na(.), 0, .) %>% as.numeric(.) )) %>% 
  mutate(virus = ifelse( nVar %in% unique(vw_pos$nVar), "SARS-CoV-2", nVar))



f.fw_vw <- get_freq(data = fw_vw,
                    symptoms = s_list[["s.fw_vw_all"]] ,
                    virus) %>% 
  filter(symptom %in% s_list[["s.fw_vw_main"]]) %>% 
  mutate(virus = ifelse(virus == "seasonal coronavirus", "seasonal CoV", virus ),
         symptom = factor(symptom, 
                          levels = s_list[["s.fw_vw_main"]]))












# PLOT --------------------------------------------------------------------


.FW_VW_pal <- c(
  "SARS-CoV-2" = "red",
  "influenza" = "steelblue",
  "RSV" = "forestgreen",
  "rhinovirus" = "orange",
  "seasonal CoV" = "#690fad")

.FW_VW_lty <- c(
  "SARS-CoV-2" = "solid",
  "influenza" = "solid",
  "RSV" = "solid",
  "rhinovirus" = "solid",
  "seasonal CoV" = "solid")


yaxis <- tibble(y = seq(0,100, 20),
                ylab = paste0(y, "%"))

p <- f.fw_vw %>% 
  ggplot(aes(x = symptom , 
             y = frequency, 
             group =virus)) + 
  
  geom_ribbon(aes(
    fill = virus,
    ymin = lower_ci, 
    ymax = upper_ci), 
    alpha = 0.2, 
    show.legend = FALSE) +
  
  geom_line(aes(
    color = virus,
    lty = virus )) +
  
  geom_point(aes(
    color = virus), 
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
  
  annotate(geom = "line", 
           x = 1:4 , 
           y = 100, 
           size = 1,
           col = "steelblue")+
  
  annotate(geom = "richtext", 
           x = median(1:4) , 
           y = 100, label = "Respiratory", 
           col = "steelblue", 
           angle = -95 )+
  
  annotate(geom = "line", 
           x = 4:7 , 
           y = 100, 
           size = 1,
           col = "#B97D4B")+
  
  annotate(geom = "richtext", 
           x = median(4:7) , 
           y = 100, label = "Constitutional", 
           col = "#B97D4B", 
           angle = 95 )+
  
  scale_y_continuous(breaks = seq(0,100, 20))+
  scale_color_manual(values = .FW_VW_pal)+
  scale_fill_manual(values =.FW_VW_pal)+
  scale_linetype_manual(values = .FW_VW_lty)+
  
  theme_light()+
  
  ggplot2::coord_polar(clip = "off")+
  .polar_theme+
  theme(legend.position = "bottom")+
  labs(colour = "Virus", lty = "Virus")


p

ggsave(p, 
       width = width,
       height = height,
       unit = "cm", 
       file = paste0(save_path, "/figure 1.svg"))



# TABLE -------------------------------------------------------------------

f.fw_vw %>% 
  mutate(across(where(is.numeric), ~round(., digits = 1))) %>% 
  mutate(frequency = paste0(frequency,"(", lower_ci, "-", upper_ci, ")")) %>% 
  select(-c("lower_ci", "upper_ci")) %>% 
  select(-denominator) %>% 
  pivot_wider(names_from = virus, values_from = frequency) %>% 
  relocate(c("symptom", "SARS-CoV-2", "seasonal CoV")) %>% 
  flextable() %>% 
  flextable::save_as_docx(path = paste0(save_path, "/freqs_fw_vw.docx"))

width = 30
height = (9/16) * width

ggsave(p, 
       width = width,
       height = height,
       unit = "cm", 
       file = paste0(save_path, "/freqs_fw_vw.svg"))

