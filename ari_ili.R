f.fw_vw_ari_ili <- get_freq(data = fw_vw,
         symptoms = c("ILI", "ARI"),
         nVar) %>%
  as_tibble() %>% 
  mutate(nVar = factor(
    nVar,
    levels = c(
      "influenza",
      "rhinovirus",
      "RSV",
      "seasonal CoV",
      "Wild Type",
      "Alpha",
      "Delta",
      "Omicron BA1",
      "Omicron BA2",
      "Omicron BA5"
    )
  ))

f.fw_vw_ari_ili %>% 
  tibble() %>% 
  filter(symptom =="ILI") %>% 
  filter(nVar != "influenza") %>% 
  summarise(across(c(frequency, lower_ci, upper_ci), ~mean(.)))


p <- f.fw_vw_ari_ili  %>% 
  ggplot(aes(x = symptom, 
             y = frequency, 
             group = nVar, 
             fill = nVar)) +
  geom_col(position = position_dodge(), colour = "black")+
  #geom_point(position = position_dodge(width = 0.9), col = "black")+
  geom_errorbar(aes(x = symptom, ymin = lower_ci, ymax = upper_ci), width = 0.4,position = position_dodge(0.9))+
  theme_minimal() +
  theme(text = element_text(size = 12))+
  labs(x = "", y = "")+
  scale_color_manual(values = .FW_VW_var_pal)+
  scale_fill_manual(values = .FW_VW_var_pal)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 15))+
  labs(col = "Virus", fill = "Virus")

ggsave(p, 
       width = width,
       height = height,
       unit = "cm", 
       file = paste0(save_path, "/figure 2.svg"))

