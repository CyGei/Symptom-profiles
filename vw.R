
.VW_lty <- c(
  "Wild Type" = "dotted",
  "Alpha" = "dotdash",
  "Delta" = "longdash",
  "Omicron BA1" = "solid",
  "Omicron BA2" = "solid",
  "Omicron BA5" = "solid")

# VW DATA -----------------------------------------------------------------

vw <- vw_pos %>%
  select(
    illnessid,
    household_id,
    newID,
    sex_bin,
    hh_age_on_entry,
    age3,
    region,
    nVar,
    start_dt2,
    s_list[["s.cov_all"]]
  ) %>% 
  dplyr::mutate(across(s_list[["s.cov_all"]], ~replace_na(., 0))) %>% 
  mutate(
    ARI = ifelse(cough == 1 | sore_throat == 1 | runny_nose == 1, 1, 0),
    ILI = ifelse(fever == 1 & cough == 1, 1, 0),
    nVar = factor(nVar, levels = c("Wild Type", "Alpha", "Delta", "Omicron BA1", "Omicron BA2", "Omicron BA5"))
  )






# ALL ---------------------------------------------------------------------

f.vw <- get_freq(data = vw,
         symptoms =   s_list[["s.cov_all"]],
         nVar) %>% 
  as_tibble() %>% 
  mutate(  nVar = factor(nVar, levels = c("Wild Type", "Alpha", "Delta", 
                                          "Omicron BA1", "Omicron BA2", 
                                          "Omicron BA5"))) %>% 
  filter(symptom %in% c(s_list$s.main_resp,s_list$s.const)) %>% 
  mutate(group = case_when(symptom %in% s_list$s.main_resp ~ "main respiratory",
                           symptom %in% s_list$s.const ~ "constitutional"),
         symptom = factor(symptom, levels = c( s_list$s.main_resp,  s_list$s.const) ))
f.vw


max_y<- max(f.vw$upper_ci)

yaxis <- tibble(y = seq(0,max_y, 20),
                ylab = paste0(y, "%"))

p <- f.vw %>% 
  ggplot(aes(x = symptom , 
             y = frequency, 
             group = nVar)) + 
  
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
  
  
  geom_hline(yintercept = seq(0,max_y, 20), colour = "gray", lty = "solid") +
  
  geom_segment(aes(yend = max_y, y = 0, xend = symptom),
             colour = "gray", lty = "solid") +
  
  geom_label(data = yaxis, 
             aes(x = 0.5, 
                 y = y, 
                 group = 1, 
                 label = ylab),
             fill = "white") +
  
  annotate(geom = "line", 
           x = 1:length(s_list$s.main_resp) , 
           y = max_y, 
           size = 1,
           col = "steelblue")+
  
  annotate(geom = "richtext", 
           x = length(s_list$s.main_resp)/2 , 
           y = max_y, label = "Respiratory", 
           col = "steelblue", 
           angle = -90 )+
  
  annotate(geom = "line", 
           x = 7:13 , 
           y = max_y, 
           size = 1,
           col = "#B97D4B")+
  
  annotate(geom = "richtext", 
           x = median(7:13)-0.6 , 
           y = max_y, label = "Constitutional", 
           col = "#B97D4B", 
           angle = 108 )+
  
  
  scale_color_manual(values = .VW_pal)+
  scale_fill_manual(values  = .VW_pal)+
  scale_linetype_manual(values = .VW_lty)+
  theme_light()+
  ggplot2::coord_polar(clip = "off")+
  .polar_theme+

#  scale_y_continuous(breaks = seq(0,100, 20))+
 
  theme(legend.position = "bottom")+
  labs(colour = "", lty = "")



width = 30
height = (9/16) * width

ggsave(p, 
       width = width,
       height = height,
       unit = "cm", 
       file = paste0(save_path, "/freqs_vw.svg"))




# 
# s_list %>% names()
# s_vec <-
#   c("s.main_resp",
#     "s.other_resp",
#     "s.const",
#     "s.gastro",
#   )
# 
# labs = gsub("s.", "", s_vec)
# 
# ### plotting frequencies by symptoms with without age
# fig_list <- list()
# for (i in svec) {
#   temp <- get_freq(data = vwatch, 
#                    symptoms = vector_list[[i]], 
#                    nVar)
#   fig_list[[i]] <- plot_freq(temp , group_var = nVar, type = "bar")+
#     theme(legend.position = "none")
# }
# ggarrange(plotlist = fig_list, labels = labs)
# 
# 
# ### plotting frequencies by symptoms with  age
# fig_list <- list()
# for (i in svec) {
#   temp <- get_freq(data = vwatch, 
#                    symptoms = vector_list[[i]], 
#                    nVar, age3)
#   fig_list[[i]] <- plot_freq(temp , group_var = nVar, type = "bar")+
#     theme(legend.position = "none")+
#     facet_wrap(~age3) 
# }
# 
# ggarrange(plotlist = fig_list, labels = labs)
