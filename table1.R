

common <- intersect(colnames(fluwatch),#cols from fluwatch
                    colnames(vw_pos)) 

s_list[["s.fw_vw_all"]] <- common %>% 
  setdiff(., c("illnessid", "individual_id", "nVar", "age3"))

fw_vw <- rbind(fluwatch %>% select(common),
               vw_pos %>% select(common)) %>% 
  select(c("illnessid","individual_id","age3","nVar",s_list[["s.fw_vw_main"]],"ARI", "ILI",)) %>% 
  mutate(across(c(s_list[["s.fw_vw_main"]],"ARI", "ILI"), ~ifelse(is.na(.), 0, .) %>% as.numeric(.) )) %>% 
  mutate(virus = ifelse( nVar %in% unique(vw_pos$nVar), "SARS-CoV-2", nVar)) %>% 
  mutate(virus = factor(virus, levels = c("influenza", "rhinovirus","RSV", "seasonal CoV", "SARS-CoV-2")),
         nVar = as.character(nVar),
         nVar = factor(nVar, levels = c("influenza", "rhinovirus","RSV", "seasonal CoV", "Wild Type", "Alpha", "Delta", "Omicron BA1", "Omicron BA2", "Omicron BA5")))



fw_vw %>% 
  select(-illnessid, -individual_id, -virus) %>%
  relocate(age3, .after = ILI) %>% 
  tbl_summary(by = nVar,
              missing = "ifany",
              missing_text = "Missing",
              statistic =list(all_categorical() ~ "{p}%")) %>% 
  add_ci(pattern = "{stat} [{ci}]") %>% 
  add_overall() %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = paste0(save_path, "table1.docx"))

# as_gt(tab) %>% 
#   gtsave(filename = "table_demographics.html", path = save_path )



freqs <- get_freq(data = fw_vw,
                  symptoms = fw_vw %>% select(where(is.numeric)) %>% names() ,
                  nVar) %>% 
  filter(symptom %in% s_list[["s.fw_vw_main"]]) %>% 
  mutate(symptom = factor(symptom,levels = s_list[["s.fw_vw_main"]]),
         across(where(is.numeric), ~round(., digits = 0))) %>% 
  as_tibble()


freqs %>%  
  filter(symptom %in% c("runny_nose", "sore_throat", "sneezing")) %>% View() 
select(-c("lower_ci", "upper_ci")) %>% 
  pivot_wider(names_from = symptom, values_from = c("frequency")) %>% 
  slice(5:10) %>% 
  mutate(agg = (sore_throat + runny_nose + sneezing) / 3,
         cumdiff = agg - lag(agg))

p <- freqs %>% 
  ggplot()+
  aes(x = frequency, y = symptom, col = nVar)+
  geom_point()+
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci))

plotly::ggplotly(p)
