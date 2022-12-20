

# FREQUENCIES -------------------------------------------------------------


#GET frequencies by group (...)
#1st input is the dataset,
#2nd input is the vector of symptoms e.g. c("fever", "cough"...)
#3rd input is the grouping variable, can be one e.g virustype or multiple
# e.g. virustype, age
get_freq<- function(data, symptoms, ...){
  data %>%
    group_by(...) %>%
    summarise(denominator = n(),
              across(all_of({{symptoms}}),  ~ sum(.x, na.rm = TRUE))) %>%
    mutate(across(all_of({{symptoms}}),
                  ~ DescTools::BinomCI(x = ., n = denominator) * 100)) %>%
    tidyr::pivot_longer(cols = all_of(symptoms),
                        names_to = "symptom",
                        values_to = "frequency") %>%
    t(.) %>% t(.) %>%
    as.data.frame(.) %>%
    dplyr::rename(frequency = frequency.1,
                  lower_ci = frequency.2,
                  upper_ci = frequency.3) %>%
    dplyr::mutate(across(c(frequency, lower_ci, upper_ci), as.numeric))
}





# PLOT --------------------------------------------------------------------


#Plot frequencies by group

.VW_pal <- c(
              "Wild Type" = "#FFB6C1",       
              "Alpha" = "#CA58AE",            
              "Delta" = "#BB2A9B",            
              "Omicron BA1" = "#971A82",
              "Omicron BA2" = "#690fad",
              "Omicron BA5" = "red")

.VW_lty <- c(
  "Wild Type" = "dotted",       
  "Alpha" =  "dotted",           
  "Delta" =  "dotted",            
  "Omicron BA1" =  "solid",
  "Omicron BA2" =  "solid",
  "Omicron BA5" =  "solid")

.FW_VW_pal <- c(
              "Wild Type" = "#FFB6C1",       
              "Alpha" = "#F1A2CF",           
              "Delta" = "#CA58AE",            
              "Omicron BA1" = "#971A82",
              "Omicron BA2" = "#690fad",
              "Omicron BA5" = "red",
              "influenza" = "steelblue",
              "RSV" = "forestgreen",
              "rhinovirus" = "orange",
              "seasonal coronavirus" = "darkgrey")

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
  "seasonal coronavirus" = "solid")


.FW_VW_var_pal <- c(
  "Wild Type" = "#FFB6C1",       
  "Alpha" = "#F1A2CF",           
  "Delta" = "#CA58AE",            
  "Omicron BA1" = "#971A82",
  "Omicron BA2" = "#690fad",
  "Omicron BA5" = "red",
  "influenza" = "steelblue",
  "RSV" = "forestgreen",
  "rhinovirus" = "orange",
  "seasonal CoV" = "darkgrey")

.polar_theme <- theme(panel.border = element_blank(),
                      panel.grid = element_blank(),
                      axis.title = element_blank(),
                      axis.text.x = element_text(size = 13),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      legend.background = element_rect(fill = "white", color = "black"))


plot_freq <- function(data, group_var, type){
  
  if(type == "polar"){
    #setting theme
    .polar_theme <- theme(
      text = element_text(size = 12),
      axis.text.x = element_text(color="black"),
      panel.grid.major.x = element_line(color="gray", size=0.1),
      panel.grid.major.y = element_line(color="black", size=0.1, linetype="solid"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.background = element_rect(fill = "white", color = "black"),
    )
    
    data %>% 
      ggplot(aes(x = symptom, 
                 y = frequency, 
                 group = {{group_var}}
                 )
             ) + 
      geom_ribbon(aes(
        fill = {{group_var}},
        ymin = lower_ci, 
        ymax = upper_ci), 
        alpha = 0.2, 
        show.legend = FALSE) +
      geom_line(aes(
        color = {{group_var}}),
        size = 0.85) +
      geom_point(aes(
        color = {{group_var}}), 
        show.legend = FALSE) +
      ggplot2::coord_polar() +
      theme_light() +
      .polar_theme +
      theme(text = element_text(size = 12))+
      labs(x = "", y = "")+
      scale_color_manual(values = .FW_VW_pal)+
      scale_fill_manual(values =.FW_VW_pal)
    } else
      if(type == "bar"){
    data %>% 
      ggplot(aes(x = symptom, 
               y = frequency, 
               group = {{group_var}}, 
               fill = {{group_var}})) +
      geom_col(position = position_dodge(), colour = "black")+
      geom_errorbar(aes(x = symptom, ymin = lower_ci, ymax = upper_ci), width = 0.4,position = position_dodge(0.9))+
      theme_minimal() +
      theme(text = element_text(size = 12))+
      labs(x = "", y = "")+
      scale_color_manual(values = .FW_VW_pal)+
      scale_fill_manual(values =.FW_VW_pal)
  }
  
}



# MODELS ------------------------------------------------------------------

#returns a dataframe of model results 
#where dependent variable is each symptoms
#independent variable =var1 & var2
get_models <- function(dataframe, symptoms, indep.vars ){
  
  indep.vars <- paste0(indep.vars, collapse = "+")
  formula <- paste0("dv_value", "~", indep.vars)
  
  models <- dataframe %>%
    gather(dv_name, dv_value, all_of(symptoms)) %>%
    group_by(dv_name) %>% 
    do(broom::tidy(
      glm(
        as.formula(formula),
        data = .,
        family = binomial(link = "logit")
      ),
      conf.int = TRUE
    )) %>% 
    dplyr::mutate(p_val = ifelse(p.value <= 0.05, "significant", "non-significant"),
                  odds = exp(estimate),
                  lower = exp(conf.low),
                  upper = exp(conf.high),
                  term = gsub("nVar", "", term)) 

  return(models)
}


get_r <- function(dataframe, symptoms, indep.vars ){
  
  indep.vars <- paste0(indep.vars, collapse = "+")
  formula <- paste0("dv_value", "~", indep.vars)
  
  data <- dataframe %>%
    gather(dv_name, dv_value, all_of(symptoms))
  
  m <- glm(
        as.formula(formula),
        data = data,
        family = binomial(link = "logit"))
  
  r <- 1 - (m$deviance / m$null.deviance)

  return(r)
}



plot_models <- function(models){
  p <- models %>% 
    ggplot()+
    aes(odds, term, color = p_val)+
    geom_point()+
    geom_errorbarh(aes(xmin = lower, xmax = upper))+
    geom_vline(aes(xintercept = 1), lty = "dotted")+
    facet_wrap(~dv_name)+
    theme_bw()+
    scale_color_manual(values = c("darkgrey", "#d004a8"))+
    labs(x = "", y = "")
  
  return(p)
}


