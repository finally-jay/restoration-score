outpath <- "../results/RS_supplementary_analyses"
dir.create(outpath)

#---------------------------------------------------------------------
df <- variables

###glm
glm_asthma <- glm(data = variables%>% filter(j45_6yr_transient == 0), j45_6yr_ever ~ scale(restoration_score), family = binomial(link = "logit")) %>% 
  broom::tidy(conf.int = T) %>%
  subset(term == "scale(restoration_score)") %>% 
  dplyr::mutate(across(c(estimate, conf.low, conf.high), exp))
glm_asthma$label = "Asthma"


glm_asthma_adj <- glm(data = variables %>% filter(j45_6yr_transient == 0), j45_6yr_ever ~ scale(restoration_score) + age_days + hospitalization_yn + 
                        sex + season + race + ab_child_1yr_ever + oldchild_yn + mother_astdoc + father_astdoc , family = binomial(link = "logit")) %>% 
  broom::tidy(conf.int = T) %>%
  subset(term == "scale(restoration_score)") %>% 
  dplyr::mutate(across(c(estimate, conf.low, conf.high), exp))
glm_asthma_adj$label = "Asthma (adjusted)"


glm_birthmode <- glm(data = variables, delivery ~ scale(restoration_score), family = binomial(link = "logit")) %>% 
  broom::tidy(conf.int = T) %>%
  subset(term == "scale(restoration_score)") %>% 
  dplyr::mutate(across(c(estimate, conf.low, conf.high), exp))
glm_birthmode$label = "C-section"

glm_oldsibling <- glm(data = variables, oldchild_yn ~ scale(restoration_score), family = binomial(link = "logit")) %>% 
  broom::tidy(conf.int = T) %>%
  subset(term == "scale(restoration_score)") %>% 
  dplyr::mutate(across(c(estimate, conf.low, conf.high), exp))

glm_oldsibling$label = "Having older siblings"

glm <- rbind(glm_asthma, glm_birthmode) %>% rbind(glm_asthma_adj) %>% rbind(glm_oldsibling) %>% as.data.frame()
glm$cohort = "COPSAC"
glm$order = c(3,1,4,2)

saveRDS(glm, paste0(outpath, "/03.glm_COPSAC.rds"))