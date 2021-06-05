#########################
####                 ####
####    Regression   ####
####                 ####
#########################

#########################
####                 ####
####   1. LDI Mean   ####
####                 ####
#########################

############################
#### OLS, simple models ####
############################

#### World, region and neighbor level diffusion ####

## World level diffusion 
summary(m1.1 <- lm(delta_libdem ~ mean_world_change_lag1,
                         data = vdem3))
summary(m1.1_rev <- lm(delta_libdem_rev ~ mean_world_change_rev_lag1, 
                     data = vdem3)) 
stargazer(m1.1, m1.1_rev, type = "text")

## Regional mean 
summary(m2.1 <- lm(delta_libdem ~ mean_region_change_lag1, 
                 data = vdem3))
summary(m2.1_rev <- lm(delta_libdem_rev ~ mean_region_change_rev_lag1, 
                       data = vdem3))
stargazer(m2.1, m2.1_rev, type = "text")

## Neighbor mean
summary(m3.1 <- lm(delta_libdem ~ mean_neighbor_change_lag1, 
                   data = neighbor_mean))
summary(m3.1_rev <- lm(delta_libdem_rev ~ mean_neighbor_change_rev_lag1, 
                       data = neighbor_mean))
stargazer(m3.1, m3.1_rev, type = "text")

stargazer(m1.1_rev, m2.1_rev, m3.1_rev, type = "text")
#### Regression outputs ####

## Mean world, region, neighbor
stargazer(m1.1_rev, m2.1_rev, m3.1_rev, 
          type = "latex",
          out = "1OLS_simple.tex",
          dep.var.labels = "Change in LDI",
          covariate.labels = c("Mean World Change LDI lag", 
                               "Mean Region Change LDI lag", 
                               "Mean Neighbor Change LDI lag"), 
          digits = 3, 
          title = "Regression Table Diffusion at Three Levels", 
          column.sep.width = "-10pt", 
          font.size = "footnotesize", 
          no.space = TRUE)
          
#### Scatterplots with regression line #### 

## World mean LDI 
ggplot(vdem3, aes(x = mean_world_change_rev_lag1, y = delta_libdem_rev)) + 
  geom_point(alpha=0.3) +
  labs(x = "Mean world change", 
       y = "Change in Liberal Democracy Index", 
       title = "Bivariate distribution of world diffusion and change in LDI") +
  theme_bw() +
  geom_smooth(method = "lm", col = "red") +
  geom_vline(xintercept = mean(vdem3$mean_world_change_lag1, na.rm = T), linetype = "dashed") +
  geom_hline(yintercept = mean(vdem3$delta_libdem, na.rm = T), linetype = "dashed")

ggsave("scatterplot_regression_line_world_LDI.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Region mean LDI 
ggplot(vdem3, aes(x = mean_region_change_rev_lag1, y = delta_libdem_rev)) +
  geom_point(alpha=0.3) +
  labs(x = "Mean change in region", 
       y = "Change in Liberal Democracy Index", 
       title = "Bivariate distribution of regional diffusion and change in LDI") +
  theme_bw() +
  geom_smooth(method = "lm", col = "red") +
  geom_vline(xintercept = mean(vdem3$mean_region_change_lag1, na.rm = T), linetype = "dashed") +
  geom_hline(yintercept = mean(vdem3$delta_libdem, na.rm = T), linetype = "dashed")

ggsave("scatterplot_regression_line_region_LDI.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Neighbor mean LDI 
ggplot(neighbor_mean, aes(x = mean_neighbor_change_rev_lag1, y = delta_libdem_rev)) +
  geom_point(alpha=0.3) +
  labs(x = "Mean change among neighbors", 
       y = "Change in Liberal Democracy Index", 
       title = "Bivariate distribution of neighbor diffusion and change in LDI") +
  theme_bw() +
  geom_smooth(method = "lm", col = "red") +
  geom_vline(xintercept = mean(neighbor_mean$mean_neighbor_change_lag1, na.rm = T), linetype = "dashed") +
  geom_hline(yintercept = mean(neighbor_mean$delta_libdem, na.rm = TRUE), linetype = "dashed")

ggsave("scatterplot_regression_line_neighbor_LDI.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

# Konfidensintervall til regresjonsmodellen 
confint(m1.1)
confint(m2.1)
confint(m3.1)

################################################
#### Full OLS: Change in level of democracy ####
################################################

## Mean world
summary(full_mod_OLS_world1 <- lm(delta_libdem_rev ~ mean_world_change_rev_lag1 +
                                    delta_libdem_rev_lag1 + # Lagged DV
                                    e_migdpgrolns + # Ln GDP growth
                                    population + # Ln population
                                    e_peaveduc + # Education
                                    e_civil_war, # Civil war
                                 data = vdem3))
summary(full_mod_OLS_world2 <- lm(delta_libdem_rev ~ mean_world_change_rev_lag1 +
                                    delta_libdem_rev_lag1 + # Lagged DV
                                    e_migdpgrolns + # Ln GDP Growth
                                    population + # Ln population
                                    e_peaveduc + # Education
                                    e_civil_war + # Civil War
                                    e_miurbani, # Urbanization
                                  data = vdem3))
summary(full_mod_OLS_world3 <- lm(delta_libdem_rev ~ mean_world_change_rev_lag1 + 
                                    delta_libdem_rev_lag1 + # Lagged DV
                                    e_migdpgrolns + # Ln GDP Growth
                                    population + # Ln population
                                    e_peaveduc + # Education
                                    e_civil_war + # Civil War
                                    e_miurbani + # Urbanization
                                    e_pt_coup_n, # Coup d'etat
                                  data = vdem3))

stargazer(full_mod_OLS_world1, full_mod_OLS_world2, full_mod_OLS_world3, 
          type = "text")

## Mean region
summary(full_mod_OLS_region1 <- lm(delta_libdem_rev ~ mean_region_change_rev_lag1 + 
                                     delta_libdem_rev_lag1 +
                                     e_migdpgrolns +
                                     population + 
                                     e_peaveduc +
                                     e_civil_war, 
                                   data = vdem3))
summary(full_mod_OLS_region2 <- lm(delta_libdem_rev ~ mean_region_change_rev_lag1 + 
                                     delta_libdem_rev_lag1 + 
                                     e_migdpgrolns + 
                                     population +
                                     e_peaveduc + 
                                     e_civil_war +
                                     e_miurbani, 
                                   data = vdem3))
summary(full_mod_OLS_region3 <- lm(delta_libdem_rev ~ mean_region_change_rev_lag1 + 
                                     delta_libdem_rev_lag1 + 
                                     e_migdpgrolns + 
                                     population +
                                     e_peaveduc + 
                                     e_civil_war +
                                     e_miurbani + 
                                     e_pt_coup_n, 
                                   data = vdem3))

stargazer(full_mod_OLS_region1, full_mod_OLS_region2, full_mod_OLS_region3, 
          type = "text")

## Mean neighbor
summary(full_mod_OLS_neighbor1 <- lm(delta_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                                       delta_libdem_rev_lag1 +
                                       e_migdpgrolns + 
                                       population + 
                                       e_peaveduc + 
                                       e_civil_war,
                                     data = neighbor_mean))
summary(full_mod_OLS_neighbor2 <- lm(delta_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                                       delta_libdem_rev_lag1 +
                                       e_migdpgrolns + 
                                       population +
                                       e_peaveduc +
                                       e_civil_war + 
                                       e_miurbani,
                                     data = neighbor_mean))
summary(full_mod_OLS_neighbor3 <- lm(delta_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                                       delta_libdem_rev_lag1 +
                                       e_migdpgrolns + 
                                       population +
                                       e_peaveduc +
                                       e_civil_war + 
                                       e_miurbani + 
                                       e_pt_coup_n, 
                                     data = neighbor_mean))

stargazer(full_mod_OLS_neighbor1, full_mod_OLS_neighbor2, full_mod_OLS_neighbor3, 
          type = "text")

## Presetning all the outputs together
stargazer(full_mod_OLS_world1, full_mod_OLS_world2, full_mod_OLS_world3, 
          full_mod_OLS_region1, full_mod_OLS_region2, full_mod_OLS_region3,
          full_mod_OLS_neighbor1, full_mod_OLS_neighbor2, full_mod_OLS_neighbor3,
          type = "latex",
          out = "full_mod_all.tex", 
          dep.var.labels = "Change in LDI", 
          covariate.labels = c("Mean World Change LDI lag", 
                               "Mean Region Change LDI lag", 
                               "Mean Neighbor Change LDI lag",
                               "Change in LDI lag",
                               "Ln GDP Growth", 
                               "Ln Population",
                               "Education",
                               "Civil War",
                               "Urbanization",
                               "Coup d'etat"), 
          title = "Regression Table With Control Variables", 
          font.size = "footnotesize", 
          column.sep.width = "-10pt",
          omit.stat = c("f", "ser"),
          no.space = TRUE, 
          float.env = "sidewaystable")

#### Fixed effects models ####

## World mean, country FE
summary(full_mod_FE_world <- lm(delta_libdem_rev ~ mean_world_change_rev_lag1 +
                                  delta_libdem_rev_lag1 + # Lagged DV  
                                  e_migdpgrolns + # Ln GDP growth 
                                  population + # Ln Population
                                  e_peaveduc + # Education 
                                  e_civil_war + # Civil war
                                  e_miurbani + # Urbanization
                                  as.factor(country_id),
                                data = vdem3))
summary(full_mod_FE_world2 <- lm(delta_libdem_rev ~ mean_world_change_rev_lag1 +
                                   delta_libdem_rev_lag1 + # Lagged DV  
                                   e_migdpgrolns + # Ln GDP growth 
                                   population + # Ln Population
                                   e_peaveduc + # Education 
                                   e_civil_war + # Civil war
                                   e_miurbani + # Urbanization
                                   e_pt_coup_n + # Coup d'etat
                                   as.factor(country_id),
                                 data = vdem3))
stargazer(full_mod_FE_world, full_mod_FE_world2,
          type = "text", 
          omit = "country_id")

## Region mean, country and year FE
summary(full_mod_FE_region <- lm(delta_libdem_rev ~ mean_region_change_rev_lag1 + 
                                   delta_libdem_rev_lag1 + # Lagged DV  
                                   e_migdpgrolns + # Ln GDP growth 
                                   population + # Ln Population
                                   e_peaveduc + # Education 
                                   e_civil_war + # Civil war
                                   e_miurbani + # Urbanization
                                   #e_pt_coup_n + # Coup d'etat
                                   as.factor(year) + 
                                   as.factor(country_id),
                                 data = vdem3))
summary(full_mod_FE_region2 <- lm(delta_libdem_rev ~ mean_region_change_rev_lag1 + 
                                   delta_libdem_rev_lag1 + # Lagged DV  
                                   e_migdpgrolns + # Ln GDP growth 
                                   population + # Ln Population
                                   e_peaveduc + # Education 
                                   e_civil_war + # Civil war
                                   e_miurbani + # Urbanization
                                   e_pt_coup_n + # Coup d'etat
                                   as.factor(year) + 
                                   as.factor(country_id),
                                 data = vdem3))
stargazer(full_mod_FE_region, full_mod_FE_region2,
          type = "text",
          omit = c("year", "country_id"))

## Neighbor mean, country and year FE
summary(full_mod_FE_neighbor <- lm(delta_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                                     delta_libdem_rev_lag1 + # Lagged DV  
                                     e_migdpgrolns + # Ln GDP growth 
                                     population + # Ln Population
                                     e_peaveduc + # Education 
                                     e_civil_war + # Civil war
                                     e_miurbani + # Urbanization
                                     #e_pt_coup_n + # Coup d'etat
                                     as.factor(year) + 
                                     as.factor(country_id),
                                   data = neighbor_mean))
summary(full_mod_FE_neighbor2 <- lm(delta_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                                     delta_libdem_rev_lag1 + # Lagged DV  
                                     e_migdpgrolns + # Ln GDP growth 
                                     population + # Ln Population
                                     e_peaveduc + # Education 
                                     e_civil_war + # Civil war
                                     e_miurbani + # Urbanization
                                     e_pt_coup_n + # Coup d'etat
                                     as.factor(year) + 
                                     as.factor(country_id),
                                   data = neighbor_mean))
stargazer(full_mod_FE_neighbor, full_mod_FE_neighbor2,
          type = "text", 
          omit = c("year", "country_id"))

#### Clustered standard errors ####

## Creating robust standard errors on country
cluster_FE1 <- sqrt(diag(cluster.vcov(full_mod_FE_world2, cluster = vdem3$country_id)))
cluster_FE2 <- sqrt(diag(cluster.vcov(full_mod_FE_region2, cluster = vdem3$country_id)))
cluster_FE3 <- sqrt(diag(cluster.vcov(full_mod_FE_neighbor2, cluster = neighbor_mean$country_id)))

## Saving the clustered standard errors as part of the regression models
full_mod_FE_world2$clusterVCOV <- cluster_FE1
full_mod_FE_region2$clusterVCOV <- cluster_FE2
full_mod_FE_neighbor2$clusterVCOV <- cluster_FE3

## Combination of FE without and with clustered standard errors 
stargazer(full_mod_FE_world2, full_mod_FE_region2, full_mod_FE_neighbor2, 
          full_mod_FE_world2, full_mod_FE_region2, full_mod_FE_neighbor2,  
          type = "latex",
          out = "1full_mod_FE_cluster.tex", 
          omit = c("country_id", "year"),
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"),
          dep.var.labels = "Change in LDI", 
          covariate.labels = c("Mean World Change LDI lag", 
                               "Mean Region Change LDI lag", 
                               "Mean Neighbor Change LDI lag", 
                               "Change in LDI lag",
                               "Ln GDP Growth", 
                               "Ln Population",
                               "Education",
                               "Civil War",
                               "Urbanization",
                               "Coup d'etat"), 
          title = "Fixed Effects Without and With Clustered Standard Errors", 
          font.size = "footnotesize", 
          column.sep.width = "-10pt",
          no.space = TRUE, 
          se = list(NULL, NULL, NULL, cluster_FE1, cluster_FE2, cluster_FE3), 
          float.env = "sidewaystable", 
          omit.stat = "f")

#################################
#### Logistic, simple models ####
#################################

#### World, region and neighbor level diffusion ####

## World mean
summary(logit1.1 <- glm(autocrat_dummy_libdem_rev ~ mean_world_change_rev_lag1, 
                        data = vdem3, 
                        family = binomial(link = "logit")))
stargazer(logit1.1, type = "text")

## Regional mean
summary(logit2.1 <- glm(autocrat_dummy_libdem_rev ~ mean_region_change_rev_lag1, 
                        data = vdem3, 
                        family = binomial(link = "logit")))

## Neighbor mean 
summary(logit3.1 <- glm(autocrat_dummy_libdem_rev ~ mean_neighbor_change_rev_lag1, 
                        data = neighbor_mean, 
                        family = binomial(link = "logit")))

## Presenting the results 
stargazer(logit1.1, logit2.1, logit3.1, 
          type = "latex", 
          out = "1logit_simple.tex",
          dep.var.labels = "LDI Autocratization Dummy", 
          covariate.labels = c("Mean World Change LDI lag", 
                               "Mean Region Change LDI lag", 
                               "Mean Neighbor Change LDI lag"), 
          digits = 3, 
          title = "Logistic Regression Table Diffusion at Three Levels", 
          font.size = "footnotesize")#, 
          #apply.coef = exp)

exp(17.114) #27,071,715
(exp(17.114)-1)*100 

exp(5.363) # 213 times as high odds
(exp(5.363)-1)*100 # 21,236% higher odds 

exp(2.702) # 14.9
(exp(2.702)-1)*100 # = One units increase in mean change among neighbor countries' 
# LDI score gives 1390 times as high odds (1390% higher odds) for autocratization

#######################################
#### Full logistic: Autocrat dummy ####
#######################################

## World regression model with control variables and fixed effects
summary(full_mod_world_logit <- glm(autocrat_dummy_libdem_rev ~ mean_world_change_rev_lag1 + 
                                      delta_libdem_rev_lag1 +
                                      e_migdpgrolns + 
                                      population +
                                      e_peaveduc + 
                                      e_civil_war +
                                      e_miurbani +
                                      #e_pt_coup_n, 
                                      as.factor(country_id),
                                    data = vdem3,
                                    family = binomial(link = "logit")))
summary(full_mod_world_logit2 <- glm(autocrat_dummy_libdem_rev ~ mean_world_change_rev_lag1 + 
                                      delta_libdem_rev_lag1 +
                                      e_migdpgrolns + 
                                      population +
                                      e_peaveduc + 
                                      e_civil_war +
                                      e_miurbani +
                                      e_pt_coup_n +
                                      as.factor(country_id),
                                    data = vdem3,
                                    family = binomial(link = "logit")))

stargazer(full_mod_world_logit, full_mod_world_logit2, 
          type = "text", 
          omit = "country_id")

# Region regression model with control variables
summary(full_mod_region_logit <- glm(autocrat_dummy_libdem_rev ~ mean_region_change_rev_lag1 + 
                                       delta_libdem_rev_lag1 +
                                       e_migdpgrolns + 
                                       population +
                                       e_peaveduc + 
                                       e_civil_war +
                                       e_miurbani +
                                       #e_pt_coup_n +
                                       as.factor(year) + 
                                       as.factor(country_id),
                                     data = vdem3,
                                     family = binomial(link = "logit")))
summary(full_mod_region_logit2 <- glm(autocrat_dummy_libdem_rev ~ mean_region_change_rev_lag1 + 
                                       delta_libdem_rev_lag1 +
                                       e_migdpgrolns + 
                                       population +
                                       e_peaveduc + 
                                       e_civil_war +
                                       e_miurbani +
                                       e_pt_coup_n +
                                       as.factor(year) + 
                                       as.factor(country_id),
                                     data = vdem3,
                                     family = binomial(link = "logit")))

stargazer(full_mod_region_logit, full_mod_region_logit2,
          type = "text", 
          omit = c("country_id", "year"))

summary(full_mod_neighbor_logit <- glm(autocrat_dummy_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                                         delta_libdem_rev_lag1 +
                                         e_migdpgrolns + 
                                         population +
                                         e_peaveduc + 
                                         e_civil_war +
                                         e_miurbani +
                                         #e_pt_coup_n +
                                         as.factor(year) + 
                                         as.factor(country_id),
                                       data = neighbor_mean,
                                       family = binomial(link = "logit")))
summary(full_mod_neighbor_logit2 <- glm(autocrat_dummy_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                                         delta_libdem_rev_lag1 +
                                         e_migdpgrolns + 
                                         population +
                                         e_peaveduc + 
                                         e_civil_war +
                                         e_miurbani +
                                         e_pt_coup_n +
                                         as.factor(year) + 
                                         as.factor(country_id),
                                       data = neighbor_mean,
                                       family = binomial(link = "logit")))

stargazer(full_mod_neighbor_logit, full_mod_neighbor_logit2,
          type = "text", 
          omit = c("country_id", "year"))

#### Clustered standard errors ####

## Creating robust standard errors on country
cluster_log1 <- sqrt(diag(cluster.vcov(full_mod_world_logit, cluster = vdem3$country_id)))
cluster_log12 <- sqrt(diag(cluster.vcov(full_mod_world_logit2, cluster = vdem3$country_id)))
cluster_log2 <- sqrt(diag(cluster.vcov(full_mod_region_logit, cluster = vdem3$country_id)))
cluster_log22 <- sqrt(diag(cluster.vcov(full_mod_region_logit2, cluster = vdem3$country_id)))
cluster_log3 <- sqrt(diag(cluster.vcov(full_mod_neighbor_logit, cluster = neighbor_mean$country_id)))
cluster_log32 <- sqrt(diag(cluster.vcov(full_mod_neighbor_logit2, cluster = neighbor_mean$country_id)))

## Saving the clustered standard errors as part of the regression models
full_mod_world_logit$clusterVCOV <- cluster_log1
full_mod_world_logit2$clusterVCOV <- cluster_log12
full_mod_region_logit$clusterVCOV <- cluster_log2
full_mod_region_logit2$clusterVCOV <- cluster_log22
full_mod_neighbor_logit$clusterVCOV <- cluster_log3
full_mod_neighbor_logit2$clusterVCOV <- cluster_log32

## Presenting the output
stargazer(full_mod_world_logit, full_mod_world_logit2, 
          full_mod_region_logit, full_mod_region_logit2, 
          full_mod_neighbor_logit, full_mod_neighbor_logit2,
          type = "latex", 
          out = "full_all_logit_mods.tex", 
          omit = c("country_id", "year"),
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"),
          dep.var.labels = "LDI Autocratization Dummy", 
          covariate.labels = c("Mean World Change LDI lag", 
                               "Mean Region Change LDI lag", 
                               "Mean Neighbor Change LDI lag", 
                               "Change in LDI lag",
                               "Ln GDP Growth", 
                               "Ln Population", 
                               "Education",
                               "Civil War",
                               "Urbanization",
                               "Coup d'etat"), 
          title = "Logistic Regression Table With Fixed Effects and Clustered Standard Errors", 
          font.size = "footnotesize", 
          column.sep.width = "-10pt",
          no.space = TRUE,
          digits = 3, 
          se = list(cluster_log1, cluster_log12, cluster_log2, cluster_log22,
                    cluster_log3, cluster_log32))

## Ln GDP Growth
exp(-1.349) # 0.259
(exp(-1.349)-1)*100 # -74,1% odds for autocratization

## Ln Population
exp(0.184) # 0.275
(exp(0.184)-1)*100 # 20.2% odds for autocratization
exp(-0.274) # 0.760
(exp(-0.274)-1)*100 # -24 %

## Coup d'etat
exp(1.667) # 5.29
(exp(1.667)-1)*100 # 429% odds for autocratization

exp(-3.335) # 0.0356 
# One units (skala unit) increase on mean change (LDI) among the countries in 
# gives 0.0356 times as high odds
(exp(-3.335)-1)*100 # -96% higher odds for autocratization controlled for the 
# other variables in the model 

