###############################
####                       ####
####   Robustness checks   ####
####                       ####
###############################

#### Autocratization Dummy LDI ####

## -0.05 LDI
vdem5 <- vdem3 %>%
  mutate(autocrat_dummy2_libdem_rev = ifelse(delta_libdem < -0.05, 1, 0)) # Cutoff point at -0.05

table2_libdem <- table(vdem5$autocrat_dummy2_libdem_rev, useNA = "ifany")
table2_libdem
prop.table(table2_libdem)*100

vdem6 <- neighbor_mean %>%
  mutate(autocrat_dummy2_libdem_rev = ifelse(delta_libdem < -0.05, 1, 0)) # Cutoff point at -0.05

## -0.05 LCI
vdem5 <- vdem5 %>%
  mutate(autocrat_dummy2_liberal_rev = ifelse(delta_liberal < -0.05, 1, 0)) # Cutoff point at -0.05

table2_liberal <- table(vdem5$autocrat_dummy2_liberal_rev, useNA = "ifany")
table2_liberal
prop.table(table2_liberal)*100

vdem6 <- vdem6 %>%
  mutate(autocrat_dummy2_liberal_rev = ifelse(delta_liberal < -0.05, 1, 0)) # Cutoff point at -0.05

## -0.05 EDI 
vdem5 <- vdem5 %>%
  mutate(autocrat_dummy2_polyarchy_rev = ifelse(delta_polyarchy < -0.05, 1, 0)) # Cutoff point at -0.05

table2_polyarchy <- table(vdem5$autocrat_dummy2_polyarchy_rev, useNA = "ifany")
table2_polyarchy
prop.table(table2_polyarchy)*100

vdem6 <- vdem6 %>%
  mutate(autocrat_dummy2_polyarchy_rev = ifelse(delta_polyarchy < -0.05, 1, 0)) # Cutoff point at -0.05

#### Regression models ####

## Simple LDI 
summary(logit_dummy2_world_libdem <- glm(autocrat_dummy2_libdem_rev ~ mean_world_change_rev_lag1, 
                                         data = vdem5, 
                                         family = binomial(link = "logit")))
summary(logit_dummy2_region_libdem <- glm(autocrat_dummy2_libdem_rev ~ mean_region_change_rev_lag1, 
                                          data = vdem5, 
                                          family = binomial(link = "logit")))
summary(logit_dummy2_neighbor_libdem <- glm(autocrat_dummy2_libdem_rev ~ mean_neighbor_change_rev_lag1, 
                                            data = vdem6, 
                                            family = binomial(link = "logit")))
stargazer(logit_dummy2_world_libdem, logit_dummy2_region_libdem, logit_dummy2_neighbor_libdem,
          type = "latex", 
          out = "logit2_cutoff_simple.tex",
          dep.var.labels = "LDI Autocratization Dummy 2", 
          covariate.labels = c("Mean World Change LDI lag", 
                               "Mean Region Change LDI lag", 
                               "Mean Neighbor Change LDI lag"), 
          digits = 3, 
          no.space = TRUE,
          title = "Logistic Regression Table LDI -0.05 Cut-off Point", 
          font.size = "footnotesize")

## With control variables
summary(logit_dummy2_world_libdem_f1 <- glm(autocrat_dummy2_libdem_rev ~ mean_world_change_rev_lag1 + 
                                              delta_libdem_rev_lag1 +
                                              e_migdpgrolns + 
                                              population +
                                              e_peaveduc + 
                                              e_civil_war +
                                              e_miurbani +
                                              #e_pt_coup_n +
                                              as.factor(country_id),
                                            data = vdem5, 
                                            family = binomial(link = "logit")))
summary(logit_dummy2_world_libdem_f2 <- glm(autocrat_dummy2_libdem_rev ~ mean_world_change_rev_lag1 +
                                              delta_libdem_rev_lag1 +
                                              e_migdpgrolns + 
                                              population +
                                              e_peaveduc + 
                                              e_civil_war +
                                              e_miurbani +
                                              e_pt_coup_n +
                                              as.factor(country_id),
                                            data = vdem5, 
                                            family = binomial(link = "logit")))

summary(logit_dummy2_region_libdem_f1 <- glm(autocrat_dummy2_libdem_rev ~ mean_region_change_rev_lag1 + 
                                               delta_libdem_rev_lag1 +
                                               e_migdpgrolns + 
                                               population +
                                               e_peaveduc + 
                                               e_civil_war +
                                               e_miurbani +
                                               #e_pt_coup_n +
                                               as.factor(year) + 
                                               as.factor(country_id), 
                                             data = vdem5, 
                                             family = binomial(link = "logit")))
summary(logit_dummy2_region_libdem_f2 <- glm(autocrat_dummy2_libdem_rev ~ mean_region_change_rev_lag1 + 
                                               delta_libdem_rev_lag1 +
                                               e_migdpgrolns + 
                                               population +
                                               e_peaveduc + 
                                               e_civil_war +
                                               e_miurbani +
                                               e_pt_coup_n +
                                               as.factor(year) + 
                                               as.factor(country_id), 
                                             data = vdem5, 
                                             family = binomial(link = "logit")))

summary(logit_dummy2_neighbor_libdem_f1 <- glm(autocrat_dummy2_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                                                 delta_libdem_rev_lag1 +
                                                 e_migdpgrolns + 
                                                 population +
                                                 e_peaveduc + 
                                                 e_civil_war +
                                                 e_miurbani +
                                                 #e_pt_coup_n +
                                                 as.factor(year) + 
                                                 as.factor(country_id), 
                                               data = vdem6, 
                                               family = binomial(link = "logit")))
summary(logit_dummy2_neighbor_libdem_f2 <- glm(autocrat_dummy2_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                                                 delta_libdem_rev_lag1 +
                                                 e_migdpgrolns + 
                                                 population +
                                                 e_peaveduc + 
                                                 e_civil_war +
                                                 e_miurbani +
                                                 e_pt_coup_n +
                                                 as.factor(year) + 
                                                 as.factor(country_id), 
                                               data = vdem6, 
                                               family = binomial(link = "logit")))

## Creating robust standard errors on country
cluster_logit_dummy2_w1_libdem <- sqrt(diag(cluster.vcov(logit_dummy2_world_libdem_f1, vdem5$country_id)))
cluster_logit_dummy2_w2_libdem <- sqrt(diag(cluster.vcov(logit_dummy2_world_libdem_f2, vdem5$country_id)))
cluster_logit_dummy2_r1_libdem <- sqrt(diag(cluster.vcov(logit_dummy2_region_libdem_f1, vdem5$country_id)))
cluster_logit_dummy2_r2_libdem <- sqrt(diag(cluster.vcov(logit_dummy2_region_libdem_f2, vdem5$country_id)))
cluster_logit_dummy2_n1_libdem <- sqrt(diag(cluster.vcov(logit_dummy2_neighbor_libdem_f1, vdem6$country_id)))
cluster_logit_dummy2_n2_libdem <- sqrt(diag(cluster.vcov(logit_dummy2_neighbor_libdem_f2, vdem6$country_id)))

## Saving the clustered standard errors as part of the regression models
logit_dummy2_world_libdem_f1$clusterVCOV <- cluster_logit_dummy2_w1_libdem
logit_dummy2_world_libdem_f2$clusterVCOV <- cluster_logit_dummy2_w2_libdem
logit_dummy2_region_libdem_f1$clusterVCOV <- cluster_logit_dummy2_r1_libdem
logit_dummy2_region_libdem_f2$clusterVCOV <- cluster_logit_dummy2_r2_libdem
logit_dummy2_neighbor_libdem_f1$clusterVCOV <- cluster_logit_dummy2_n1_libdem
logit_dummy2_neighbor_libdem_f2$clusterVCOV <- cluster_logit_dummy2_n2_libdem

stargazer(logit_dummy2_world_libdem_f1, logit_dummy2_world_libdem_f2, 
          logit_dummy2_region_libdem_f1, logit_dummy2_region_libdem_f2, 
          logit_dummy2_neighbor_libdem_f1, logit_dummy2_neighbor_libdem_f2, 
          type = "text", omit = c("country_id", "year"))

## Presenting the output
stargazer(logit_dummy2_world_libdem_f1, logit_dummy2_world_libdem_f2, 
          logit_dummy2_region_libdem_f1, logit_dummy2_region_libdem_f2, 
          logit_dummy2_neighbor_libdem_f1, logit_dummy2_neighbor_libdem_f2, 
          type = "latex", 
          out = "logit2_cutoff_full.tex",
          title = "Logistic Regression Table LDI With Controls -0.05 Cut-off Point", 
          omit = c("country_id", "year"), 
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"),
          dep.var.labels = "LDI Autocratization Dummy 2",
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
          font.size = "footnotesize", 
          column.sep.width = "-10pt", 
          no.space = TRUE, 
          se = list(cluster_logit_dummy2_w1_libdem, cluster_logit_dummy2_w2_libdem, 
                    cluster_logit_dummy2_r1_libdem, cluster_logit_dummy2_r2_libdem, 
                    cluster_logit_dummy2_n1_libdem, cluster_logit_dummy2_n2_libdem))


## LCI
summary(logit_dummy2_world_liberal <- glm(autocrat_dummy2_liberal_rev ~ mean_world_change1_rev_lag1 +
                                            delta_liberal_rev_lag1 + 
                                            e_migdpgrolns +
                                            population + 
                                            e_peaveduc +
                                            e_civil_war +
                                            e_miurbani +
                                            e_pt_coup_n + 
                                            as.factor(country_id),
                                          data = vdem5, 
                                          family = binomial(link = "logit")))
summary(logit_dummy2_region_liberal <- glm(autocrat_dummy2_liberal_rev ~ mean_region_change1_rev_lag1 + 
                                             delta_liberal_rev_lag1 + 
                                             e_migdpgrolns +
                                             population + 
                                             e_peaveduc +
                                             e_civil_war +
                                             e_miurbani +
                                             e_pt_coup_n + 
                                             as.factor(year) + 
                                             as.factor(country_id), 
                                           data = vdem5,
                                           family = binomial(link = "logit")))
summary(logit_dummy2_neighbor_liberal <- glm(autocrat_dummy2_liberal_rev ~ mean_neighbor_change1_rev_lag1 + 
                                               delta_liberal_rev_lag1 + 
                                               e_migdpgrolns +
                                               population + 
                                               e_peaveduc +
                                               e_civil_war +
                                               e_miurbani +
                                               e_pt_coup_n + 
                                               as.factor(year) + 
                                               as.factor(country_id), 
                                             data = vdem6, 
                                             family = binomial(link = "logit")))
stargazer(logit_dummy2_world_liberal, logit_dummy2_region_liberal, logit_dummy2_neighbor_liberal, 
          type = "text", omit = c("country_id", "year"))

## EDI
summary(logit_dummy2_world_polyarchy <- glm(autocrat_dummy2_polyarchy_rev ~ mean_world_change2_rev_lag1 + 
                                              delta_polyarchy_rev_lag1 + 
                                              e_migdpgrolns +
                                              population + 
                                              e_peaveduc +
                                              e_civil_war +
                                              e_miurbani +
                                              e_pt_coup_n + 
                                              as.factor(country_id),
                                            data = vdem5, 
                                            family = binomial(link = "logit")))
summary(logit_dummy2_region_polyarchy <- glm(autocrat_dummy2_polyarchy_rev ~ mean_region_change2_rev_lag1 + 
                                               delta_polyarchy_rev_lag1 + 
                                               e_migdpgrolns +
                                               population + 
                                               e_peaveduc +
                                               e_civil_war +
                                               e_miurbani +
                                               e_pt_coup_n + 
                                               as.factor(year) + 
                                               as.factor(country_id), 
                                             data = vdem5, 
                                             family = binomial(link = "logit")))
summary(logit_dummy2_neighbor_polyarchy <- glm(autocrat_dummy2_polyarchy_rev ~ mean_neighbor_change2_rev_lag1 +
                                                 delta_polyarchy_rev_lag1 + 
                                                 e_migdpgrolns +
                                                 population + 
                                                 e_peaveduc +
                                                 e_civil_war +
                                                 e_miurbani +
                                                 e_pt_coup_n + 
                                                 as.factor(year) + 
                                                 as.factor(country_id), 
                                               data = vdem6, 
                                               family = binomial(link = "logit")))

stargazer(logit_dummy2_world_polyarchy, logit_dummy2_region_polyarchy, logit_dummy2_neighbor_polyarchy, 
          type = "text", omit = c("country_id", "year"))

## Creating robust standard errors on country and year 
cluster_logit_dummy2_w_liberal <- sqrt(diag(cluster.vcov(logit_dummy2_world_liberal, vdem5$country_id)))
cluster_logit_dummy2_r_liberal <- sqrt(diag(cluster.vcov(logit_dummy2_region_liberal, vdem5$country_id)))
cluster_logit_dummy2_n_liberal <- sqrt(diag(cluster.vcov(logit_dummy2_neighbor_liberal, vdem6$country_id)))

cluster_logit_dummy2_w_polyarchy <- sqrt(diag(cluster.vcov(logit_dummy2_world_polyarchy, vdem5$country_id)))
cluster_logit_dummy2_r_polyarchy <- sqrt(diag(cluster.vcov(logit_dummy2_region_polyarchy, vdem5$country_id)))
cluster_logit_dummy2_n_polyarchy <- sqrt(diag(cluster.vcov(logit_dummy2_neighbor_polyarchy, vdem6$country_id)))

## Saving the clustered standard errors as part of the regression models
logit_dummy2_world_liberal$clusterVCOV <- cluster_logit_dummy2_w_liberal
logit_dummy2_region_liberal$clusterVCOV <- cluster_logit_dummy2_r_liberal
logit_dummy2_neighbor_liberal$clusterVCOV <- cluster_logit_dummy2_n_liberal

logit_dummy2_world_polyarchy$clusterVCOV <- cluster_logit_dummy2_w_polyarchy
logit_dummy2_region_polyarchy$clusterVCOV <- cluster_logit_dummy2_r_polyarchy
logit_dummy2_neighbor_polyarchy$clusterVCOV <- cluster_logit_dummy2_n_polyarchy

stargazer(logit_dummy2_world_liberal, logit_dummy2_region_liberal, logit_dummy2_neighbor_liberal,
          logit_dummy2_world_polyarchy, logit_dummy2_region_polyarchy, logit_dummy2_neighbor_polyarchy,
          type = "text", omit = c("country_id", "year")) 

## Presenting the output
stargazer(logit_dummy2_world_liberal, logit_dummy2_region_liberal, logit_dummy2_neighbor_liberal,
          logit_dummy2_world_polyarchy, logit_dummy2_region_polyarchy, logit_dummy2_neighbor_polyarchy,
          type = "latex", 
          out = "logit2_lib_poly_full.tex", 
          title = "Logistic Regression Table LCI and EDI With Controls -0.05 Cut-off Point", 
          dep.var.labels = c("LCI Autocratization Dummy 2", "EDI Autocratization Dummy 2"), 
          covariate.labels = c("Mean World Change LCI lag", 
                               "Mean Region Change LCI lag", 
                               "Mean Neighbor Change LCI lag", 
                               "Change in LCI lag",
                               "Mean World Change EDI lag", 
                               "Mean Region Change EDI lag", 
                               "Mean Neighbor Change EDI lag", 
                               "Change in EDI lag", 
                               "Ln GDP Growth", 
                               "Ln Population", 
                               "Education", 
                               "Civil War", 
                               "Urbanization", 
                               "Coup d'etat"),
          font.size = "footnotesize", 
          column.sep.width = "-10pt", 
          no.space = TRUE, 
          omit = c("country_id", "year"),
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"),
          se = list(cluster_logit_dummy2_w_liberal, cluster_logit_dummy2_r_liberal, cluster_logit_dummy2_n_liberal, 
                    cluster_logit_dummy2_w_polyarchy, cluster_logit_dummy2_r_polyarchy, cluster_logit_dummy2_n_polyarchy))




# Robustness checks

## Dummy variable UV

## Lagging UVs with three years
vdem3$mean_world_change_lag3 <- ave(vdem3$mean_world_change, 
                                    vdem3$country_id, 
                                    FUN = function(x){lag(x, 3)})

## Lagging e_regionpol (10 categories) with 3 years
vdem3$mean_region_pol_lag3 <- ave(vdem3$mean_region_pol,
                                  vdem3$country_id, 
                                  FUN = function(x){lag(x, 3)})

## Three different variables region, two others as robustness checks

# First examining the region variables 
table(vdem3$e_regiongeo)
summary(vdem3$e_regiongeo)
table(vdem3$e_regionpol_6C)
summary(vdem3$e_regionpol_6C)

# Creating a variable for mean regional democracy level for e_regiongeo (19 categories) 
vdem3 <- vdem3 %>%
  group_by(year, e_regiongeo) %>%
  mutate(mean_region_geo = (sum(delta_libdem) - delta_libdem) / (n()-1))
sum(is.na(vdem3$mean_region_geo))

## Creating a variable for mean regional democracy level for e_regionpol_6C (6 categories)
vdem3 <- vdem3 %>%
  group_by(year, e_regionpol_6C) %>%
  mutate(mean_region_6C = (sum(delta_libdem) - delta_libdem) / (n()-1))
sum(is.na(vdem3$mean_region_6C))

# Lagging e_regiongeo (19 categories) with 1 and 3 years
vdem3$mean_region_geo_lag1 <- ave(vdem3$mean_region_geo,
                                  vdem3$country_id, 
                                  FUN = function(x){lag(x, 1)})

vdem3$mean_region_geo_lag3 <- ave(vdem3$mean_region_geo,
                                  vdem3$country_id, 
                                  FUN = function(x){lag(x, 3)})


# Lagging e_regionpol_6C (6 categories) with 1 and 3 years
vdem3$mean_region_6Clag1 <- ave(vdem3$mean_region_6C,
                                vdem3$country_id, 
                                FUN = function(x){lag(x, 1)})

vdem3$mean_region_6C_lag3 <- ave(vdem3$mean_region_6C,
                                 vdem3$country_id, 
                                 FUN = function(x){lag(x, 3)})


####### AV ########

## Bivariate logistic regression with dummy variable (negative change below 0 is coded as 1)
summary(logit_dummy <- glm(autocrat_dummy_libdem ~ mean_world_change, 
                           data = vdem3, 
                           #method = , #random for RE model, and within for FE model
                           family = binomial(link = "logit"))) #probit

## Bivariate logistic regression with dummy variable 2 (negative change below -0.05 is coded as 1)
summary(logit_dummy2 <- glm(autocrat_dummy2_libdem ~ mean_world_change, 
                            data = vdem3, 
                            #method = , #random for RE model, and within for FE model
                            family = binomial(link = "logit"))) #probit

## Presenting the output
stargazer(logit_dummy, logit_dummy2,
          type = "text")

## 3 year lag for dummy variable (negative change below 0 is coded as 1)
summary(logit_dummy_lag3 <- glm(autocrat_dummy_libdem ~ mean_world_change_lag3, 
                                data = vdem3, 
                                family = binomial(link = "logit")))

stargazer(logit_dummy, logit_dummy_lag1, logit_dummy_lag3, 
          type = "text")

## 3 year lag for dummy variable 2 (negative change below -0.05 is coded as 1)
summary(logit_dummy2_lag3 <- glm(autocrat_dummy2_libdem ~ mean_world_change_lag3, 
                                 data = vdem3, 
                                 family = binomial(link = "logit")))


