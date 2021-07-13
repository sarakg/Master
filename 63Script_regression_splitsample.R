#################################
#### Split sample regression ####
#################################

#### Splitting the sample into two time periods ####

## 1900-1959 world and region diffusion
split1 <- vdem3 %>%
  filter(year < 1960)
range(split1$year) 

## 1960-2019
split2 <- vdem3 %>%
  filter(year > 1959)
range(split2$year) 

## 1900-1959 neighbor diffusion
split3 <- neighbor_mean %>%
  filter(year < 1960)
range(split3$year)

## 1960-2019
split4 <- neighbor_mean %>%
  filter(year > 1959)
range(split4$year)

#### Regression three diffusion levels ####

## Mean world
summary(split1_m1 <- lm(delta_libdem_rev ~ mean_world_change_rev_lag1 +  
                          delta_libdem_rev_lag1 + # Lagged DV
                          e_migdpgrolns + # Ln GDP Growth
                          population + # Ln population
                          e_peaveduc + # Education
                          e_civil_war + # Civil War
                          e_miurbani + # Urbanization
                          #e_pt_coup_n, # Coup d'etat
                          as.factor(country_id),
                        data = split1))
summary(split2_m1 <- lm(delta_libdem_rev ~ mean_world_change_rev_lag1 + 
                          delta_libdem_rev_lag1 + # Lagged DV
                          e_migdpgrolns + # Ln GDP Growth
                          population + # Ln population
                          e_peaveduc + # Education
                          e_civil_war + # Civil War
                          e_miurbani + # Urbanization
                          #e_pt_coup_n, # Coup d'etat
                          as.factor(country_id),
                        data = split2))

stargazer(split1_m1, split2_m1, type = "text", omit = "country_id",
          column.labels = c("1900-1959", "1960-2019"))

## Mean region
summary(split1_m2 <- lm(delta_libdem_rev ~ mean_region_change_rev_lag1 +
                          delta_libdem_rev_lag1 +
                          e_migdpgrolns + 
                          population + 
                          e_peaveduc + 
                          e_civil_war + 
                          e_miurbani + 
                          #e_pt_coup_n, 
                          as.factor(year) +
                          as.factor(country_id),
                        data = split1))
summary(split2_m2 <- lm(delta_libdem_rev ~ mean_region_change_rev_lag1 +
                          delta_libdem_rev_lag1 + 
                          e_migdpgrolns + 
                          population + 
                          e_peaveduc + 
                          e_civil_war + 
                          e_miurbani + 
                          #e_pt_coup_n, 
                          as.factor(year) +
                          as.factor(country_id),
                        data = split2))

stargazer(split1_m2, split2_m2, type = "text", omit = c("year", "country_id"),
          column.labels = c("1900-1959", "1960-2019"))

## Mean neighbor
summary(split3_m3 <- lm(delta_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                          delta_libdem_rev_lag1 + 
                          e_migdpgrolns + 
                          population + 
                          e_peaveduc + 
                          e_civil_war + 
                          e_miurbani + 
                          #e_pt_coup_n, 
                          as.factor(year) +
                          as.factor(country_id),
                        data = split3))
summary(split4_m3 <- lm(delta_libdem_rev ~ mean_neighbor_change_rev_lag1 + 
                          delta_libdem_rev_lag1 + 
                          e_migdpgrolns + 
                          population + 
                          e_peaveduc + 
                          e_civil_war + 
                          e_miurbani + 
                          #e_pt_coup_n, 
                          as.factor(year) +
                          as.factor(country_id),
                        data = split4))

stargazer(split3_m3, split4_m3, type = "text", omit = c("year", "country_id"),
          column.labels = c("1900-1959", "1960-2019"))

#### Clustering the standard errors ####

## Creating robust standard errors on country
cluster_split1_m1 <- sqrt(diag(cluster.vcov(split1_m1, cluster = split1$country_id)))
cluster_split2_m1 <- sqrt(diag(cluster.vcov(split2_m1, cluster = split2$country_id)))
cluster_split1_m2 <- sqrt(diag(cluster.vcov(split1_m2, cluster = split1$country_id)))
cluster_split2_m2 <- sqrt(diag(cluster.vcov(split2_m2, cluster = split2$country_id)))
cluster_split3_m3 <- sqrt(diag(cluster.vcov(split3_m3, cluster = split3$country_id)))
cluster_split4_m3 <- sqrt(diag(cluster.vcov(split4_m3, cluster = split4$country_id)))

## Saving the clustered standard errors as part of the regression models
split1_m1$clusterVCOV <- cluster_split1_m1
split2_m1$clusterVCOV <- cluster_split2_m1
split1_m2$clusterVCOV <- cluster_split1_m2
split2_m2$clusterVCOV <- cluster_split2_m2
split3_m3$clusterVCOV <- cluster_split3_m3
split4_m3$clusterVCOV <- cluster_split4_m3

## Presenting the output
stargazer(split1_m1, split2_m1, split1_m2, split2_m2, split3_m3, split4_m3, 
          type = "latex", 
          out = "split_sample_full.tex",
          omit = c("country_id", "year"), 
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"),
          column.labels = c("1900-1959", "1960-2019", "1900-1959", "1960-2019",
                            "1900-1959", "1960-2019"), 
          dep.var.labels = "Autocratization in LDI", 
          covariate.labels = c("Mean World Change LDI lag", 
                               "Mean Region Change LDI lag", 
                               "Mean Neighbor Change LDI lag", 
                               "Change in LDI lag", 
                               "Ln GDP Growth", 
                               "Ln Population", 
                               "Education", 
                               "Civil War", 
                               "Urbanization"), 
          title = "Split Sample Regression With Control Variables and Robust Standard Errors:
          1900-1959 and 1960-2019", 
          se = list(cluster_split1_m1, cluster_split2_m1, cluster_split1_m2, 
                    cluster_split2_m2, cluster_split3_m3, cluster_split4_m3),
          font.size = "footnotesize", 
          no.space = TRUE, 
          float.env = "sidewaystable", 
          omit.stat = c("f"))

#### Logistic Regression ####

## Mean World 
summary(split1_logit_m1 <- glm(autocrat_dummy_libdem_rev ~ mean_world_change_rev_lag1 + 
                                 delta_libdem_rev_lag1 + 
                                 e_migdpgrolns + 
                                 population + 
                                 e_peaveduc + 
                                 e_civil_war + 
                                 e_miurbani + 
                                 as.factor(country_id),
                               data = split1))
summary(split2_logit_m1 <- glm(autocrat_dummy_libdem_rev ~ mean_world_change_rev_lag1 + 
                                 delta_libdem_rev_lag1 + 
                                 e_migdpgrolns + 
                                 population + 
                                 e_peaveduc + 
                                 e_civil_war + 
                                 e_miurbani + 
                                 as.factor(country_id),
                               data = split2))
stargazer(split1_logit_m1, split2_logit_m1, 
          type = "text", omit = "country_id",
          column.labels = c("1900-1959", "1960-2019"))

sum(is.na(split1$e_migdpgrolns)) # 5129
sum(is.na(split1$e_miurbani)) # 1855
sum(is.na(split1$population)) # 420
sum(is.na(split1$e_peaveduc)) # 3429
sum(is.na(split1$e_civil_war)) # 5009

####################
#### LCI vs EDI ####
####################

#### LCI ####

## World
summary(split1_liberal_m1 <- lm(delta_liberal_rev ~ mean_world_change1_rev_lag1 + 
                                  delta_liberal_rev_lag1 + 
                                  e_migdpgrolns + 
                                  population + 
                                  e_peaveduc + 
                                  e_civil_war + 
                                  e_miurbani + 
                                  as.factor(country_id),
                                data = split1))
summary(split2_liberal_m1 <- lm(delta_liberal_rev ~ mean_world_change1_rev_lag1 + 
                                  delta_liberal_rev_lag1 + 
                                  e_migdpgrolns + 
                                  population + 
                                  e_peaveduc + 
                                  e_civil_war + 
                                  e_miurbani +  
                                  as.factor(country_id),
                                data = split2))
stargazer(split1_liberal_m1, split2_liberal_m1, type = "text", omit = "country_id")

## Region
summary(split1_liberal_m2_r <- lm(delta_liberal_rev ~ mean_region_change1_rev_lag1 + 
                                    delta_liberal_rev_lag1 + 
                                    e_migdpgrolns + 
                                    population + 
                                    e_peaveduc + 
                                    e_civil_war + 
                                    e_miurbani + 
                                    as.factor(year) + 
                                    as.factor(country_id),
                                  data = split1))
summary(split2_liberal_m2_r <- lm(delta_liberal_rev ~ mean_region_change1_rev_lag1 + 
                                    delta_liberal_rev_lag1 + 
                                    e_migdpgrolns + 
                                    population + 
                                    e_peaveduc + 
                                    e_civil_war + 
                                    e_miurbani + 
                                    as.factor(year) + 
                                    as.factor(country_id),
                                  data = split2))
stargazer(split1_liberal_m2_r, split2_liberal_m2_r, type = "text", omit = c("year", "country_id"))

## Neighbor
summary(split3_liberal_m3_n <- lm(delta_liberal_rev ~ mean_neighbor_change1_rev_lag1 + 
                                    delta_liberal_rev_lag1 + 
                                    e_migdpgrolns + 
                                    population + 
                                    e_peaveduc + 
                                    e_civil_war + 
                                    e_miurbani + 
                                    as.factor(year) +
                                    as.factor(country_id),
                                  data = split3))
summary(split4_liberal_m3_n <- lm(delta_liberal_rev ~ mean_neighbor_change1_rev_lag1 + 
                                    delta_liberal_rev_lag1 + 
                                    e_migdpgrolns + 
                                    population + 
                                    e_peaveduc + 
                                    e_civil_war + 
                                    e_miurbani + 
                                    as.factor(year) +
                                    as.factor(country_id),
                                  data = split4))
stargazer(split3_liberal_m3_n, split4_liberal_m3_n, type = "text", omit = c("year", "country_id"))

## Creating robust standard errors on country
cluster_split1_liberal_w <- sqrt(diag(cluster.vcov(split1_liberal_m1, cluster = split1$country_id)))
cluster_split2_liberal_w <- sqrt(diag(cluster.vcov(split2_liberal_m1, cluster = split2$country_id)))
cluster_split1_liberal_r <- sqrt(diag(cluster.vcov(split1_liberal_m2_r, cluster = split1$country_id)))
cluster_split2_liberal_r <- sqrt(diag(cluster.vcov(split2_liberal_m2_r, cluster = split2$country_id)))
cluster_split3_liberal_n <- sqrt(diag(cluster.vcov(split3_liberal_m3_n, cluster = split3$country_id)))
cluster_split4_liberal_n <- sqrt(diag(cluster.vcov(split4_liberal_m3_n, cluster = split4$country_id)))

## Saving the clustered standard errors as part of the regression models
split1_liberal_m1$clusterVCOV <- cluster_split1_liberal_w
split2_liberal_m1$clusterVCOV <- cluster_split2_liberal_w
split1_liberal_m2_r$clusterVCOV <- cluster_split1_liberal_r
split2_liberal_m2_r$clusterVCOV <- cluster_split2_liberal_r
split3_liberal_m3_n$clusterVCOV <- cluster_split3_liberal_n
split4_liberal_m3_n$clusterVCOV <- cluster_split4_liberal_n

## Presenting the output
stargazer(split1_liberal_m1, split2_liberal_m1, split1_liberal_m2_r,
          split2_liberal_m2_r, split3_liberal_m3_n, split4_liberal_m3_n, 
          type = "latex", 
          out = "liberal_split_sample_full.tex",
          omit = c("country_id", "year"), 
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"), 
          column.labels = c("1900-1959", "1960-2019", "1900-1959", "1960-2019",
                            "1900-1959", "1960-2019"),
          dep.var.labels = "Autocratization in LCI", 
          covariate.labels = c("Mean World Change LCI lag", 
                               "Mean Region Change LCI lag", 
                               "Mean Neighbor Change LCI lag", 
                               "Change in LCI lag", 
                               "Ln GDP Growth", 
                               "Ln Population", 
                               "Education", 
                               "Civil War", 
                               "Urbanization"),
          title = "Split Sample Regression LCI With Control Variables and Robust Standard Errors:
          1900-1959 and 1960-2019", 
          se = list(cluster_split1_liberal_w, cluster_split2_liberal_w, 
                    cluster_split1_liberal_r, cluster_split2_liberal_r, 
                    cluster_split3_liberal_n, cluster_split4_liberal_n),
          font.size = "footnotesize", 
          no.space = TRUE, 
          float.env = "sidewaystable", 
          omit.stat = c("f"))

#### EDI ####

## World
summary(split1_polyarchy_m1 <- lm(delta_polyarchy_rev ~ mean_world_change2_rev_lag1 + 
                                  delta_polyarchy_rev_lag1 + 
                                  e_migdpgrolns + 
                                  population + 
                                  e_peaveduc + 
                                  e_civil_war + 
                                  e_miurbani + 
                                  as.factor(country_id),
                                data = split1))
summary(split2_polyarchy_m1 <- lm(delta_polyarchy_rev ~ mean_world_change2_rev_lag1 + 
                                  delta_polyarchy_rev_lag1 + 
                                  e_migdpgrolns + 
                                  population + 
                                  e_peaveduc + 
                                  e_civil_war + 
                                  e_miurbani +  
                                  as.factor(country_id),
                                data = split2))
stargazer(split1_polyarchy_m1, split2_polyarchy_m1, type = "text", omit = "country_id")

## Region
summary(split1_polyarchy_m2_r <- lm(delta_polyarchy_rev ~ mean_region_change2_rev_lag1 + 
                                    delta_polyarchy_rev_lag1 + 
                                    e_migdpgrolns + 
                                    population + 
                                    e_peaveduc + 
                                    e_civil_war + 
                                    e_miurbani + 
                                    as.factor(year) + 
                                    as.factor(country_id),
                                  data = split1))
summary(split2_polyarchy_m2_r <- lm(delta_polyarchy_rev ~ mean_region_change2_rev_lag1 + 
                                    delta_polyarchy_rev_lag1 + 
                                    e_migdpgrolns + 
                                    population + 
                                    e_peaveduc + 
                                    e_civil_war + 
                                    e_miurbani + 
                                    as.factor(year) + 
                                    as.factor(country_id),
                                  data = split2))
stargazer(split1_polyarchy_m2_r, split2_polyarchy_m2_r, type = "text", omit = c("year", "country_id"))

## Neighbor
summary(split3_polyarchy_m3_n <- lm(delta_polyarchy_rev ~ mean_neighbor_change2_rev_lag1 + 
                                    delta_polyarchy_rev_lag1 + 
                                    e_migdpgrolns + 
                                    population + 
                                    e_peaveduc + 
                                    e_civil_war + 
                                    e_miurbani + 
                                    as.factor(year) +
                                    as.factor(country_id),
                                  data = split3))
summary(split4_polyarchy_m3_n <- lm(delta_polyarchy_rev ~ mean_neighbor_change2_rev_lag1 + 
                                    delta_polyarchy_rev_lag1 + 
                                    e_migdpgrolns + 
                                    population + 
                                    e_peaveduc + 
                                    e_civil_war + 
                                    e_miurbani + 
                                    as.factor(year) +
                                    as.factor(country_id),
                                  data = split4))
stargazer(split3_polyarchy_m3_n, split4_polyarchy_m3_n, type = "text", omit = c("year", "country_id"))

## Creating robust standard errors on country
cluster_split1_polyarchy_w <- sqrt(diag(cluster.vcov(split1_polyarchy_m1, cluster = split1$country_id)))
cluster_split2_polyarchy_w <- sqrt(diag(cluster.vcov(split2_polyarchy_m1, cluster = split2$country_id)))
cluster_split1_polyarchy_r <- sqrt(diag(cluster.vcov(split1_polyarchy_m2_r, cluster = split1$country_id)))
cluster_split2_polyarchy_r <- sqrt(diag(cluster.vcov(split2_polyarchy_m2_r, cluster = split2$country_id)))
cluster_split3_polyarchy_n <- sqrt(diag(cluster.vcov(split3_polyarchy_m3_n, cluster = split3$country_id)))
cluster_split4_polyarchy_n <- sqrt(diag(cluster.vcov(split4_polyarchy_m3_n, cluster = split4$country_id)))

## Saving the clustered standard errors as part of the regression models
split1_polyarchy_m1$clusterVCOV <- cluster_split1_polyarchy_w
split2_polyarchy_m1$clusterVCOV <- cluster_split2_polyarchy_w
split1_polyarchy_m2_r$clusterVCOV <- cluster_split1_polyarchy_r
split2_polyarchy_m2_r$clusterVCOV <- cluster_split2_polyarchy_r
split3_polyarchy_m3_n$clusterVCOV <- cluster_split3_polyarchy_n
split4_polyarchy_m3_n$clusterVCOV <- cluster_split4_polyarchy_n

## Presenting the output
stargazer(split1_polyarchy_m1, split2_polyarchy_m1, split1_polyarchy_m2_r,
          split2_polyarchy_m2_r, split3_polyarchy_m3_n, split4_polyarchy_m3_n, 
          type = "latex", 
          out = "polyarchy_split_sample_full.tex",
          omit = c("country_id", "year"), 
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"), 
          column.labels = c("1900-1959", "1960-2019", "1900-1959", "1960-2019",
                            "1900-1959", "1960-2019"),
          dep.var.labels = "Autocratization in EDI", 
          covariate.labels = c("Mean World Change EDI lag", 
                               "Mean Region Change EDI lag", 
                               "Mean Neighbor Change EDI lag", 
                               "Change in EDI lag", 
                               "Ln GDP Growth", 
                               "Ln Population", 
                               "Education", 
                               "Civil War", 
                               "Urbanization"),
          title = "Split Sample Regression EDI With Control Variables and Robust Standard Errors:
          1900-1959 and 1960-2019", 
          se = list(cluster_split1_polyarchy_w, cluster_split2_polyarchy_w, 
                    cluster_split1_polyarchy_r, cluster_split2_polyarchy_r, 
                    cluster_split3_polyarchy_n, cluster_split4_polyarchy_n),
          font.size = "footnotesize", 
          no.space = TRUE, 
          float.env = "sidewaystable", 
          omit.stat = c("f"))

#### Clean Elections Index ####

## Reversing v2xel_frefair
vdem3$v2xel_frefair_rev <- vdem3$v2xel_frefair*-1+max(vdem3$v2xel_frefair, na.rm = TRUE)

summary(vdem3$v2xel_frefair_rev)
summary(vdem3$v2xel_frefair)
ggplot(vdem3, aes(v2xel_frefair)) + geom_histogram()
ggplot(vdem3, aes(v2xel_frefair_rev)) + geom_histogram()

## ## Variable measuring change in democracy level for each country from year to year
vdem3 <- vdem3 %>%
  group_by(country_id) %>%
  mutate(delta_frefair = v2xel_frefair - dplyr::lag(v2xel_frefair),
         delta_frefair_rev = v2xel_frefair_rev - dplyr::lag(v2xel_frefair_rev))

# Lagging the dependent variable
vdem3$delta_frefair_lag1 <- ave(vdem3$delta_frefair, 
                                vdem3$country_id, 
                                FUN = function(x){dplyr::lag(x, 1)})
vdem3$delta_frefair_rev_lag1 <- ave(vdem3$delta_frefair_rev, 
                                    vdem3$country_id, 
                                    FUN = function(x){dplyr::lag(x, 1)}) 

## Mean world change
vdem3 <- vdem3 %>%   
  group_by(year) %>% 
  mutate(mean_world_changeff = (sum(delta_frefair, na.rm = TRUE) - delta_frefair) / (n() -1),
         mean_world_changeff_rev = (sum(delta_frefair_rev, na.rm = TRUE) -  delta_frefair_rev) / (n() -1))

# Lagging independent variables
vdem3$mean_world_changeff_lag1 <- ave(vdem3$mean_world_changeff, 
                                      vdem3$country_id, 
                                      FUN = function(x){dplyr::lag(x, 1)})
vdem3$mean_world_changeff_rev_lag1 <- ave(vdem3$mean_world_changeff_rev, 
                                          vdem3$country_id, 
                                          FUN = function(x){dplyr::lag(x ,1)})

## Mean region change
vdem3 <- vdem3 %>%
  group_by(e_regionpol, year) %>%
  mutate(mean_region_changeff = (sum(delta_frefair, na.rm = TRUE) - delta_frefair) / (n()-1) ,
         mean_region_changeff_rev = (sum(delta_frefair_rev, na.rm = TRUE) - delta_frefair_rev) / (n()-1))

# Lagging independent variables
vdem3$mean_region_changeff_lag1 <- ave(vdem3$mean_region_changeff, 
                                      vdem3$country_id, 
                                      FUN = function(x){dplyr::lag(x, 1)})
vdem3$mean_region_changeff_rev_lag1 <- ave(vdem3$mean_region_changeff_rev, 
                                          vdem3$country_id, 
                                          FUN = function(x){dplyr::lag(x ,1)})

## Mean neighbor change
neighbor_ff <- left_join(x = cow, 
                         y = vdem3, 
                         by = c("state2no"="COWcode", "year"))

neighbor_ff_mean <- neighbor_ff %>%
  group_by(state1no, year) %>%
  summarise(mean_neighbor_changeff = sum(delta_frefair, na.rm = TRUE) / n(), 
            mean_neighbor_changeff_rev = sum(delta_frefair_rev, na.rm = TRUE) / n())

# Lagging the variables 
neighbor_ff_mean$mean_neighbor_changeff_lag1 <- ave(neighbor_ff_mean$mean_neighbor_changeff, 
                                                    neighbor_ff_mean$state1no, 
                                                    FUN = function(x){dplyr::lag(x, 1)})
neighbor_ff_mean$mean_neighbor_changeff_rev_lag1 <- ave(neighbor_ff_mean$mean_neighbor_changeff_rev, 
                                                        neighbor_ff$state1no, 
                                                        FUN = function(x){dplyr::lag(x ,1)})

neighbor_ff_merged <- left_join(x = neighbor_ff_mean, 
                                y = vdem3, 
                                by = c("state1no"="COWcode", "year"))

## Regression 
summary(mod4 <- lm(delta_frefair_rev ~ mean_world_changeff_rev_lag1, 
                   data = vdem3))

## World
summary(mod4_full <- lm(delta_frefair_rev ~ mean_world_changeff_rev_lag1 + 
                          delta_frefair_rev_lag1 +
                          e_migdpgrolns + 
                          population +
                          e_peaveduc + 
                          e_civil_war +
                          e_miurbani + 
                          #e_pt_coup_n +
                          as.factor(country_id), 
                        data = vdem3))
summary(mod4_full2 <- lm(delta_frefair_rev ~ mean_world_changeff_rev_lag1 + 
                            delta_frefair_rev_lag1 +
                            e_migdpgrolns + 
                            population +
                            e_peaveduc + 
                            e_civil_war +
                            e_miurbani + 
                            e_pt_coup_n +
                            as.factor(country_id), 
                          data = vdem3))

## Region
summary(mod4_full_r <- lm(delta_frefair_rev ~ mean_region_changeff_rev_lag1 + 
                            delta_frefair_rev_lag1 + 
                            e_migdpgrolns + 
                            population +
                            e_peaveduc + 
                            e_civil_war +
                            e_miurbani + 
                            #e_pt_coup_n +
                            as.factor(year) +
                            as.factor(country_id), 
                          vdem3))
summary(mod4_full2_r <- lm(delta_frefair_rev ~ mean_region_changeff_rev_lag1 + 
                            delta_frefair_rev_lag1 + 
                            e_migdpgrolns + 
                            population +
                            e_peaveduc + 
                            e_civil_war +
                            e_miurbani + 
                            e_pt_coup_n +
                            as.factor(year) +
                            as.factor(country_id), 
                          vdem3))

## Neighbor
summary(mod4_full_n <- lm(delta_frefair_rev ~ mean_neighbor_changeff_rev_lag1 + 
                            delta_frefair_rev_lag1 + 
                            e_migdpgrolns + 
                            population +
                            e_peaveduc + 
                            e_civil_war +
                            e_miurbani + 
                            #e_pt_coup_n +
                            as.factor(year) +
                            as.factor(country_id), 
                          neighbor_ff_merged))
summary(mod4_full2_n <- lm(delta_frefair_rev ~ mean_neighbor_changeff_rev_lag1 + 
                            delta_frefair_rev_lag1 + 
                            e_migdpgrolns + 
                            population +
                            e_peaveduc + 
                            e_civil_war +
                            e_miurbani + 
                            e_pt_coup_n +
                            as.factor(year) +
                            as.factor(country_id), 
                          neighbor_ff_merged))

stargazer(mod4_full, mod4_full2, mod4_full_r, mod4_full2_r, mod4_full_n, mod4_full2_n, 
          type = "text", omit = c("country_id", "year"))

## Presenting the output
stargazer(mod4_full, mod4_full2, mod4_full_r, mod4_full2_r, mod4_full_n, mod4_full2_n, 
          type = "text", 
          out = "clean_elections_regression.tex",
          omit = c("country_id", "year"), 
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"), 
          dep.var.labels = "Autocratization in Clean Elections Index", 
          covariate.labels = c("Mean World Change lag", 
                               "Mean Region Change lag", 
                               "Mean Neighbor Change lag", 
                               "Change in Clean Elections lag",
                               "Ln GDP Growth", 
                               "Ln Population",
                               "Education",
                               "Civil War",
                               "Urbanization",
                               "Coup d'etat"), 
          title = "Clean Elections Index: Control Variables and Fixed Effects", 
          font.size = "footnotesize", 
          column.sep.width = "-5pt",
          no.space = TRUE, 
          float.env = "sidewaystable",
          omit.stat = "f")

## Creating robust standard errors on country
cluster_mod4 <- sqrt(diag(cluster.vcov(mod4_full, cluster = vdem3$country_id)))
cluster_mod4_2 <- sqrt(diag(cluster.vcov(mod4_full2, cluster = vdem3$country_id)))
cluster_mod4_r <- sqrt(diag(cluster.vcov(mod4_full_r, cluster = vdem3$country_id)))
cluster_mod4_r2 <- sqrt(diag(cluster.vcov(mod4_full2_r, cluster = vdem3$country_id)))
cluster_mod4_n <- sqrt(diag(cluster.vcov(mod4_full_n, cluster = neighbor_ff_merged$country_id)))
cluster_mod4_n2 <- sqrt(diag(cluster.vcov(mod4_full2_n, cluster = neighbor_ff_merged$country_id)))

## Saving the clustered standard errors as part of the regression models
mod4_full$clusterVCOV <- cluster_mod4
mod4_full2$clusterVCOV <- cluster_mod4_2
mod4_full_r$clusterVCOV <- cluster_mod4_r
mod4_full2_r$clusterVCOV <- cluster_mod4_r2
mod4_full_n$clusterVCOV <- cluster_mod4_n
mod4_full2_n$clusterVCOV <- cluster_mod4_n2

## Presenting the output with robust standard errors
stargazer(mod4_full, mod4_full2, mod4_full_r, mod4_full2_r, mod4_full_n, mod4_full2_n, 
          type = "text", 
          out = "clean_elections_robust_SE.tex",
          omit = c("country_id", "year"), 
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"), 
          dep.var.labels = "Autocratization in Clean Elections Index", 
          covariate.labels = c("Mean World Change lag", 
                               "Mean Region Change lag", 
                               "Mean Neighbor Change lag", 
                               "Change in Clean Elections lag",
                               "Ln GDP Growth", 
                               "Ln Population",
                               "Education",
                               "Civil War",
                               "Urbanization",
                               "Coup d'etat"), 
          title = "Clean Elections Index: Control Variables, Fixed Effects and Clustered Standard Errors", 
          font.size = "footnotesize", 
          column.sep.width = "-5pt",
          no.space = TRUE, 
          omit.stat = "f", 
          float.env = "sidewaystable",
          se = list(cluster_mod4, cluster_mod4_2, cluster_mod4_r, cluster_mod4_r2, 
          cluster_mod4_n, cluster_mod4_n2))

   
