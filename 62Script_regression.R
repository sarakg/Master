############################
####                    ####
####   2. LCI vs. EDI   ####
####                    ####
############################

############################
#### OLS, simple models ####
############################

#### World level diffusion ####

## World mean liberal and polyarchy
summary(m1.1_liberal <- lm(delta_liberal_rev ~ mean_world_change1_rev_lag1, 
                           data = vdem3))
summary(m1.1_polyarchy <- lm(delta_polyarchy_rev ~ mean_world_change2_rev_lag1, 
                             data = vdem3))

# Presenting the results
stargazer(m1.1_liberal, m1.1_polyarchy, 
          type = "text")

#### Regional level diffusion ####

## Regional mean liberal and polyarchy
summary(m2.1_liberal <- lm(delta_liberal_rev ~ mean_region_change1_rev_lag1, 
                           data = vdem3))
summary(m2.1_polyarchy <- lm(delta_polyarchy_rev ~ mean_region_change2_rev_lag1, 
                             data = vdem3))

# Presenting the results
stargazer(m2.1_liberal, m2.1_polyarchy, 
          type = "text")

#### Neighbor level diffusion ####

## Neighbor mean liberal and polyarchy
summary(m3.1_liberal <- lm(delta_liberal_rev ~ mean_neighbor_change1_rev_lag1, 
                           data = neighbor_mean))
summary(m3.1_polyarchy <- lm(delta_polyarchy_rev ~ mean_neighbor_change2_rev_lag1, 
                             data = neighbor_mean))

# Presenting the results
stargazer(m3.1_liberal, m3.1_polyarchy, 
          type = "text")

#### Presenting the results ####
stargazer(m1.1_liberal, m1.1_polyarchy, m2.1_liberal, m2.1_polyarchy, m3.1_liberal, m3.1_polyarchy, 
          type = "latex",
          out = "liberal_polyarchy_simple_mods.tex",
          dep.var.labels = c("Change in LCI", "Change in EDI", "Change in LCI", "Change in EDI", 
                             "Change in LCI", "Change in EDI"), 
          covariate.labels = c("Mean world change LCI lag", "Mean world change EDI lag", 
                               "Mean region change LCI lag", "Mean region change EDI lag", 
                               "Mean neighbor change LCI lag", "Mean neighbor change EDI lag"), 
          title = "Regression Table LCI vs EDI", 
          font.size = "footnotesize", 
          column.sep.width = "-8pt", 
          no.space = TRUE, 
          float.env = "sidewaystable", 
          omit.stat = "f")

################################################
#### Full OLS: Change in level of democracy ####
################################################

## First check with different control variables before FE

## World mean liberal and polyarchy full
summary(m1.1_liberal_full <- lm(delta_liberal_rev ~ mean_world_change1_rev_lag1 + 
                                  delta_liberal_rev_lag1 + 
                                  e_migdpgrolns +
                                  population + 
                                  e_peaveduc +
                                  e_civil_war +
                                  e_miurbani +
                                  e_pt_coup_n +  
                                  as.factor(country_id),
                                data = vdem3))
summary(m1.1_polyarchy_full <- lm(delta_polyarchy_rev ~ mean_world_change2_rev_lag1 +
                                    delta_polyarchy_rev_lag1 + 
                                    e_migdpgrolns +
                                    population + 
                                    e_peaveduc +
                                    e_civil_war +
                                    e_miurbani +
                                    e_pt_coup_n + 
                                    as.factor(country_id),
                                  data = vdem3))

stargazer(m1.1_liberal_full, m1.1_polyarchy_full, 
          type = "text", 
          omit = "country_id")

## Region mean liberal and polyarchy full
summary(m2.1_liberal_full <- lm(delta_liberal_rev ~ mean_region_change1_rev_lag1 + 
                                  delta_liberal_rev_lag1 +  
                                  e_migdpgrolns +
                                  population + 
                                  e_peaveduc +
                                  e_civil_war +
                                  e_miurbani +
                                  e_pt_coup_n + 
                                  as.factor(year) + 
                                  as.factor(country_id),
                                data = vdem3))
summary(m2.1_polyarchy_full <- lm(delta_polyarchy_rev ~ mean_region_change2_rev_lag1 +
                                    delta_polyarchy_rev_lag1 + 
                                    e_migdpgrolns +
                                    population + 
                                    e_peaveduc + 
                                    e_civil_war +
                                    e_miurbani +
                                    e_pt_coup_n + 
                                    as.factor(year) + 
                                    as.factor(country_id),
                                  data = vdem3))

stargazer(m2.1_liberal_full, m2.1_polyarchy_full, 
          type = "text", 
          omit = c("country_id", "year"))

## Neighbor mean liberal and polyarchy full
summary(m3.1_liberal_full <- lm(delta_liberal_rev ~ mean_neighbor_change1_rev_lag1 + 
                                  delta_liberal_rev_lag1 + 
                                  e_migdpgrolns +
                                  population + 
                                  e_peaveduc +
                                  e_civil_war +
                                  e_miurbani +
                                  e_pt_coup_n + 
                                  as.factor(year) + 
                                  as.factor(country_id),
                                data = neighbor_mean))
summary(m3.1_polyarchy_full <- lm(delta_polyarchy_rev ~ mean_neighbor_change2_rev_lag1 +
                                    delta_polyarchy_rev_lag1 + 
                                    e_migdpgrolns +
                                    population + 
                                    e_peaveduc +
                                    e_civil_war +
                                    e_miurbani +
                                    e_pt_coup_n +
                                    as.factor(year) + 
                                    as.factor(country_id),
                                  data = neighbor_mean))

stargazer(m3.1_liberal_full, m3.1_polyarchy_full, 
          type = "text", 
          omit = c("country_id", "year"))

stargazer(m1.1_liberal_full, m2.1_liberal_full, m3.1_liberal_full,
          m1.1_polyarchy_full, m2.1_polyarchy_full, m3.1_polyarchy_full,
          type = "text",
          omit = c("country_id", "year"))

#### Clustered standard errors ####

## Creating robust standard errors on country and year 
cluster_liberal_m1 <- sqrt(diag(cluster.vcov(m1.1_liberal_full, vdem3$country_id)))
cluster_liberal_m2 <- sqrt(diag(cluster.vcov(m2.1_liberal_full, vdem3$country_id)))
cluster_liberal_m3 <- sqrt(diag(cluster.vcov(m3.1_liberal_full, neighbor_mean$country_id)))

cluster_polyarchy_m1 <- sqrt(diag(cluster.vcov(m1.1_polyarchy_full, vdem3$country_id)))
cluster_polyarchy_m2 <- sqrt(diag(cluster.vcov(m2.1_polyarchy_full, vdem3$country_id)))
cluster_polyarchy_m3 <- sqrt(diag(cluster.vcov(m3.1_polyarchy_full, neighbor_mean$country_id)))

## Saving the clustered standard errors as part of the regression models
m1.1_liberal_full$clusterVCOV <- cluster_liberal_m1
m2.1_liberal_full$clusterVCOV <- cluster_liberal_m2
m3.1_liberal_full$clusterVCOV <- cluster_liberal_m3

m1.1_polyarchy_full$clusterVCOV <- cluster_polyarchy_m1
m2.1_polyarchy_full$clusterVCOV <- cluster_polyarchy_m2
m3.1_polyarchy_full$clusterVCOV <- cluster_polyarchy_m3

#### Presenting the output ####

stargazer(m1.1_liberal_full, m2.1_liberal_full, m3.1_liberal_full,
          m1.1_polyarchy_full, m2.1_polyarchy_full, m3.1_polyarchy_full,
          type = "latex",
          out = "liberal_polyarchy_full_mods.tex",
          omit = c("country_id", "year"), 
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"),
          dep.var.labels = c("Change in LCI", "Change in EDI"), 
          covariate.labels = c("Mean world change LCI lag", "Mean region change LCI lag", 
                               "Mean neighbor change LCI lag", "Change in LCI lag", "Mean world change EDI lag", 
                               "Mean region change EDI lag", "Mean neighbor change EDI lag", 
                               "Change in EDI lag", "Ln GDP Growth", "Ln Population", "Education", 
                               "Civil War", "Urbanization", "Coup d'etat"), 
          title = "Full Regression Table LCI vs EDI with Robust Standard Errors", 
          font.size = "footnotesize", 
          column.sep.width = "-10pt", 
          no.space = TRUE, 
          omit.stat = c("f", "ser"), 
          se = list(cluster_liberal_m1, cluster_liberal_m2, cluster_liberal_m3, 
                    cluster_polyarchy_m1, cluster_polyarchy_m2, cluster_polyarchy_m3))

####################################
#### Predicted Probability Plot ####
####################################

# Making a sequence of the mean_world_change1_lag1 variable
xrange <- seq(min(vdem3$mean_world_change1_rev_lag1, na.rm = TRUE), 
              max(vdem3$mean_world_change1_rev_lag1, na.rm = TRUE), 
              length.out = 18787)

# Names on coefficients in the lm-model
names(coefficients(m1.1_liberal_full))

m1.1_liberal_full_pred <- lm(delta_liberal_rev ~ mean_world_change1_rev_lag1 +
                               delta_liberal_rev_lag1 + 
                               e_migdpgrolns + 
                               population + 
                               e_peaveduc + 
                               e_civil_war +
                               e_miurbani + 
                               e_pt_coup_n, 
                             data = vdem3)

## Creating a new dataframe 
pred_LCI <- data.frame(1, # the intercept 
                       mean_world_change1_rev_lag1 = xrange, 
                       delta_liberal_rev_lag1 = mean(vdem3$delta_libdem_rev_lag1, na.rm = T),
                       e_migdpgrolns = mean(vdem3$e_migdpgrolns, na.rm = T),
                       population = mean(vdem3$population, na.rm = T),
                       e_peaveduc = mean(vdem3$e_peaveduc, na.rm = T),
                       e_civil_war = 1, 
                       e_miurbani = mean(vdem3$e_miurbani, na.rm = T),
                       e_pt_coup_n = 1)

predict <- predict(object = m1.1_liberal_full_pred, newdata = pred_LCI, interval = "confidence")
pred_LCI <- cbind(pred_LCI, predict)

## Plotting the predicted probability plot
ggplot(pred_LCI, aes(mean_world_change1_rev_lag1, fit, ymin=lwr, ymax=upr)) + 
  geom_line(color = "lightcoral")+
  geom_ribbon(alpha=0.1) +
  geom_vline(xintercept = mean(vdem3$mean_world_change1_rev_lag1, na.rm = T), linetype = "dashed") +
  geom_hline(yintercept = mean(vdem3$delta_liberal_rev, na.rm = T), linetype = "dashed") +
  theme_bw() + 
  labs(x = "Mean World Change LCI", 
       y = "Predicted Change in LCI", 
       title = "Predicted Probability Plot")

ggsave("predprob_liberal_full.png", 
       plot = last_plot(),  
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")





# Variable from 0-1 if variable spesifisert like that. Covariates at mean value
# create a data.frame . ggplot to plot
pred <- effect("mean_world_change1_rev_lag1", m1.1_liberal, 
               confidence.level=.975)
pred <- as.data.frame(pred)
ggplot(pred, aes(mean_world_change1_rev_lag1, fit)) +
  geom_smooth(method = "lm") +
  geom_ribbon(data=pred,aes(ymin=lower,ymax=upper),alpha=0.1)


pred2 <- Effect(c("mean_world_change1_rev_lag1", 
                  "delta_liberal_rev_lag1", 
                  "e_migdpgrolns",
                  "population",
                  "e_peaveduc", 
                  "e_civil_war", 
                  "e_miurbani", 
                  "e_pt_coup_n"), 
                m1.1_liberal_full_pred,  
                xlevels=list(mean_world_change1_rev_lag1 = seq(-0.0487, 0.0169),
                             delta_liberal_rev_lag1 = mean(vdem3$delta_liberal_rev_lag1, na.rm=T),
                             e_migdpgrolns = mean(vdem3$e_migdpgrolns, na.rm = T), 
                             population = mean(vdem3$population, na.rm = T),
                             e_peaveduc = mean(vdem3$e_peaveduc, na.rm=T), 
                             e_civil_war = 1, 
                             e_miurbani = mean(vdem3$e_miurbani, na.rm=T), 
                             e_pt_coup_n = 1), 
                confidence.level=.975, typical=mean)
pred2 <- as.data.frame(pred2)

ggplot(pred2, aes(mean_world_change1_lag1, fit, 
                  ymin = lower, ymax = upper)) +
  geom_smooth(method = "lm") +
  geom_ribbon(alpha=0.1) 



pred1 <- effect("mean_world_change1_rev_lag1", m1.1_liberal_full_pred, #vcov. = cluster_liberal_m1, #mod2_vcov
                xlevels=list(delta_liberal_rev_lag1 = seq(-1,1,0.1),
                             e_migdpgrolns = mean(vdem3$e_migdpgrolns, na.rm = T), 
                             population = mean(vdem3$population, na.rm = T), 
                             e_peaveduc = mean(vdem3$e_peaveduc, na.rm = T), 
                             e_civil_war = 1, 
                             e_miurbani = mean(vdem3$e_miurbani, na.rm = T),
                             e_pt_coup_n = 1), 
                #se=list(RobustSE), 
                confidence.level=.975, typical=mean)
pred1 <- as.data.frame(pred1)
ggplot(pred1, aes(mean_world_change1_rev_lag1, fit)) + 
  geom_line() +
  geom_ribbon(data=pred1, aes(ymin=lower, ymax=upper), alpha=0.1)
#   

  
  
library(jtools)

effect_plot(m1.1_liberal, pred = mean_world_change1_rev_lag1, 
            interval = TRUE, int.type = "confidence", int.width = .95, 
            colors = "coral", 
            x.label = "Mean World Change", 
            y.label = "LCI", 
            main.title = "Predicted Probability Plot")
effect_plot(m1.1_polyarchy, pred = mean_world_change2_lag1, 
            interval = TRUE, int.type = "confidence", int.width = .95, 
            colors = "coral", 
            x.label = "Mean World Change", 
            y.label = "EDI", 
            main.title = "Predicted Probability Plot")


effect_plot(m2.1_liberal, pred = mean_region_change1_lag1, 
            interval = TRUE, int.type = "confidence", int.width = .95, 
            colors = "lightcoral", 
            x.label = "Mean Region Change", 
            y.label = "LCI", 
            main.title = "Predicted Probability Plot")
effect_plot(m2.1_polyarchy, pred = mean_region_change2_lag1, 
            interval = TRUE, int.type = "confidence", int.width = .95, 
            colors = "lightcoral", 
            x.label = "Mean Region Change", 
            y.label = "EDI", 
            main.title = "Predicted Probability Plot")

effect_plot(m3.1_liberal, pred = mean_neighbor_change1_lag1, 
            interval = TRUE, int.type = "confidence", int.width = .95, 
            colors = "lightcoral", 
            x.label = "Mean Neighbor Change", 
            y.label = "LCI", 
            main.title = "Predicted Probability Plot")
effect_plot(m3.1_polyarchy, pred = mean_neighbor_change2_lag1, 
            interval = TRUE, int.type = "confidence", int.width = .95, 
            colors = "lightcoral", 
            x.label = "Mean Neighbor Change", 
            y.label = "EDI", 
            main.title = "Predicted Probability Plot")

##################################
#### Logistic: Autocrat dummy ####
##################################

#### World level diffusion ####

## World mean change in liberal and polyarchy
summary(logit1.1_liberal <- glm(autocrat_dummy_liberal_rev ~ mean_world_change1_rev_lag1, 
                                data = vdem3, 
                                family = binomial(link = "logit")))
summary(logit1.1_polyarchy <- glm(autocrat_dummy_polyarchy_rev ~ mean_world_change2_rev_lag1, 
                                  data = vdem3, 
                                  family = binomial(link = "logit")))
stargazer(logit1.1_liberal, logit1.1_polyarchy, 
          type = "text")

#### Regional level diffusion ####

## Region mean liberal and polyarchy
summary(logit2.1_liberal <- glm(autocrat_dummy_liberal_rev ~ mean_region_change1_rev_lag1, 
                                data = vdem3, 
                                family = binomial(link = "logit")))
summary(logit2.1_polyarchy <- glm(autocrat_dummy_polyarchy_rev ~ mean_region_change2_rev_lag1, 
                                  data = vdem3, 
                                  family = binomial(link = "logit")))

# Presenting the output
stargazer(logit2.1_liberal, logit2.1_polyarchy, 
          type = "text")

#### Neighbor diffusion ####

## Neighbor mean liberal and polyarchy 
summary(logit3.1_liberal <- glm(autocrat_dummy_liberal_rev ~ mean_neighbor_change1_rev_lag1, 
                                data = neighbor_mean, 
                                family = binomial(link = "logit")))
summary(logit3.1_polyarchy <- glm(autocrat_dummy_polyarchy_rev ~ mean_neighbor_change2_rev_lag1, 
                                  data = neighbor_mean, 
                                  family = binomial(link = "logit")))

# Presenting the output
stargazer(logit3.1_liberal, logit3.1_polyarchy, 
          type = "text")

#### Presenting the results ####
stargazer(logit1.1_liberal, logit2.1_liberal, logit3.1_liberal,
          logit1.1_polyarchy, logit2.1_polyarchy, logit3.1_polyarchy,  
          type = "text")

stargazer(logit1.1_liberal, logit2.1_liberal, logit3.1_liberal,
          logit1.1_polyarchy, logit2.1_polyarchy, logit3.1_polyarchy, 
          type = "latex",
          out = "logit_simple_liberal_polyarchy.tex", 
          title = "Logistic Regression Table LCI and EDI",
          dep.var.labels = c("LCI Autocratization Dummy", "EDI Autocratization Dummy"),
          covariate.labels = c("Mean World Change LCI lag", 
                               "Mean Region Change LCI lag", 
                               "Mean Neighbor Change LCI lag", 
                               "Mean World Change EDI lag", 
                               "Mean Region Change EDI lag", 
                               "Mean Neighbor Change EDI lag"), 
          font.size = "footnotesize", 
          column.sep.width = "-10pt", 
          no.space = TRUE)

#######################################
#### Full logistic: Autocrat dummy ####
#######################################

#### World level diffusion ####

## World mean change in liberal and polyarchy
summary(logit1.1_liberal_full <- glm(autocrat_dummy_liberal_rev ~ mean_world_change1_rev_lag1 +
                                       delta_liberal_rev_lag1 + 
                                       e_migdpgrolns +
                                       population + 
                                       e_peaveduc +
                                       e_civil_war +
                                       e_miurbani +
                                       e_pt_coup_n + 
                                       as.factor(country_id),
                                     data = vdem3, 
                                     family = binomial(link = "logit")))
summary(logit1.1_polyarchy_full <- glm(autocrat_dummy_polyarchy_rev ~ mean_world_change2_rev_lag1 +
                                         delta_polyarchy_rev_lag1 + 
                                         e_migdpgrolns +
                                         population + 
                                         e_peaveduc +
                                         e_civil_war +
                                         e_miurbani +
                                         e_pt_coup_n + 
                                         as.factor(country_id),
                                       data = vdem3, 
                                       family = binomial(link = "logit"))) 

stargazer(logit1.1_liberal_full, logit1.1_polyarchy_full, 
          type = "text", 
          omit = "country_id")

#### Regional level diffusion ####

## Region mean liberal and polyarchy
summary(logit2.1_liberal_full <- glm(autocrat_dummy_liberal_rev ~ mean_region_change1_rev_lag1 +
                                       delta_liberal_rev_lag1 + 
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
summary(logit2.1_polyarchy_full <- glm(autocrat_dummy_polyarchy_rev ~ mean_region_change2_rev_lag1 + 
                                         delta_polyarchy_rev_lag1 + 
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

stargazer(logit2.1_liberal_full, logit2.1_polyarchy_full, 
          type = "text", 
          omit = c("country_id", "year"))

#### Neighbor diffusion ####

## Neighbor mean liberal and polyarchy 
summary(logit3.1_liberal_full <- glm(autocrat_dummy_liberal_rev ~ mean_neighbor_change1_rev_lag1 +
                                       delta_liberal_rev_lag1 + 
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
summary(logit3.1_polyarchy_full <- glm(autocrat_dummy_polyarchy_rev ~ mean_neighbor_change2_rev_lag1 + 
                                         delta_polyarchy_rev_lag1 + 
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

stargazer(logit3.1_liberal_full, logit3.1_polyarchy_full, 
          type = "text", 
          omit = c("country_id", "year"))

#### Clustered standard errors ####

## Creating robust standard errors on country and year 
cluster_logit1.1_liberal_full <- sqrt(diag(cluster.vcov(logit1.1_liberal_full, vdem3$country_id)))
cluster_logit2.1_liberal_full <- sqrt(diag(cluster.vcov(logit2.1_liberal_full, vdem3$country_id)))
cluster_logit3.1_liberal_full <- sqrt(diag(cluster.vcov(logit3.1_liberal_full, neighbor_mean$country_id)))

cluster_logit1.1_polyarchy_full <- sqrt(diag(cluster.vcov(logit1.1_polyarchy_full, vdem3$country_id)))
cluster_logit2.1_polyarchy_full <- sqrt(diag(cluster.vcov(logit2.1_polyarchy_full, vdem3$country_id)))
cluster_logit3.1_polyarchy_full <- sqrt(diag(cluster.vcov(logit3.1_polyarchy_full, neighbor_mean$country_id)))

## Saving the clustered standard errors as part of the regression models
logit1.1_liberal_full$clusterVCOV <- cluster_logit1.1_liberal_full
logit2.1_liberal_full$clusterVCOV <- cluster_logit2.1_liberal_full
logit3.1_liberal_full$clusterVCOV <- cluster_logit3.1_liberal_full

logit1.1_polyarchy_full$clusterVCOV <- cluster_logit1.1_polyarchy_full
logit2.1_polyarchy_full$clusterVCOV <- cluster_logit2.1_polyarchy_full
logit3.1_polyarchy_full$clusterVCOV <- cluster_logit3.1_polyarchy_full

#### Presenting the output ####
stargazer(logit1.1_liberal_full, logit2.1_liberal_full, logit3.1_liberal_full,
          logit1.1_polyarchy_full, logit2.1_polyarchy_full, logit3.1_polyarchy_full,  
          type = "text",
          omit = c("year", "country_id"))

stargazer(logit1.1_liberal_full, logit2.1_liberal_full, logit3.1_liberal_full,
          logit1.1_polyarchy_full, logit2.1_polyarchy_full, logit3.1_polyarchy_full,  
          type = "latex",
          out = "logit_liberal_polyarchy_full.tex", 
          omit = c("country_id", "year"), 
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"), 
          dep.var.labels = c("Change in LCI", "Change in EDI"), 
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
          title = "Logistic Regression Table LCI and EDI With Robust Standard Errors", 
          font.size = "footnotesize", 
          no.space = TRUE, 
          column.sep.width = "-10pt", 
          se = list(cluster_logit1.1_liberal_full, cluster_logit2.1_liberal_full,
                    cluster_logit3.1_liberal_full, cluster_logit1.1_polyarchy_full, 
                    cluster_logit2.1_polyarchy_full, cluster_logit3.1_polyarchy_full))

###################################
####                           ####
####   Clean Elections Index   ####
####                           ####
###################################

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
          dep.var.labels = "Change in Clean Elections Index", 
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
          dep.var.labels = "Change in Clean Elections Index", 
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
