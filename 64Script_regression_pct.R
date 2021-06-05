###############################
####                       ####
####   1. LDI Percentage   ####
####                       ####
###############################

###########################
#### OLS Simple models ####
###########################

#### World, region and neighbor level diffusion ####

## World percentage
summary(pct1.2 <- lm(delta_libdem ~ percentage_world_aut_lag1, 
                   data = vdem4))

summary(pct <- lm(delta_libdem ~ percentage_world_autn_lag1, 
                  data = vdem4))

stargazer(pct1.2, pct, 
          type = "text")

## Regional percentage 
summary(pct2.2 <- lm(delta_libdem ~ percentage_region_aut_lag1, 
                   data = vdem4))

## Neighbor percentage 
summary(pct3.2 <- lm(delta_libdem ~ percentage_neighbor_aut_lag1, 
                   data = neighbor_percentage))

#### Regression outputs ####

## Percentage world, region, neighbor
stargazer(pct1.2, pct2.2, pct3.2, 
          type = "latex", 
          out = "pct_reg_LDI.tex", 
          dep.var.labels = "Change in LDI", 
          covariate.labels = c("Pct World Autocratization lag",
                               "Pct Region Autocratization lag", 
                               "Pct Neighbor Autocratization lag"), 
          title = "Regression Table LDI Diffusion Pct", 
          font.size = "footnotesize", 
          column.sep.width = "-10pt")

#### Scatterplot with regression line ####

## World percentage LDI
ggplot(vdem4, aes(x = percentage_world_aut_lag1, y = delta_libdem)) +
  geom_point(alpha=0.3) +
  labs(x = "Percentage experiencing autocratization", 
       y = "Change in LDI", 
       title = "The effect of percentage of countries in the world experiencing \nautocratization on change in democracy level", 
       subtitle = "LDI") +
  theme_bw() +
  geom_smooth(method = "lm", col = "red") +
  geom_vline(xintercept = mean(vdem3$percentage_world_aut_lag1, na.rm = T), linetype = "dashed") +
  geom_hline(yintercept = mean(vdem3$delta_libdem, na.rm = T), linetype = "dashed")

## Region percentage LDI
ggplot(vdem4, aes(x = percentage_region_aut_lag1, y = delta_libdem)) +
  geom_point(alpha=0.3) +
  labs(x = "Percentage experiencing autocratization", 
       y = "Change in LDI", 
       title = "The effect of percentage of countries in region experiencing autocratization on \nchange in democracy level", 
       subtitle = "LDI") +
  theme_bw() +
  geom_smooth(method = "lm", col = "red") +
  geom_vline(xintercept = mean(vdem3$percentage_region_aut_lag1, na.rm = T), linetype = "dashed") +
  geom_hline(yintercept = mean(vdem3$delta_libdem, na.rm = T), linetype = "dashed")

## Neighbor percentage LDI
ggplot(neighbor_percentage, aes(x = percentage_neighbor_aut_lag1, y = delta_libdem)) +
  geom_point(alpha=0.3) +
  labs(x = "Percentage experiencing autocratization", 
       y = "Change in LDI", 
       title = "The effect of percentage of a country's neighbors democracy level on change in \ndemocracy level", 
       subtitle = "LDI") +
  theme_bw() +
  geom_smooth(method = "lm", col = "red") +
  geom_vline(xintercept = mean(neighbor_percentage$percentage_neighbor_aut_lag1, na.rm = T), linetype = "dashed") +
  geom_hline(yintercept = mean(neighbor_percentage$delta_libdem, na.rm = T), linetype = "dashed")

################################
#### Logistic simple models ####
################################

#### World, region and neighbor level diffusion ####

## World percentage LDI
summary(pct_logit1.2 <- glm(autocrat_dummy_libdem ~ percentage_world_aut_lag1, 
                        data = vdem4, 
                        family = binomial(link = "logit")))

## Regional percentage LDI
summary(pct_logit2.2 <- glm(autocrat_dummy_libdem ~ percentage_region_aut_lag1, 
                        data = vdem4, 
                        family = binomial(link = "logit")))

## Neighbor percentage LDI
summary(pct_logit3.2 <- glm(autocrat_dummy_libdem ~ percentage_neighbor_aut_lag1, 
                        data = neighbor_percentage, 
                        family = binomial(link = "logit")))

#### Regression outputs ####

## World, region, neighbor LDI
stargazer(pct_logit1.2, pct_logit2.2, pct_logit3.2, 
          type = "text")

######################
#### Full OLS LDI ####
######################

## Regression model with control variables for percentage world
summary(pct <- lm(delta_libdem ~ percentage_world_aut_lag1 + 
                  e_migdppcln + # GDP per capita logged
                  e_mipopula + # Population
                  e_miurbpop + # Urbanization
                  e_pt_coup + # Coup d'etat
                  as.factor(country_id), # Controlling for the effect of each year
                data = vdem3))

stargazer(pct, 
          omit = "country_id", 
          omit.labels = "Entity fixed effects", 
          type = "text")

############################
####                    ####
####   2. LCI vs. EDI   ####
####                    ####
############################

############################
#### OLS, simple models ####
############################

#### World, region and neighbor level diffusion ####

## World percentage liberal and polyarchy
summary(pct_m1.2_liberal <- lm(delta_liberal ~ percentage_world_aut1_lag1, 
                           data = vdem4))
summary(pct_m1.2_polyarchy <- lm(delta_polyarchy ~ percentage_world_aut2_lag1, 
                             data = vdem4))

# Presenting the results
stargazer(pct_m1.2_liberal, pct_m1.2_polyarchy, 
          type = "text")


## Regional percentage liberal and polyarchy
summary(pct_2.2_liberal <- lm(delta_liberal ~ percentage_region_aut1_lag1, 
                           data = vdem3))
summary(pct_2.2_polyarchy <- lm(delta_polyarchy ~ percentage_region_aut2_lag1, 
                             data = vdem3))

# Resenting the results
stargazer(pct_2.2_liberal, pct_2.2_polyarchy, 
          type = "text")

## Neighbor percentage liberal and polyarchy
summary(pct_3.2_liberal <- lm(delta_liberal ~ percentage_neighbor_aut1_lag1, 
                           data = neighbor_percentage))
summary(pct_3.2_polyarchy <- lm(delta_polyarchy ~ percentage_neighbor_aut2_lag1, 
                             data = neighbor_percentage))

# Presenting the results
stargazer(pct_3.2_liberal, pct_3.2_polyarchy, 
          type = "text")

#### Presenting the output ####
stargazer(pct_1.2_liberal, pct_1.2_polyarchy, pct_2.2_liberal, pct_2.2_polyarchy, pct_3.2_liberal, pct_3.2_polyarchy, 
          type = "text")

stargazer(m1.2_liberal, m2.2_liberal, m3.2_liberal, 
          m1.2_polyarchy, m2.2_polyarchy, m3.2_polyarchy, 
          type = "text")

##################################
#### Logistic: Autocrat dummy ####
##################################

## World percentage liberal and polyarchy 
summary(logit1.2_liberal <- glm(autocrat_dummy_liberal ~ percentage_world_aut1_lag1, 
                                data = vdem3, 
                                family = binomial(link = "logit")))
summary(logit1.2_polyarchy <- glm(autocrat_dummy_polyarchy ~ percentage_world_aut2_lag1, 
                                  data = vdem3, 
                                  family = binomial(link = "logit")))

# Presenting the output
stargazer(logit1.2_liberal, logit1.2_polyarchy, 
          type = "text")

## Region percentage liberal and polyarchy
summary(logit2.2_liberal <- glm(autocrat_dummy_liberal ~ percentage_region_aut1_lag1, 
                                data = vdem3, 
                                family = binomial(link = "logit")))
summary(logit2.2_polyarchy <- glm(autocrat_dummy_polyarchy ~ percentage_region_aut2_lag1, 
                                  data = vdem3, 
                                  family = binomial(link = "logit")))

# Presenting the output
stargazer(logit2.2_liberal, logit2.2_polyarchy, 
          type = "text")

## Percentage region liberal and polyarchy
summary(logit3.2_liberal <- glm(autocrat_dummy_liberal ~ percentage_neighbor_aut1_lag1, 
                                data = neighbor_percentage, 
                                family = binomial(link = "logit")))
summary(logit3.2_polyarchy <- glm(autocrat_dummy_polyarchy ~ percentage_neighbor_aut2_lag1, 
                                  data = neighbor_percentage, 
                                  family = binomial(link = "logit")))

# Presenting the output
stargazer(logit3.2_liberal, logit3.2_polyarchy, 
          type = "text")

#### Presenting the results ####

stargazer(logit1.2_liberal, logit1.2_polyarchy, 
          logit2.2_liberal, logit2.2_polyarchy, 
          logit3.2_liberal, logit3.2_polyarchy, 
          type = "text")

stargazer(logit1.2_liberal, logit2.2_liberal, logit3.2_liberal, 
          logit1.2_polyarchy, logit2.2_polyarchy, logit3.2_polyarchy, 
          type = "text")
