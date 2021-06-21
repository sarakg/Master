###############################
####                       ####
####   Robustness checks   ####
####                       ####
###############################

#############################
####                     ####
#### Shares creating IVs ####
####                     ####
#############################

#########################
#### World Diffusion ####
#########################

#### Creating a dependent variable for share of the world experiencing negative 
#### change in level of democracy above 0.01 (when reversed)

## Variable for share of world, without each individual observation
vdem4 <- vdem3 %>%
  group_by(year) %>%
  mutate(percentage_world_aut = (sum(delta_libdem < -0.01, na.rm = TRUE) - delta_libdem) / (n()-1), 
         percentage_world_aut1 = (sum(delta_liberal < -0.01, na.rm = TRUE) - delta_liberal) / (n()-1),
         percentage_world_aut2 = (sum(delta_polyarchy < -0.01, na.rm = TRUE) - delta_polyarchy) / (n()-1))

## Actual percentage because *100 and with each individual observation
vdem4 <- vdem4 %>%
  group_by(year) %>%
  mutate(percentage_world_autn = 100*mean(delta_libdem < -0.01, na.rm = TRUE), 
         percentage_world_aut1n = 100*mean(delta_liberal < -0.01, na.rm = TRUE),  
         percentage_world_aut2n = 100*mean(delta_polyarchy < -0.01, na.rm = TRUE))

View(vdem4)

#### Lagging the variables ####

## Lagging each with one year 
vdem4$percentage_world_aut_lag1 <- ave(vdem4$percentage_world_aut, 
                                       vdem4$country_id, 
                                       FUN = function(x){dplyr::lag(x, 1)})
vdem4$percentage_world_aut1_lag1 <- ave(vdem4$percentage_world_aut1, 
                                        vdem4$country_id, 
                                        FUN = function(x){dplyr::lag(x, 1)})
vdem4$percentage_world_aut2_lag1 <- ave(vdem4$percentage_world_aut2, 
                                        vdem4$country_id, 
                                        FUN = function(x){dplyr::lag(x, 1)})

vdem4$percentage_world_autn_lag1 <- ave(vdem4$percentage_world_autn, 
                                        vdem4$country_id, 
                                        FUN = function(x){dplyr::lag(x, 1)})
vdem4$percentage_world_aut1n_lag1 <- ave(vdem4$percentage_world_aut1n, 
                                         vdem4$country_id, 
                                         FUN = function(x){dplyr::lag(x, 1)})
vdem4$percentage_world_aut2n_lag1 <- ave(vdem4$percentage_world_aut2n, 
                                         vdem4$country_id, 
                                         FUN = function(x){dplyr::lag(x, 1)})

#### Graphical illustrations ####

## Scatterplot, liberal democracy index
ggplot(vdem4, aes(year, percentage_world_aut_lag1)) +
  geom_point() + 
  geom_smooth(color = "lightcoral") + 
  theme_classic() +
  labs(x = "Year", 
       y = "Share", 
       title = "Share of the world experiencing autocratization during a year", 
       subtitle = "Liberal Democracy Index")

## Scatterplot, liberal component index
ggplot(vdem4, aes(year, percentage_world_aut1_lag1)) +
  geom_point() + 
  geom_smooth(color = "gold1") + 
  theme_classic() +
  labs(x = "Year", 
       y = "Share", 
       title = "Share of the countries in the region experiencing autocratization during a year", 
       subtitle = "Liberal Component Index")

## Scatterplot, electoral democracy index
ggplot(vdem4, aes(year, percentage_world_aut2_lag1)) +
  geom_point() + 
  geom_smooth(color = "steelblue") + 
  theme_classic() +
  labs(x = "Year", 
       y = "Share", 
       title = "Share of the neighbor countries experiencing autocratization during a year", 
       subtitle = "Electoral Democracy Index")

#### New data frame ####

## Creating a new data frame for percentage experiencing negative change each year
autocrat_share <- vdem3 %>%
  group_by(year) %>%
  summarise(share_autocrat = mean(delta_libdem < -0.01, na.rm = TRUE))

View(autocrat_percentage)

## Scatterplot 
ggplot(autocrat_share, aes(year, share_autocrat)) + 
  geom_point() +
  geom_smooth() + 
  theme_classic() +
  labs(x = "Year", 
       y = "Share", 
       title = "Share of world experiencing autocratization below -0.01 (LDI)")

############################
#### Regional Diffusion ####
############################

#### Share of countries in the region experiencing autocratization ####

## Adding a variable for the share of countries in the region experiencing negative change in level of democracy 
## below -0.01
vdem4 <- vdem4 %>%
  group_by(e_regionpol, year) %>%
  mutate(percentage_region_aut = mean(delta_libdem < -0.01, na.rm = TRUE), 
         percentage_region_aut1 = mean(delta_liberal < -0.01, na.rm = TRUE),
         percentage_region_aut2 = mean(delta_polyarchy < -0.01, na.rm = TRUE))

View(vdem4)

#### Lagging the variables ####

## Lagging the region variable with 1 year
vdem4$percentage_region_aut_lag1 <- ave(vdem4$percentage_region_aut,
                                        vdem4$country_id, 
                                        FUN = function(x){dplyr::lag(x, 1)})
vdem4$percentage_region_aut1_lag1 <- ave(vdem4$percentage_region_aut1,
                                         vdem4$country_id, 
                                         FUN = function(x){dplyr::lag(x, 1)})
vdem4$percentage_region_aut2_lag1 <- ave(vdem4$percentage_region_aut2,
                                         vdem4$country_id, 
                                         FUN = function(x){dplyr::lag(x, 1)})
View(vdem4)

#### Graphical illustrations ####

## Scatterplot, percentage LDI
ggplot(vdem4, aes(year, percentage_region_aut_lag1, color = e_regionpol)) +
  geom_point() + 
  geom_smooth(col = "black") + 
  theme_classic() +
  facet_wrap(vars(e_regionpol), nrow = 3, ncol = 4) +
  facet_rep_wrap(~ e_regionpol, scales = "free_y", 
                 repeat.tick.labels = 'all', labeller = as_labeller(strip.labs)) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_text(size = 10)) +
  labs(x = "Year", 
       y = "Share", 
       title = "Share of countries experiencing autocratization during a year 1900-2019", 
       subtitle = "Liberal Democracy Index") 

ggsave("scatter_pct_LDI_e_region.png", plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Scatterplot, percentage LCI
ggplot(vdem4, aes(year, percentage_region_aut1_lag1, color = e_regionpol)) +
  geom_point() + 
  geom_smooth(col = "black") + 
  theme_classic() +
  facet_wrap(vars(e_regionpol)) +
  facet_rep_wrap(~ e_regionpol, scales = "free_y", 
                 repeat.tick.labels = 'all', labeller = as_labeller(strip.labs)) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_text(size = 10)) +
  labs(x = "Year", 
       y = "Share", 
       title = "Share of countries experiencing autocratization during a year 1900-2019", 
       subtitle = "Liberal Component Index")

ggsave("scatter_pct_LCI_e_region.png", plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Scatterplot, percentage EDI
ggplot(vdem4, aes(year, percentage_region_aut2_lag1, color = e_regionpol)) +
  geom_point() + 
  geom_smooth(col = "black") + 
  theme_classic() +
  facet_wrap(vars(e_regionpol)) +
  facet_rep_wrap(~ e_regionpol, scales = "free_y", 
                 repeat.tick.labels = 'all', labeller = as_labeller(strip.labs)) +
  theme(legend.title = element_blank()) +
  theme(strip.text = element_text(size = 10)) +
  labs(x = "Year", 
       y = "Share", 
       title = "Share of countries experiencing autocratization during a year 1900-2019", 
       subtitle = "Electoral Democracy Index")

ggsave("scatter_pct_EDI_e_region.png", plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

############################
#### Neighbor diffusion ####
############################

#### Share of a country's contiguous neighbors undergoing autocratization ####

## New data frame for percentage 
cow_vdem_percentage <- cow_vdem %>%
  group_by(state1no, year) %>%
  summarise(percentage_neighbor_aut = mean(delta_libdem < -0.01, na.rm = TRUE), 
            percentage_neighbor_aut1 = mean(delta_liberal < -0.01, na.rm = TRUE), 
            percentage_neighbor_aut2 = mean(delta_polyarchy < -0.01, na.rm = TRUE))

View(cow_vdem_percentage)

#### Lagging the variables ####

## Lagging with 1 year
cow_vdem_percentage$percentage_neighbor_aut_lag1 <- ave(cow_vdem_percentage$percentage_neighbor_aut,
                                                        cow_vdem_percentage$state1no,
                                                        FUN = function(x){dplyr::lag(x, 1)})
cow_vdem_percentage$percentage_neighbor_aut1_lag1 <- ave(cow_vdem_percentage$percentage_neighbor_aut1,
                                                         cow_vdem_percentage$state1no,
                                                         FUN = function(x){dplyr::lag(x, 1)})
cow_vdem_percentage$percentage_neighbor_aut2_lag1 <- ave(cow_vdem_percentage$percentage_neighbor_aut2,
                                                         cow_vdem_percentage$state1no,
                                                         FUN = function(x){dplyr::lag(x, 1)})

View(cow_vdem_percentage)

#### Merging cow_vdem_percentage with vdem3 again ####

## Merging with vdem3 again 
neighbor_percentage <- left_join(x = cow_vdem_percentage, # includes all rows in cow_vdem_percentage
                                 y = vdem4, 
                                 by = c("state1no"="COWcode", "year")) # state2no in first dataset, COWcode in second. 
# year is the same

View(neighbor_percentage)

########################
####   LDI Shares   ####
########################

###########################
#### OLS Simple models ####
###########################

#### World, region and neighbor level diffusion ####

## World 
summary(pct1.2 <- lm(delta_libdem ~ percentage_world_aut_lag1, 
                     data = vdem4))

## Regional  
summary(pct2.2 <- lm(delta_libdem ~ percentage_region_aut_lag1, 
                     data = vdem4))

## Neighbor  
summary(pct3.2 <- lm(delta_libdem ~ percentage_neighbor_aut_lag1, 
                     data = neighbor_percentage))

#### Regression outputs ####

## Percentage world, region, neighbor
stargazer(pct1.2, pct2.2, pct3.2,
          type = "latex", 
          out = "pct_reg_LDI.tex", 
          dep.var.labels = "Change in LDI", 
          covariate.labels = c("World Autocratization lag",
                               "Region Autocratization lag", 
                               "Neighbor Autocratization lag"), 
          title = "Regression Table LDI Diffusion: Shares", 
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

######################
#### Full OLS LDI ####
######################

## Regression model with control variables for percentage world
summary(share_world <- lm(delta_libdem ~ percentage_world_aut_lag1 + 
                            delta_libdem_lag1 + # Lagged DV
                            e_migdpgrolns + # Ln GDP Growth
                            population + # Ln population
                            e_peaveduc + # Education
                            e_civil_war + # Civil War
                            e_miurbani + # Urbanization
                            e_pt_coup_n + # Coup d'etat
                            as.factor(country_id), # Controlling for the effect of each year
                          data = vdem4))
summary(share_region <- lm(delta_libdem ~ percentage_region_aut_lag1 + 
                             delta_libdem_lag1 + # Lagged DV
                             e_migdpgrolns + # Ln GDP Growth
                             population + # Ln population
                             e_peaveduc + # Education
                             e_civil_war + # Civil War
                             e_miurbani + # Urbanization
                             e_pt_coup_n + # Coup d'etat
                             as.factor(year) +
                             as.factor(country_id), # Controlling for the effect of each year
                           data = vdem4))
summary(share_neighbor <- lm(delta_libdem ~ percentage_neighbor_aut_lag1 + 
                               delta_libdem_lag1 + # Lagged DV
                               e_migdpgrolns + # Ln GDP Growth
                               population + # Ln population
                               e_peaveduc + # Education
                               e_civil_war + # Civil War
                               e_miurbani + # Urbanization
                               e_pt_coup_n + # Coup d'etat
                               as.factor(year) +
                               as.factor(country_id), # Controlling for the effect of each year
                             data = neighbor_percentage))

stargazer(share_world, share_region, share_neighbor, 
          omit = c("country_id", "year"),
          omit.labels = c("Entity fixed effects", "Year Fixed Effects"), 
          type = "latex",
          out = "share_full_LDI.tex", 
          dep.var.labels = "Change in LDI", 
          covariate.labels = c("World Diffusion LDI lag", 
                               "Region Diffusion LDI lag", 
                               "Neighbor Diffusion Change LDI lag",
                               "Change in LDI lag",
                               "Ln GDP Growth", 
                               "Ln Population",
                               "Education",
                               "Civil War",
                               "Urbanization",
                               "Coup d'etat"), 
          title = "Regression Table: Share of Countries Experiencing Autocratization", 
          font.size = "footnotesize", 
          column.sep.width = "-5pt",
          omit.stat = "f",
          no.space = TRUE)

################################
####   LCI vs. EDI Shares   ####
################################

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
                              data = vdem4))
summary(pct_2.2_polyarchy <- lm(delta_polyarchy ~ percentage_region_aut2_lag1, 
                                data = vdem4))

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
stargazer(pct_m1.2_liberal, pct_m1.2_polyarchy, pct_2.2_liberal, 
          pct_2.2_polyarchy, pct_3.2_liberal, pct_3.2_polyarchy,
          type = "text")


summary(share_world_LCI <- lm(delta_liberal ~ percentage_world_aut1_lag1 + 
                                delta_liberal_lag1 + # Lagged DV
                                e_migdpgrolns + # Ln GDP Growth
                                population + # Ln population
                                e_peaveduc + # Education
                                e_civil_war + # Civil War
                                e_miurbani + # Urbanization
                                e_pt_coup_n + # Coup d'etat
                                as.factor(country_id), # Controlling for the effect of each year
                              data = vdem4))
summary(share_region_LCI <- lm(delta_liberal ~ percentage_region_aut1_lag1 + 
                                 delta_liberal_lag1 + # Lagged DV
                                 e_migdpgrolns + # Ln GDP Growth
                                 population + # Ln population
                                 e_peaveduc + # Education
                                 e_civil_war + # Civil War
                                 e_miurbani + # Urbanization
                                 e_pt_coup_n + # Coup d'etat
                                 as.factor(year) +
                                 as.factor(country_id), # Controlling for the effect of each year
                               data = vdem4))
summary(share_neighbor_LCI <- lm(delta_liberal ~ percentage_neighbor_aut1_lag1 + 
                                   delta_liberal_lag1 + # Lagged DV
                                   e_migdpgrolns + # Ln GDP Growth
                                   population + # Ln population
                                   e_peaveduc + # Education
                                   e_civil_war + # Civil War
                                   e_miurbani + # Urbanization
                                   e_pt_coup_n + # Coup d'etat
                                   as.factor(year) +
                                   as.factor(country_id), # Controlling for the effect of each year
                                 data = neighbor_percentage))

summary(share_world_EDI <- lm(delta_polyarchy ~ percentage_world_aut2_lag1 + 
                                delta_polyarchy_lag1 + # Lagged DV
                                e_migdpgrolns + # Ln GDP Growth
                                population + # Ln population
                                e_peaveduc + # Education
                                e_civil_war + # Civil War
                                e_miurbani + # Urbanization
                                e_pt_coup_n + # Coup d'etat
                                as.factor(country_id), # Controlling for the effect of each year
                              data = vdem4))
summary(share_region_EDI <- lm(delta_polyarchy ~ percentage_region_aut2_lag1 + 
                                 delta_polyarchy_lag1 + # Lagged DV
                                 e_migdpgrolns + # Ln GDP Growth
                                 population + # Ln population
                                 e_peaveduc + # Education
                                 e_civil_war + # Civil War
                                 e_miurbani + # Urbanization
                                 e_pt_coup_n + # Coup d'etat
                                 as.factor(year) +
                                 as.factor(country_id), # Controlling for the effect of each year
                               data = vdem4))
summary(share_neighbor_EDI <- lm(delta_polyarchy ~ percentage_neighbor_aut2_lag1 + 
                                   delta_polyarchy_lag1 + # Lagged DV
                                   e_migdpgrolns + # Ln GDP Growth
                                   population + # Ln population
                                   e_peaveduc + # Education
                                   e_civil_war + # Civil War
                                   e_miurbani + # Urbanization
                                   e_pt_coup_n + # Coup d'etat
                                   as.factor(year) +
                                   as.factor(country_id), # Controlling for the effect of each year
                                 data = neighbor_percentage))


stargazer(share_world_LCI, share_region_LCI, share_neighbor_LCI, 
          share_world_EDI, share_region_EDI, share_neighbor_EDI, 
          type = "latex",
          omit = c("country_id", "year"), 
          omit.labels = c("Country Fixed Effects", "Year Fixed Effects"), 
          out = "shares_LCIEDI.tex",
          dep.var.labels = c("Change in LCI", "Change in EDI"), 
          covariate.labels = c("World Diffusion LCI lag", 
                               "Region Diffusion LCI lag", 
                               "Neighbor Diffusion Change LCI lag",
                               "Change in LCI lag",
                               "World Diffusion EDI lag", 
                               "Region Diffusion EDI lag", 
                               "Neighbor Diffusion Change EDI lag",
                               "Change in EDI lag",
                               "Ln GDP Growth", 
                               "Ln Population",
                               "Education",
                               "Civil War",
                               "Urbanization",
                               "Coup d'etat"), 
          title = "Regression Table: Share of Countries Experiencing Autocratization LCI vs EDI", 
          font.size = "footnotesize", 
          column.sep.width = "-10pt",
          no.space = TRUE, 
          omit.stat = c("f", "ser"))

###################################
#### Autocratization Dummy LDI ####
###################################

## 0.05 LDI
vdem5 <- vdem3 %>%
  mutate(autocrat_dummy2_libdem_rev = ifelse(delta_libdem < -0.05, 1, 0)) # Cutoff point at -0.05

table2_libdem <- table(vdem5$autocrat_dummy2_libdem_rev, useNA = "ifany")
table2_libdem
prop.table(table2_libdem)*100

vdem6 <- neighbor_mean %>%
  mutate(autocrat_dummy2_libdem_rev = ifelse(delta_libdem < -0.05, 1, 0)) # Cutoff point at -0.05

## 0.05 LCI
vdem5 <- vdem5 %>%
  mutate(autocrat_dummy2_liberal_rev = ifelse(delta_liberal < -0.05, 1, 0)) # Cutoff point at -0.05

table2_liberal <- table(vdem5$autocrat_dummy2_liberal_rev, useNA = "ifany")
table2_liberal
prop.table(table2_liberal)*100

vdem6 <- vdem6 %>%
  mutate(autocrat_dummy2_liberal_rev = ifelse(delta_liberal < -0.05, 1, 0)) # Cutoff point at -0.05

## 0.05 EDI 
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
          title = "Logistic Regression Table LDI 0.05 Cut-off Point", 
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
          title = "Logistic Regression Table LDI With Controls 0.05 Cut-off Point", 
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


## Simple LCI
summary(logit_dummy2_world_liberal <- glm(autocrat_dummy2_liberal_rev ~ mean_world_change1_rev_lag1, 
                                         data = vdem5, 
                                         family = binomial(link = "logit")))
summary(logit_dummy2_region_liberal <- glm(autocrat_dummy2_liberal_rev ~ mean_region_change1_rev_lag1, 
                                          data = vdem5, 
                                          family = binomial(link = "logit")))
summary(logit_dummy2_neighbor_liberal <- glm(autocrat_dummy2_liberal_rev ~ mean_neighbor_change1_rev_lag1, 
                                            data = vdem6, 
                                            family = binomial(link = "logit")))

summary(logit_dummy2_world_polyarchy <- glm(autocrat_dummy2_polyarchy_rev ~ mean_world_change2_rev_lag1, 
                                          data = vdem5, 
                                          family = binomial(link = "logit")))
summary(logit_dummy2_region_polyarchy <- glm(autocrat_dummy2_polyarchy_rev ~ mean_region_change2_rev_lag1, 
                                           data = vdem5, 
                                           family = binomial(link = "logit")))
summary(logit_dummy2_neighbor_polyarchy <- glm(autocrat_dummy2_polyarchy_rev ~ mean_neighbor_change2_rev_lag1, 
                                             data = vdem6, 
                                             family = binomial(link = "logit")))

stargazer(logit_dummy2_world_liberal, logit_dummy2_region_liberal, logit_dummy2_neighbor_liberal,
          logit_dummy2_world_polyarchy, logit_dummy2_region_polyarchy, logit_dummy2_neighbor_polyarchy,
          type = "text")

stargazer(logit_dummy2_world_liberal, logit_dummy2_region_liberal, logit_dummy2_neighbor_liberal,
          logit_dummy2_world_polyarchy, logit_dummy2_region_polyarchy, logit_dummy2_neighbor_polyarchy, 
          type = "latex", 
          out = "logit2_cutoff_LCIEDI_simple.tex",
          dep.var.labels = c("LCI Autocratization Dummy 2", "EDI Autocratization Dummy 2"), 
          covariate.labels = c("Mean World Change LCI lag", 
                               "Mean Region Change LCI lag", 
                               "Mean Neighbor Change LCI lag", 
                               "Mean World Change EDI lag",
                               "Mean Region Change EDI lag", 
                               "Mean Neighbor Change EDI lag"), 
          digits = 3, 
          no.space = TRUE,
          column.sep.width = "-10pt",
          title = "Logistic Regression Table LCI and EDI 0.05 Cut-off Point", 
          font.size = "footnotesize")

## With control variables
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
          title = "Logistic Regression Table LCI and EDI With Controls 0.05 Cut-off Point", 
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

