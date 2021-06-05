###############################
####                       ####
####   Control variables   ####
####                       ####
###############################

#####################################
#### Lagging Dependent variables ####
#####################################

#### Lagging with 1 year to include as independent variable in main analysis ####

## Change in LDI
vdem3$delta_libdem_lag1 <- ave(vdem3$delta_libdem, 
                               vdem3$country_id, 
                               FUN = function(x){dplyr::lag(x, 1)})
vdem3$delta_libdem_rev_lag1 <- ave(vdem3$delta_libdem_rev, 
                                   vdem3$country_id, 
                                   FUN = function(x){dplyr::lag(x, 1)})
neighbor_mean$delta_libdem_lag1 <- ave(neighbor_mean$delta_libdem, 
                                       neighbor_mean$country_id, 
                                       FUN = function(x){dplyr::lag(x, 1)})
neighbor_mean$delta_libdem_rev_lag1 <- ave(neighbor_mean$delta_libdem_rev, 
                                           neighbor_mean$country_id, 
                                           FUN = function(x){dplyr::lag(x, 1)})

## Change in LCI
vdem3$delta_liberal_lag1 <- ave(vdem3$delta_liberal, 
                                vdem3$country_id, 
                                FUN = function(x){dplyr::lag(x, 1)})
vdem3$delta_liberal_rev_lag1 <- ave(vdem3$delta_liberal_rev,
                                    vdem3$country_id, 
                                    FUN = function(x){dplyr::lag(x, 1)})
neighbor_mean$delta_liberal_lag1 <- ave(neighbor_mean$delta_liberal, 
                                       neighbor_mean$country_id, 
                                       FUN = function(x){dplyr::lag(x, 1)})
neighbor_mean$delta_liberal_rev_lag1 <- ave(neighbor_mean$delta_liberal_rev, 
                                        neighbor_mean$country_id, 
                                        FUN = function(x){dplyr::lag(x, 1)})

## Change in EDI
vdem3$delta_polyarchy_lag1 <- ave(vdem3$delta_polyarchy, 
                                  vdem3$country_id, 
                                  FUN = function(x){dplyr::lag(x, 1)})
vdem3$delta_polyarchy_rev_lag1 <- ave(vdem3$delta_polyarchy_rev, 
                                  vdem3$country_id, 
                                  FUN = function(x){dplyr::lag(x, 1)})
neighbor_mean$delta_polyarchy_lag1 <- ave(neighbor_mean$delta_polyarchy,
                                          neighbor_mean$country_id, 
                                          FUN = function(x){dplyr::lag(x, 1)})
neighbor_mean$delta_polyarchy_rev_lag1 <- ave(neighbor_mean$delta_polyarchy_rev, 
                                       neighbor_mean$country_id, 
                                       FUN = function(x){dplyr::lag(x, 1)})

##############################################
#### Loading Gapminder population dataset ####
##############################################

## Can be found at https://www.gapminder.org/data/, must be saved in working directory
gapminder <- read_excel("population_total.xlsx")

## Changing from wide format to long format, and tidying the dataset
gapminder <- gapminder %>%
  select("country", "1900":"2019") %>%
  gather(year, value, -country) %>%
  arrange(country, year, value) %>%
  rename(population = value) %>%
  group_by(country) %>%
  mutate(population = log(population))

names(sort(-table(gapminder$country)))
range(gapminder$year) # 1900-2019
gapminder$year <- as.numeric(gapminder$year)

## Comparing country names
setdiff(gapminder$country, vdem3$country_name)
setdiff(vdem3$country_name, gapminder$country)

## Changing country names in the gapminder dataset to correspond to country names in vdem3
gapminder$country <- ifelse(gapminder$country == "Congo, Dem. Rep.", "Democratic Republic of the Congo", 
                            ifelse(gapminder$country == "Congo, Rep.", "Republic of the Congo", 
                                   ifelse(gapminder$country == "Cote d'Ivoire", "Ivory Coast", 
                                          ifelse(gapminder$country == "Gambia", "The Gambia", 
                                                 ifelse(gapminder$country == "Kyrgyz Republic", "Kyrgyzstan", 
                                                        ifelse(gapminder$country == "Lao", "Laos", 
                                                               ifelse(gapminder$country == "Myanmar", "Burma/Myanmar", 
                                                                      ifelse(gapminder$country == "Slovak Republic", "Slovakia",
                                                                             ifelse(gapminder$country == "United States", "United States of America", 
                                                                                    gapminder$country)))))))))
names(sort(-table(gapminder$country)))

#### Mean ####

## Merging vdem3 and gapminder (mean)
vdem3 <- left_join(x = vdem3, # Includes all rows in vdem3
                   y = gapminder, 
                   by = c("country_name"="country", "year"))

View(vdem3)

## Merging neighbor_mean and gapminder (mean)
neighbor_mean <- left_join(x = neighbor_mean, # Includes all rows in neighbor_mean
                           y = gapminder, 
                           by = c("country_name"="country", "year"))

View(neighbor_mean)

#### Percentage ####

## Merging vdem4 and gapminder (percentage)
vdem4 <- left_join(x = vdem4, # Includes all rows in vdem4
                   y = gapminder, 
                   by = c("country_name"="country", "year"))

## Merging neighbor_percentage and gapminder (percentage)
population2_pct <- left_join(x = vdem4, # Includes all rows in vdem4
                             y = gapminder,  
                             by = c("country_name"="country", "year"))

################################
#### Recoding Coup-variable ####
################################

## Coup d'etat
vdem3 <- vdem3 %>%
  mutate(e_pt_coup_n = ifelse(e_pt_coup >= 1, 1, 0))

neighbor_mean <- neighbor_mean %>%
  mutate(e_pt_coup_n = ifelse(e_pt_coup >= 1, 1, 0))



