######################################
##                                  ##  
##    Script for Master's thesis    ##
##           Spring 2021            ##
##      Sara Kristine Grimstad      ##
##                                  ##
######################################

###################################
####                           ####
####    Introductory things    ####
####                           ####
###################################

#### Setting working directory ####

getwd()
setwd("C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R")

#### Installing and loading packages ####

## Creating a vector with packages to later install and load packages  
packages <- c("dplyr", "tidyverse", "stargazer", "texreg", "xtable", "countrycode", 
              "haven", "ggplot2", "multiwayvcov", "devtools", "knitr", "tidyr", 
              "maps", "mapproj") 

## Applying a function that installs/loads packages to the vector of packages
sapply(packages, 
       FUN = function(x){
         # Check if package is installed. If not, install it:
         if(x %in% rownames(installed.packages()) == FALSE){
           install.packages(x)
         }
         # Load the package:
         library(x, character.only = TRUE)
       })

#### Loading the vdemdata R package from GitHub #### 

## Using the devtools package to install the data from GitHub. Must have an updated version of R installed (>3.5). 
## Rtools must also be installed (version >4.0 is compatible with R >3.5). If not previously installed, it can be 
## downloaded from https://cran.r-project.org/bin/windows/Rtools/
devtools::install_github("vdeminstitute/vdemdata",  
                         force = TRUE)

## Retrieving the vdem dataset from the installed vdemdata package
vdem <- vdemdata::vdem

################################################
####                                        ####
####    Exploring and preparing the data    ####
####                                        ####
################################################

#### Inspecting the data ####

class(vdem) # data frame
range(vdem$year) # Checking the time span, 1789 2019
names(sort(-table(vdem$country_name))) # 202 country names
length(unique(vdem$country_name)) # 202 unique countries
View(vdem)

#### Tidying the data set ####

## Selecting only relevant variables for my analysis and filtering the time period 
vdem2 <- vdem %>%
  select(country_name, country_id, year, country_text_id, COWcode, 
         v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem,
         v2xel_frefair, v2x_frassoc_thick, v2x_suffr, v2x_elecoff, v2x_freexp_altinf, 
         v2x_liberal, v2xcl_rol, v2x_jucon, v2xlg_legcon, v2lgbicam, v2x_regime,
         e_democracy_breakdowns, e_democracy_trans, e_bnr_dem, e_regionpol, 
         e_regiongeo, e_regionpol_6C, e_migdpgro, e_migdpgrolns, e_migdppc, 
         e_migdppcln, e_mipopula, e_miurbani, e_wb_pop, e_civil_war, e_pt_coup) %>%
  filter(year > 1899) %>% # Filter out years prior to 1900
  filter(year < 2020) # Filter out years after 2019

## Inspecting the new data set
head(vdem2)
names(vdem2)
length(unique(vdem2$country_name)) # 183 unique countries
names(sort(-table(vdem2$country_name))) # 183 countries names
range(vdem2$year) # Checking if the filtered time series is correct. It is. 
#range(vdem2$e_wb_pop) # Checking if the filtering of population number is correct. It is.
View(vdem2)

################################################
####                                        ####
####    Tweaking the dependent variables    ####
####                                        ####
################################################

#################################
#### Liberal Democracy Index ####
#################################

#### Descriptive statistics ####

## Brief summary 
summary(vdem2$v2x_libdem)
sd(vdem2$v2x_libdem, na.rm = TRUE)

## Histogram over liberal democracy index to show distribution 
ggplot(vdem2, aes(v2x_libdem)) +
  geom_histogram() +
  theme_bw() + 
  xlab("Liberal democracy index score") + 
  ylab("Number of observations") + 
  xlim(0, 1) +
  ggtitle("Histogram over liberal democracy index")

#### Creating a new variable measuring change in level of democracy #### 

## Variable measuring change in level of democracy for each country from year to year, 
## making it possible to measure autocratization
vdem2 <- vdem2 %>%
  group_by(country_id) %>%
  mutate(delta_libdem = v2x_libdem - lag(v2x_libdem))

## Brief summary
summary(vdem2$delta_libdem)
sd(vdem2$delta_libdem, na.rm = TRUE)

#### Graphical illustrations ####

## Histogram 
ggplot(vdem2, aes(delta_libdem)) +
  geom_histogram() +
  theme_bw() + 
  xlab("Change in liberal democracy index score") + 
  ylab("Amount of observations") + 
  ggtitle("Histogram over yearly change in liberal democracy index")
# The majority of the country-year observations do not experience major change in level of democracy 
# Supports and matches the median, which is 0.000

ggsave("histogram_delta_libdem.png", 
       plot = last_plot())

## Scatterplot across time
ggplot(vdem2, 
       aes(year, delta_libdem)) +
  geom_point() +
  theme_classic() + 
  xlab("Years") + 
  ylab("Change in liberal democracy") + 
  ggtitle("Scatterplot for yearly change in world liberal democracy score \n over time") 
# Outliers before and after WW1, WW2 and around the dissolution of the USSR

ggsave("plot_libdem.png", 
       plot = last_plot()) # CAN ADD OTHER ARGUMENTS, TO CHANGE SIZE ETC

#### Creating dummy variables ####

## Dummy measuring whether there was negative or non-negative change in democracy level
vdem2 <- vdem2 %>%
  mutate(autocrat_dummy_libdem = ifelse(delta_libdem < 0, 1, 0), # Cutoff point at 0
         autocrat_dummy2_libdem = ifelse(delta_libdem < -0.05, 1, 0)) # Cutoff point at -0.05

View(vdem2)
summary(vdem2$autocrat_dummy_libdem)
summary(vdem2$autocrat_dummy2_libdem)

## Frequency tables, absolute and relative distribution
table_libdem <- table(vdem2$autocrat_dummy_libdem)
table_libdem
prop.table(table_libdem)*100

table2_libdem <- table(vdem2$autocrat_dummy2_libdem)
table2_libdem
prop.table(table2_libdem)*100

#### Creating data frame for autocratization episodes ####

## Creating a new data frame measuring how many countries in a year have experienced autocratization, 
## when basing this on all negative change in unique democracy level 
autocrat_trend <- vdem2 %>%
  group_by(year) %>%
  summarise(count_autocrat = sum(autocrat_dummy_libdem, na.rm = TRUE)) ## NB! Must think of the cutoff point 

summary(autocrat_trend$count_autocrat)
View(autocrat_trend)

## Scatterplot for autocratic_trend, showing autocratization episodes over time 
ggplot(autocrat_trend, 
       aes(year, count_autocrat)) +
  geom_smooth() + 
  geom_point() +
  theme_classic() + 
  xlab("Years") + 
  ylab("Number of autocratic downturns") + 
  ggtitle("Scatterplot for annual number of autocratic downturns \n over time")
# The number of countries that during a year experienced autocratization, measured as decline in level of democracy (negative change),
# has increased steadily over time, with an upsurge since the start of the 1990s.

#################################
#### Liberal Component Index ####
#################################

#### Descriptive statistics first ####

## Brief summary 
summary(vdem2$v2x_liberal)
sd(vdem2$v2x_liberal, na.rm = TRUE)

## Histogram over liberal component index to show distribution 
ggplot(vdem2, aes(v2x_liberal)) +
  geom_histogram() +
  theme_bw() + 
  xlab("Liberal component index score") + 
  ylab("Amount of observations") + 
  xlim(0, 1) +
  ggtitle("Histogram over liberal component index")

#### Creating a new variable measuring change in level of liberal component index ####

## Variable measuring change in level of liberal component for each country from year to year, 
## making it possible to measure autocratization in this component
vdem2 <- vdem2 %>%
  group_by(country_id) %>%
  mutate(delta_liberal = v2x_liberal - lag(v2x_liberal))

## Brief summary
summary(vdem2$delta_liberal)
sd(vdem2$delta_liberal, na.rm = TRUE)

#### Graphical illustrations ####

## Scatterplot across time
ggplot(vdem2, 
       aes(year, delta_liberal)) +
  geom_point() +
  theme_classic() + 
  xlab("Years") + 
  ylab("Change in liberal component") + 
  ggtitle("Scatterplot for change in world liberal component democracy \n score over time")

ggsave("plot_liberal.png", 
       plot = last_plot()) # CAN ADD OTHER ARGUMENTS, TO CHANGE SIZE ETC

#### Creating dummy variables ####

## Dummy measuring whether there was negative or non-negative change 
vdem2 <- vdem2 %>%
  mutate(autocrat_dummy_liberal = ifelse(delta_liberal < 0, 1, 0), # Cutoff point at 0
         autocrat_dummy2_liberal = ifelse(delta_liberal < -0.05, 1, 0)) # Cutoff point at -0.05

View(vdem2)
summary(vdem2$autocrat_dummy_liberal)
summary(vdem2$autocrat_dummy2_liberal)

## Frequency tables, absolute and relative distribution
table_liberal <- table(vdem2$autocrat_dummy_liberal)
table_liberal
prop.table(table_liberal)*100

table2_liberal <- table(vdem2$autocrat_dummy2_liberal)
table2_liberal
prop.table(table2_liberal)*100

###################################
#### Electoral Democracy Index ####
###################################

#### Descriptive statistics first ####

## Brief summary
summary(vdem2$v2x_polyarchy)
sd(vdem2$v2x_polyarchy, na.rm = TRUE)

## Histogram over electoral democracy index to show distribution 
ggplot(vdem2, aes(v2x_polyarchy)) +
  geom_histogram() +
  theme_bw() + 
  xlab("Electoral democracy index score") + 
  ylab("Amount of observations") + 
  xlim(0, 1) +
  ggtitle("Histogram over electoral democracy index")

#### Creating a variable measuring change in level of electoral democracy ####

## Variable measuring change in level of electoral democracy for each country from year to year, 
## making it possible to measure autocratization in this component
vdem2 <- vdem2 %>%
  group_by(country_id) %>%
  mutate(delta_polyarchy = v2x_polyarchy - lag(v2x_polyarchy))

## Brief summary
summary(vdem2$delta_polyarchy)
sd(vdem2$delta_polyarchy, na.rm = TRUE)

#### Graphical illustrations ####

## Scatterplot across time
ggplot(vdem2, 
       aes(year, delta_polyarchy)) +
  geom_point() +
  theme_classic() + 
  xlab("Years") + 
  ylab("Change in electoral democracy") + 
  ggtitle("Scatterplot for change in world electoral democracy score \n over time")

ggsave("plot_polyarchy.png", 
       plot = last_plot()) # CAN ADD OTHER ARGUMENTS, TO CHANGE SIZE ETC

#### Creating dummy variables ####

## Dummy measuring whether there was negative or non-negative change
vdem2 <- vdem2 %>%
  mutate(autocrat_dummy_polyarchy = ifelse(delta_polyarchy < 0, 1, 0), # Cutoff point at 0
         autocrat_dummy2_polyarchy = ifelse(delta_polyarchy < -0.05, 1, 0)) # Cutoff point at -0.05

View(vdem2)
summary(vdem2$autocrat_dummy_polyarchy)
summary(vdem2$autocrat_dummy2_polyarchy)

## Frequency tables, absolute and relative distribution
table_polyarchy <- table(vdem2$autocrat_dummy_polyarchy)
table_polyarchy
prop.table(table_polyarchy)*100

table2_polyarchy <- table(vdem2$autocrat_dummy2_polyarchy)
table2_polyarchy
prop.table(table2_polyarchy)*100

############################################
####                                    ####  
####   Creating independent variables   ####
####                                    ####
############################################

#######################################################
#### Creating a variable for world democracy level ####
#######################################################

#### Creating a variable for mean change in world level of democracy minus each country ####

## Adding a variable for mean change in world level of democracy each year minus each country, mean_world_unique
vdem4 <- vdem2 %>%   
  group_by(year) %>% 
  mutate(mean_world_unique = (sum(delta_libdem, na.rm = TRUE) - delta_libdem) / (n() - 1)) 

## Adding a new variable, mean_world_all, measuring mean change in world level of democracy each year to compare with 
## mean_world_unique, and check that this variable in fact subtracts the democracy level for each country
vdem4 <- vdem4 %>%
  group_by(year) %>%
  mutate(mean_world_all = mean(delta_libdem, na.rm = TRUE)) 

View(vdem4)
# The computation of the mean_world_unique variable is successful 

## Brief summary
summary(vdem4$mean_world_unique)
sd(vdem4$mean_world_unique, na.rm=T) # Normal distribution? 

#### Graphical illustrations ####

## Histogram 
ggplot(vdem4, aes(mean_world_unique)) +
  geom_histogram() +
  theme_classic() +
  xlab("Change in mean world democracy level") +
  ylab("Number of observations experiencing change") +
  ggtitle("Histogram for distribution of change in mean world democracy level")
# Majority of observations experience very little change in level of democracy 
  
## Scatterplot
ggplot(vdem4, aes(year, mean_world_unique)) +
  geom_point() + #geom_col
  theme_classic() +
  xlab("Change over time") +
  ylab("Mean change in world democracy level") +
  ggtitle("Scatterplot for annual mean change in world democracy level \n over time")
# Similar to delta_libdem, greatest positive change after WW1, WW2 and dissolution of the USSR 

## Limiting number of digits after comma
#mean_world <- format(round(vdem4$mean_world_unique, 3), nsmall = 3)
#view(mean_world)

## Adding a variable for whether the mean change in world democracy level indicated autocratization or not, 
## thus creating a dichotomous independent variable for negative change in mean level of world democracy 
vdem4 <- vdem4 %>%
  mutate(world_autocrat_dummy = ifelse(mean_world_unique < 0, 1, 0))

View(vdem4)
summary(vdem4$world_autocrat_dummy)

table_world <- table(vdem4$world_autocrat_dummy)
table_world
prop.table(table_world)*100

#### Lagging the variable ####

## Lagging with 1 and 3 years
vdem4$mean_world_unique_lag1 <- ave(vdem4$mean_world_unique, 
                                    vdem4$country_id, 
                                    FUN = function(x){lag(x, 1)})

vdem4$mean_world_unique_lag3 <- ave(vdem4$mean_world_unique, 
                                    vdem4$country_id, 
                                    FUN = function(x){lag(x, 3)})

View(vdem4)

##########################################################
#### Creating a variable for regional democracy level ####
##########################################################

#### Creating a variable for mean change in regional level of democracy minus each region ####

## First examining the three region variables in the data set
table(vdem4$e_regiongeo)
summary(vdem4$e_regiongeo)
table(vdem4$e_regionpol)
summary(vdem4$e_regionpol)
table(vdem4$e_regionpol_6C)
summary(vdem4$e_regionpol_6C)

## Creating a variable for mean regional democracy level for e_regiongeo (19 categories) 
vdem4 <- vdem4 %>%
  group_by(year, e_regiongeo) %>%
  mutate(mean_region_geo = (sum(delta_libdem) - delta_libdem) / (n()-1))
sum(is.na(vdem4$mean_region_geo))

## Creating a variable for mean regional democracy level for e_regionpol (10 categories)
vdem4 <- vdem4 %>%
  group_by(year, e_regionpol) %>%
  mutate(mean_region_pol = (sum(delta_libdem) - delta_libdem) / (n()-1))

## Creating a variable for mean regional democracy level for e_regionpol_6C (6 categories)
vdem4 <- vdem4 %>%
  group_by(year, e_regionpol_6C) %>%
  mutate(mean_region_6C = (sum(delta_libdem) - delta_libdem) / (n()-1))

View(vdem4)

#### Lagging the variable ####

## Lagging e_regiongeo (19 categories) with 1 and 3 years
vdem4$mean_region_geo_lag1 <- ave(vdem4$mean_region_geo,
                                  vdem4$country_id, 
                                  FUN = function(x){lag(x, 1)})

vdem4$mean_region_geo_lag3 <- ave(vdem4$mean_region_geo,
                                  vdem4$country_id, 
                                  FUN = function(x){lag(x, 3)})

## Lagging e_regionpol (10 categories) with 1 and 3 years
vdem4$mean_region_pol_lag1 <- ave(vdem4$mean_region_pol,
                                  vdem4$country_id, 
                                  FUN = function(x){lag(x, 1)})

vdem4$mean_region_pol_lag3 <- ave(vdem4$mean_region_pol,
                                  vdem4$country_id, 
                                  FUN = function(x){lag(x, 3)})

## Lagging e_regionpol_6C (6 categories) with 1 and 3 years
vdem4$mean_region_pol_6C_lag1 <- ave(vdem4$mean_region_pol_6C,
                                     vdem4$country_id, 
                                     FUN = function(x){lag(x, 1)})

vdem4$mean_region_pol_6C_lag3 <- ave(vdem4$mean_region_pol_6C,
                                      vdem4$country_id, 
                                      FUN = function(x){lag(x, 3)})

View(vdem4)

##########################################################
#### Creating a variable for neighbor democracy level ####
##########################################################

#### Creating a variable for mean change in neighboring countries' democracy level minus each country ####

#install.packages(c("rgeos", "rgdal"), type = "source")

## Downloading a COW dataset for contiguous countries. Can be downloaded from: https://correlatesofwar.org/data-sets/direct-contiguity 
cow <- read.csv("contdird.csv")

## Inspecting the data
head(cow)
length(unique(cow$state1ab)) # 215 unique countries
length(unique(cow$state2ab)) # 215 unique countries
range(cow$year) # 1816-2016
View(cow)

## Selecting relevant variables, filtering time period and arranging order of rows 
cow <- cow %>%
  filter(year > 1899) %>% # Filtering out years prior to 1900
  filter(conttype < 5) %>% # Filtering out the greatest water distance border
  select(state1no, state2no, state1ab, state2ab, year, conttype) %>%
  arrange(state1no, year, state2no) # Each state1 now gets paired with each state1 for each year

## Examining the new COW dataset 
View(cow)
names(sort(-table(cow$state1ab))) 
length(unique(cow$state1ab)) # 190 countries
range(cow$year) # 1900-2016 

#### Merging the COW dataset and V-Dem dataset ####

## Using left_join to include all rows in COW (all observations of a country's contiguous neighbor countries)
cow_vdem <- left_join(x = cow, 
                      y = vdem4, 
                      by = c("state2no"="COWcode", "year")) #state2no in first dataset, COWcode in second. year is the same
# I think maybe state2ab from cow corresponds to country_name from vdem, when I want state1ab to correspond to it... Or
# is that not important? ##OBS##

View(cow_vdem)

## Compute mean level of democracy among contiguous neighbors 
cow_vdem_computed <- cow_vdem %>%
  group_by(state1no, year) %>%
  summarise(mean(delta_libdem, na.rm = TRUE)) 
# A new dataset with the mean democracy level among the country's contiguous neighbors each year

## Inspecting the new dataset 
length(unique(cow_vdem_computed$state1no)) # 190 countries
range(cow_vdem_computed$year) # 1900 - 2016 (problem...?) ##OBS##

######################################
####                              ####
####    Descriptive statistics    ####
####                              ####
######################################

#### Exporting a table with descriptive statistics of variables ####

## Creating a new data frame with variables
descriptive <- data.frame(vdem2[, c("v2x_polyarchy", "v2x_libdem", "v2x_regime", "e_pt_coup")])

## Exporting the table
stargazer(descriptive, 
          type = "text", 
          title = "Descriptive statistics", 
          out = "descriptive.txt")

##################################
####                          ####
####    Univariate analysis   ####
####                          ####
##################################

R <- cor(vdem4$delta_libdem, vdem4$mean_world_unique,
         use = "pairwise.complete.obs", 
         method = "pearson")
R # 0.15 (svak positiv korrelasjon)

cor.test(vdem4$delta_libdem, vdem4$mean_world_unique,
         use = "pairwise.complete.obs")
#	95 prosent konfidensintervall mellom 0.134 og 0.162. I 95 av 100 analyser av samme populasjon vil 
# korrelasjonen mellom mellom X og Y befinne seg i intervallet mellom 0.134 og 0.162. 
# Konfidensintervallet befinner seg relativt nærme null, så det er ikke helt sikkert at korrelasjonen er signifikant. 
# Men vi leser av p-verdien (<2e-16), som er lavl og indikerer at det er svært lite sannsynlig at et datasett 
# med en slik korrelasjon vil kunne trekkes ved en tilfeldighet. 
 
## R squared
R_squared <- R^2 
R_squared # 0.02

##########################################
####                                  ####
####   Regression, part 1 of thesis   ####
####                                  ####
##########################################

##########################################################
#### Dependent variable: Change in level of democracy ####
##########################################################

#### Regression forutsetninger ####

#### Does autocratization diffuse? ####

## OLS simple models, only dependent and independent variable 
summary(simple <- lm(delta_libdem ~ mean_world_unique, 
                     data = vdem4))
summary(simple_lag1 <- lm(delta_libdem ~ mean_world_unique_lag1, 
                         data = vdem4))
summary(simple_lag3 <- lm(delta_libdem ~ mean_world_unique_lag3, 
                          data = vdem4))

stargazer(simple, simple_lag1, simple_lag3, 
          type = "text")

## Logistic simple models, only dependent and independent variable

summary(mod_dymmy<- glm(delta_libdem ~ autocrat_dummy_libdem, 
                        data = vdem4, 
                        method = , #random for RE model, and within for FE model
                        family = ))

## Regression model with control variables, lagged UVs and lagged AV, panel corrected standard errors etc...
summary(mod1 <- lm(delta_libdem ~ mean_world_unique +  
                     e_migdppcln + # GDP per capita
                     e_mipopula + # Population
                     e_pt_coup + # Coup d'etat
                     e_miurbani + # Urbanization
                     as.factor(year), # Controlling for the effect of each year
                   data = vdem4))

## Viewing the results in a regression table 
stargazer(mod1_1, mod1,
          omit = "year",
          omit.labels = "Year fixed effects", # WHY NO?? It is included...
          type = "text", 
          out = "regresjontabell.text")

####################################################################
#### Dependent variable: Autocrat Dummy for change in democracy ####
####################################################################

#### Regression forutsetninger ####

#### Does autocratization diffuse? ####

## Simple models
summary(mod2 <- glm(autocrat_dummy_libdem ~ mean_world_unique,
                   data = vdem4, 
                   family = binomial(link = "logit"))) # probit or logit?

summary(mod3 <- glm(autocrat_dummy2_libdem ~ mean_world_unique,
                    data = vdem4, 
                    family = binomial(link = "logit")))

## Viewing the results in a regression table 
stargazer(mod2, mod3, 
          type = "text", 
          out = "regresjonstabell2.text")


stargazer(mod1, mod2, mod3, 
          type = "text", 
          out = "regresjontabell123.text")

plot(vdem4$mean_world_unique, vdem4$delta_libdem) 
abline(mod1_1) 


###########################################
####                                   ####
####    Regression, part 2 of thesis   ####
####                                   ####
###########################################

summary(mod4 <- lm(delta_liberal ~ mean_world_unique, 
                   data = vdem4))

summary(mod5 <- lm(delta_polyarchy ~ mean_world_unique, 
                   data = vdem4))

stargazer(mod4, mod5, 
          type = "text",
          out = "regressiontabell45.text")   

stargazer(mod1, mod4, mod5, 
          type = "text", 
          out = "regressiontabell145.text")

#####################
####             ####
####   Mapping   ####
####             ####
#####################

map("world", fill = TRUE, col = 8, lty = 1)

world_map <- map_data("world")
head(world_map)
p <- ggplot(data = world_map, mapping = aes(x = long, y = lat, 
                                            group = group))
p + geom_polygon(fill = "lightblue", color = "black")

p <- ggplot(data = world_map, aes(long, lat, 
                                  group = group, 
                                  fill = region))
p + geom_polygon(color = "gray90", size = 0.1) +
  guides(fill = FALSE)
# Different map projection
p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 45) +
  guides(fill = FALSE)

## TEST 
autocrat_colors <- c("red", "blue")

p0 <- ggplot(data = vdem4, 
             aes(x=delta_libdem, 
                 y = reorder(country_name, delta_libdem), 
                 color = as.factor(autocrat_dummy_libdem)))

p1 <- p0 + geom_vline(xintercept = 0, color = "gray30") +
  geom_point(size = 2)

p2 <- p1 + scale_color_manual(values = autocrat_colors)

p3 <- p2 + scale_x_continuous(breaks = c(-1, -0.7, -0.4, 0, 0.4, 0.7, 1), 
                              labels = c("1\n (Autocratic)", "0.7", "0.4", "0", 
                              "0.4", "0.7", "1\n(Democratic"))

p3 + facet_wrap(~ e_regionpol_6C, ncol = 1, scales = "free_y") + 
             guides(color=FALSE) + labs(x="Point Margin", y="") + 
  theme(axis.text=element_text(size=8))
