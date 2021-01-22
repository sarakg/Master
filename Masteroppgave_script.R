####################################
##                                ##  
##   Script for Master's thesis   ##
##         Sara Grimstad          ##
##                                ##
####################################

#############################
#### Introductory things ####
#############################

#### Setting working directory ####

getwd()
setwd("C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R")

#### Installing and loading packages ####

## Creating a vector with packages to later install and load packages  
packages <- c("dplyr", "tidyverse", "stargazer", "texreg", "xtable", "countrycode", 
              "haven", "ggplot2", "multiwayvcov", "devtools") 

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
## Rtools must also be installed. (version >4.0 is compatible with R >3.5) If not previosuly installed, it can be 
## downloaded from https://cran.r-project.org/bin/windows/Rtools/
##devtools::install_github("vdeminstitute/vdemdata", 
##                         force = TRUE)

## Retrieving the vdem data set from the installed vdemdata package
# data("vdem")
# vdem <- data
# data("codebook")
# codebook <- codebook

## Loading downloaded vdem data from https://github.com/vdeminstitute/vdemdata/tree/master/data 
load("vdem.RData") 

##########################################
#### Exploring and preparing the data ####
##########################################

#### Inspecting the data ####

View(vdem)
class(vdem) # data frame
dim(vdem) # 27013 observations and 4108 variables
range(vdem$year) # Checking the time span, 1789 2019
names(sort(-table(vdem$country_name))) # 202 countries
length(unique(vdem$country_name)) # 202 unique country names
#table(is.na(vdem))

#### Tidying the data set ####

## Selecting only relevant variables for my analysis and kicking out the others 
vdem2 <- vdem %>%
  select(country_name, country_id, year, country_text_id, COWcode, 
         v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem,
         v2xel_frefair, v2x_frassoc_thick, v2x_suffr, v2x_elecoff, v2x_freexp_altinf, 
         v2x_liberal, v2xcl_rol, v2x_jucon, v2xlg_legcon, v2lgbicam, v2x_regime,
         e_democracy_breakdowns, e_democracy_trans, e_bnr_dem, 
         e_regionpol, e_regiongeo, e_migdpgro, e_migdpgrolns, e_migdppc, 
         e_mipopula, e_miurbani, e_wb_pop, e_civil_war, e_pt_coup) %>%
  filter(year > 1899) %>% # Filter out years prior to 1919
  filter(year < 2020) # Filter out years after 2019

## Inspecting the new data set
View(vdem2)
head(vdem2)
names(vdem2)
dim(vdem2) 
length(unique(vdem2$country_name)) # 183 unique countries
names(sort(-table(vdem2$country_name))) # 183 countries names
range(vdem2$year) # Checking if the filtered time series is correct. It is. 
#range(vdem2$e_wb_pop) # Checking if the filtering of population number is correct. It is.
vdem2 %>%
  complete(country_name, year)

#########################################
#### Tweaking the dependent variable ####
#########################################

#### Descriptive analysis first ####

## Examining the measurement level of v2x_libdem (Liberal Democracy Index)
as.character(vdem2$v2x_libdem)
class(vdem2$v2x_libdem) # numeric, meaning a continuous measure

## Brief summary with descriptive statistics
summary(vdem2$v2x_libdem)

## Histogram over Liberal democracy index (v2x_libdem) to show distribution (continuous measure)
hist(vdem2$v2x_libdem, 
     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), # Breaking into 10 categories
     main = "Histogram over Liberal democracy index \n (v2x_libdem)", 
     xlab = "Liberal democracy index score",
     ylab = "Amount of observations",
     col = "grey")

#### Creating a new variable measuring change in level of democracy #### 

## I need a variable measuring change in level of democracy for each country from year to year, 
## making it possible to measure autocratization, which is decline in the level of democracy
vdem2 <- vdem2 %>%
  group_by(country_id) %>%
  mutate(delta_libdem = v2x_libdem - lag(v2x_libdem))

summary(vdem2$delta_libdem)

## Histogram over change in computed liberal democracy index score to show distribution 
hist(vdem2$delta_libdem,
     main = "Histogram over amound of change in score on \n Liberal democracy index",
     xlab = "Change in liberal democracy index score",
     ylab = "Number of observations", 
     col = "grey")

########################################
#### Creating independent variables ####
########################################

#### Creating a variable for world democracy level ####

## World democracy level measured as mean change in level of democracy for all countries each year
## i.e. looking at mean change for all countries for each year 

vdem2$delta_libdem[is.na(vdem2$delta_libdem)] <- 0

# Checking if making all NAs to 0 gjør stort utslag 
summary(vdem2$delta_libdem)
hist(vdem2$delta_libdem)

## Creating a variable for mean change in world level of democracy 
vdem3 <- vdem2 %>%
  group_by(year) %>%
  mutate(mean_world = mean(delta_libdem))

## Mikkels løsning
mean_world = aggregate(delta_libdem ~ year, vdem2, mean)

vdem6 = merge(vdem2, 
              mean_world, 
              by = "year",
              all = TRUE) 

## When looking at the effect of mean change in world democracy level on change in democracy level for 
## a specific country, I must remove that specific country    
# vdem2 <- vdem2 %>%
#  mean_world in %>% rownames("country")

#### Creating a variable for regional democracy level ####
#### measured as mean change in level of democracy ####
# mean_region =

#### Creating a variable for neighbour democracy level ####
#### measured as mean change in level of democracy ####
# mean_neighbour 

# AV/y: Endring i demokratinivå i de enkelte landene i ett år. UV/x: Demokratinivå i resten av verden eller 
# gjenommsnittsdemokratinivået blant naboland ett eller to år før feks. 
# Må også lage en egen downturn variabel (autokratiseringsvariabel), en dikotom variabel som har verdien 1
# i Ungarn i 2005 dersom det var downturn (f.eks. -0,1 eller -0,2)

################################
#### Descriptive statistics ####
################################

summary(vdem2$v2x_polyarchy)
summary(vdem2)

## Histogram over Electoral democracy index (v2x_polyarchy) to show distribution (continuous measure)
hist(vdem2$v2x_polyarchy, 
     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), # Breaking into 10 categories
     main = "Histogram over Electoral democracy index \n (v2x_polyarchy)", 
     xlab = "Electoral democracy index score",
     ylab = "Amount of observations",
     col = "grey")

## Histogram over Liberal democracy index (v2x_libdem) to show distribution (continuous measure)
hist(vdem2$v2x_libdem, 
     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), # Breaking into 10 categories
     main = "Histogram over Liberal democracy index \n (v2x_libdem)", 
     xlab = "Liberal democracy index score",
     ylab = "Amount of observations",
     col = "grey")


plot(x = vdem2$e_democracy_trans, 
     y = vdem2$v2x_polyarchy)

#### Exporting a table with descriptive statistics of variables ####

## Creating a new data frame with variables
descriptive <- data.frame(vdem2[, c("v2x_polyarchy", "v2x_libdem", "v2x_regime", "e_pt_coup")])

## Exporting the table LATEX?!?!
stargazer(descriptive, 
          type = "text", 
          title = "Descriptive statistics", 
          out = "descriptive.txt")
