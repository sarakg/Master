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

#### Installing and loading packages ####

## Creating a vector with packages to later install and load packages  
packages <- c("tidyverse", "stargazer", "texreg", "xtable", "countrycode", 
              "haven", "ggplot2", "multiwayvcov", "knitr", "tidyr", "maps", 
              "mapproj", "plm", "lemon", "readxl","tmap", "lmtest", "tseries", 
              "prais", "car", "jtools", "effects") 

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

## Using the devtools package to install the data from GitHub. Must have an 
## updated version of R installed (>3.5). Rtools must also be installed 
## (version >4.0 is compatible with R >3.5). If not previously installed, it 
## can be downloaded from https://cran.r-project.org/bin/windows/Rtools/
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

range(vdem$year) # Checking the time span, 1789-2019
length(unique(vdem$country_id)) # 202 unique countries
names(sort(-table(vdem$country_name))) # 202 country names

#### Tidying the data ####

## Selecting only relevant variables for my analysis and filtering the time period 
vdem2 <- vdem %>%
  select(country_name, country_id, year, country_text_id, COWcode, v2x_libdem,
         v2x_liberal, v2x_polyarchy, v2x_freexp_altinf, v2x_frassoc_thick, 
         v2x_suffr, v2xel_frefair, v2x_elecoff, v2x_liberal, v2xcl_rol, 
         v2x_jucon, v2xlg_legcon, v2x_regime, e_regionpol, e_regiongeo, 
         e_regionpol_6C, e_migdpgro, e_migdpgrolns, e_migdppc, e_migdppcln, 
         e_miurbani, e_miurbpop, e_civil_war, e_miinteco, e_miinterc, 
         e_pt_coup, e_peaveduc) %>%
  filter(year > 1899) %>% # Filter out years prior to 1900
  filter(year < 2020) # Filter out years after 2019

## Inspecting the new data set
head(vdem2)
names(vdem2) # Checking if all variables got included in the new data frame
length(unique(vdem2$country_id)) # 183 unique countries
names(sort(-table(vdem2$country_name))) # 183 countries names
range(vdem2$year) # Checking if the filtered time series is correct. It is. 
View(vdem2)
     