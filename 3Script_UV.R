############################################
####                                    ####  
####   Creating independent variables   ####
####                                    ####
############################################

#########################
####                 ####
#### World Diffusion ####
####                 ####
#########################

#################################################
#### Mean change in world level of democracy ####
#################################################

#### Mean change in world democracy level ####

## Adding a variable for mean change in world level of democracy each year, minus the current country observation
vdem3 <- vdem2 %>%   
  group_by(year) %>% 
  mutate(mean_world_change = (sum(delta_libdem, na.rm = TRUE) - delta_libdem) / (n() - 1), 
         mean_world_change1 = (sum(delta_liberal, na.rm = TRUE) - delta_liberal) / (n() - 1), 
         mean_world_change2 = (sum(delta_polyarchy, na.rm = TRUE) - delta_polyarchy) / (n() -1), 
         mean_world_change_rev = (sum(delta_libdem_rev, na.rm = TRUE) - delta_libdem_rev) / (n() - 1), 
         mean_world_change1_rev = (sum(delta_liberal_rev, na.rm = TRUE) - delta_liberal_rev) / (n() - 1), 
         mean_world_change2_rev = (sum(delta_polyarchy_rev, na.rm = TRUE) - delta_polyarchy_rev) / (n() -1))

View(vdem3)

#### Lagging the variables ####

## Lagging with 1 year to include in main analysis 
vdem3$mean_world_change_lag1 <- ave(vdem3$mean_world_change, 
                                    vdem3$country_id, 
                                    FUN = function(x){dplyr::lag(x, 1)})
vdem3$mean_world_change1_lag1 <- ave(vdem3$mean_world_change1, 
                                     vdem3$country_id, 
                                     FUN = function(x){dplyr::lag(x, 1)})
vdem3$mean_world_change2_lag1 <- ave(vdem3$mean_world_change2, 
                                     vdem3$country_id, 
                                     FUN = function(x){dplyr::lag(x, 1)})

vdem3$mean_world_change_rev_lag1 <- ave(vdem3$mean_world_change_rev, 
                                    vdem3$country_id, 
                                    FUN = function(x){dplyr::lag(x, 1)})
vdem3$mean_world_change1_rev_lag1 <- ave(vdem3$mean_world_change1_rev, 
                                     vdem3$country_id, 
                                     FUN = function(x){dplyr::lag(x, 1)})
vdem3$mean_world_change2_rev_lag1 <- ave(vdem3$mean_world_change2_rev, 
                                     vdem3$country_id, 
                                     FUN = function(x){dplyr::lag(x, 1)})
View(vdem3)


## Brief summary statistics
summary(vdem3$mean_world_change_lag1) # LDI
summary(vdem3$mean_world_change1_lag1) # LCI
summary(vdem3$mean_world_change2_lag1) # EDI
summary(vdem3$mean_world_change_rev_lag1) # LDI reversed
summary(vdem3$mean_world_change1_rev_lag1) # LCI reversed
summary(vdem3$mean_world_change2_rev_lag1) # EDI reversed

#### Graphical illustrations ####

## Histogram, liberal democracy index 
ggplot(vdem3, aes(mean_world_change_rev_lag1)) +
  geom_histogram(binwidth = 0.003, col = "white", fill = "lightcoral") +
  theme_classic() +
  labs(x = "Mean change", 
       y = "Number of observations", 
       title = "Change in mean world democracy level", 
       subtitle = "Liberal Democracy Index (LDI)")
# Majority of observations experience very little change in level of democracy 

ggsave("histogram_LDI_mean_world_change.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Histogram, liberal component index
ggplot(vdem3, aes(mean_world_change1_rev_lag1)) +
  geom_histogram(binwidth = 0.003, col = "white", fill = "lightcoral") +
  theme_classic() +
  labs(x = "Mean change", 
       y = "Number of observations", 
       title = "Change in mean world democracy level", 
       subtitle = "Liberal Component Index (LCI)")

ggsave("histogram_LCI_mean_world_change.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Histogram, electoral democracy index
ggplot(vdem3, aes(mean_world_change2_rev_lag1)) +
  geom_histogram(binwidth = 0.003, col = "white", fill = "lightcoral") +
  theme_classic() +
  labs(x = "Mean change", 
       y = "Number of observations", 
       title = "Change in mean world democracy level", 
       subtitle = "Electoral Democracy Index (EDI)")

ggsave("histogram_EDI_mean_world_change.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

############################
####                    ####
#### Regional diffusion ####
####                    ####
############################

############################################################################
#### Creating a variable for mean change in regional level of democracy ####
############################################################################

#### Mean regional democracy level ####

## First examining the region variable
table(vdem3$e_regionpol) # 10 categories
vdem3$e_regionpol <- as.factor(vdem3$e_regionpol)
ggplot(vdem3, aes(e_regionpol)) +
  geom_bar() 

## Creating a variable for mean regional democracy level for e_regionpol minus each country-obs
vdem3 <- vdem3 %>%
  group_by(e_regionpol, year) %>%
  mutate(mean_region_change = (sum(delta_libdem, na.rm = TRUE) - delta_libdem) / (n()-1), 
         mean_region_change1 = (sum(delta_liberal, na.rm = TRUE) - delta_liberal) / (n()-1),
         mean_region_change2 = (sum(delta_polyarchy, na.rm = TRUE) - delta_polyarchy) / (n()-1), 
         mean_region_change_rev = (sum(delta_libdem_rev, na.rm = TRUE) - delta_libdem_rev) / (n()-1), 
         mean_region_change1_rev = (sum(delta_liberal_rev, na.rm = TRUE) - delta_liberal_rev) / (n()-1),
         mean_region_change2_rev = (sum(delta_polyarchy_rev, na.rm = TRUE) - delta_polyarchy_rev) / (n()-1))

View(vdem3)

#### Lagging the variables ####

## Lagging the region variable with 1 year
vdem3$mean_region_change_lag1 <- ave(vdem3$mean_region_change,
                                     vdem3$country_id, 
                                     FUN = function(x){dplyr::lag(x, 1)})
vdem3$mean_region_change1_lag1 <- ave(vdem3$mean_region_change1,
                                     vdem3$country_id, 
                                     FUN = function(x){dplyr::lag(x, 1)})
vdem3$mean_region_change2_lag1 <- ave(vdem3$mean_region_change2,
                                     vdem3$country_id, 
                                     FUN = function(x){dplyr::lag(x, 1)})

vdem3$mean_region_change_rev_lag1 <- ave(vdem3$mean_region_change_rev,
                                     vdem3$country_id, 
                                     FUN = function(x){dplyr::lag(x, 1)})
vdem3$mean_region_change1_rev_lag1 <- ave(vdem3$mean_region_change1_rev,
                                      vdem3$country_id, 
                                      FUN = function(x){dplyr::lag(x, 1)})
vdem3$mean_region_change2_rev_lag1 <- ave(vdem3$mean_region_change2_rev,
                                      vdem3$country_id, 
                                      FUN = function(x){dplyr::lag(x, 1)})

View(vdem3)

## Brief summary statistics
summary(vdem3$mean_region_change_lag1)
summary(vdem3$mean_region_change1_lag1)
summary(vdem3$mean_region_change2_lag1)
summary(vdem3$mean_region_change_rev_lag1)
summary(vdem3$mean_region_change1_rev_lag1)
summary(vdem3$mean_region_change2_rev_lag1)

#### Graphical illustrations ####

## Histogram, LDI
ggplot(vdem3, aes(mean_region_change_rev_lag1)) +
  geom_histogram(fill = "grey60", col = "white", binwidth = 0.01) +
  theme_classic() +
  labs(x = "Mean change", 
       y = "Number of observations", 
       title = "Change in mean region democracy level", 
       subtitle = "Liberal Democracy Index (LDI)")
# Majority of regional observations experience very little change in level of regional democracy 

## Line plot, LDI
ggplot(vdem3, aes(e_regionpol, mean_region_change_rev_lag1, color = e_regionpol)) + 
  geom_line() +
  labs(x = "Region (politico-geographic)", 
       y = "Mean change", 
       title = "Mean change in regional democracy level", 
       subtitle = "Liberal Democracy Index")
 
## Scatterplot, LDI
ggplot(vdem3, aes(year, mean_region_change_rev_lag1)) +
  geom_point(alpha=0.2, size = 1) +
  theme_classic() +
  labs(x = "Change over time", 
       y = "Mean change", 
       title = "Scatterplot for annual mean change in regional democracy level \n over time", 
       subtitle = "Liberal Democracy Index")
# Greatest positive change after WW1, WW2 and dissolution of the USSR, greatest negative 
# change prior to WW", and somewhat more after the turn of the millenium

#### Graphical illustrations ####

## Creating a vector for e_regionpol with pertaining region names
strip.labs <- c("1" = "Eastern Europe and post USSR", 
                "2" = "Latin America", 
                "3" = "North Africa and Middle East", 
                "4" = "Sub-Saharan Africa", 
                "5" = "Western Europe and North America", 
                "6" = "East Asia", 
                "7" = "South-East Asia", 
                "8" = "South Asia", 
                "9" = "The Pacific excl. AUS + NZL", 
                "10" = "The Carribean")

## Scatterplot, mean LDI
ggplot(vdem3, aes(year, mean_region_change_rev_lag1, color = e_regionpol)) +
  geom_point() + 
  geom_smooth(color = "black") + 
  theme_classic() +
  facet_wrap(vars(e_regionpol)) +
  facet_rep_wrap(~ e_regionpol, scales = "free_y",  #### Free or fixed?? ####
                 repeat.tick.labels = 'all', labeller = as_labeller(strip.labs)) +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(strip.text = element_text(size = 10)) +
  labs(x = "Year", 
       y = "Mean change", 
       title = "Scatterplot for mean change in regional democracy level 1900-2019", 
       subtitle = "Liberal Democracy Index (LDI)")
# Countries in category 5 (Western Europe + North America) was affected by WW2, 
# and countries in category 2 (Eastern Europe + post USSR) was affected by the end of the cold war, 
# while some countries in cateory 10 (The Carribean) was affected around year 2000

ggsave("scatter_mean_LDI_e_region.png", plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Scatterplot, mean LCI
ggplot(vdem3, aes(year, mean_region_change1_rev_lag1, color = e_regionpol)) +
  geom_point() + 
  geom_smooth(color = "black") + 
  theme_classic() +
  facet_wrap(vars(e_regionpol)) +
  facet_rep_wrap(~ e_regionpol, scales = "free_y",  #### Free or fixed?? ####
                 repeat.tick.labels = 'all', labeller = as_labeller(strip.labs)) +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(strip.text = element_text(size = 10)) +
  labs(x = "Year", 
       y = "Mean change", 
       title = "Scatterplot for mean change in regional democracy level 1900-2019", 
       subtitle = "Liberal Component Index (LCI)")

ggsave("scatter_mean_LCI_e_region.png", plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Scatterplot, mean EDI
ggplot(vdem3, aes(year, mean_region_change2_rev_lag1, color = e_regionpol)) +
  geom_point() + 
  geom_smooth(color = "black") + 
  theme_classic() +
  facet_wrap(vars(e_regionpol)) +
  facet_rep_wrap(~ e_regionpol, scales = "free_y",  #### Free or fixed?? ####
                 repeat.tick.labels = 'all', labeller = as_labeller(strip.labs)) +
  theme(legend.title = element_blank()) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(strip.text = element_text(size = 10)) +
  labs(x = "Year", 
       y = "Mean change", 
       title = "Scatterplot for mean change in regional democracy level 1900-2019", 
       subtitle = "Electoral Democracy Index (EDI)")

ggsave("scatter_mean_EDI_e_region.png", plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

##################################
####                          ####
#### Neighbor democracy level ####
####                          ####
##################################

#######################################################################################
#### Creating a variable for mean change in neighboring countries' democracy level ####
#######################################################################################

## Downloading a COW dataset for contiguous countries. Can be downloaded from: 
# https://correlatesofwar.org/data-sets/direct-contiguity 
cow <- read.csv("contdird.csv")

## Inspecting the data
head(cow)
length(unique(cow$state1ab)) # 215 unique countries
length(unique(cow$state2ab)) # 215 unique countries registered as neighbors
range(cow$year) # 1816-2016
View(cow)

## Selecting relevant variables, filtering time period and arranging order of rows 
cow <- cow %>%
  filter(year > 1899) %>% # Filtering out years prior to 1900
  filter(conttype < 5) %>% # Filtering out the greatest water distance border
  select(state1no, state2no, state1ab, state2ab, year, conttype) %>% # Selecting only relevant variables 
  arrange(state1no, year, state2no) # Each state1 now gets matched with every neighbor state for each year

## Examining the new COW dataset 
head(cow)
length(unique(cow$state1ab)) # 190 countries
names(sort(-table(cow$state1ab)))
range(cow$year) # 1900-2016 
View(cow)

## Comparing COW code numbers in cow and vdem3
setdiff(cow$state1no, vdem3$COWcode)
setdiff(vdem3$COWcode, cow$state1no)
# Countries not in cow is countries with no contiguous neighbors, like New Zealand and Iceland

#### Merging the COW dataset and V-Dem dataset ####

## Using left_join to include all rows in COW (all observations of a country's contiguous neighbor countries)
cow_vdem <- left_join(x = cow, # includes all rows in COW
                      y = vdem3, 
                      by = c("state2no"="COWcode", "year")) # state2no in first dataset equals COWcode in second. 
                                                            # Year is the same

View(cow_vdem) 
range(cow_vdem$year) # 1900-2016 
names(sort(-table(cow_vdem$state1ab)))
length(unique(cow_vdem$state1ab)) # 190 post merging, like in cow

#### Computing mean level of democracy among a country's contiguous neighbors ####

## Mean neighbor democracy level
cow_vdem_mean <- cow_vdem %>%
  group_by(state1no, year) %>%  
  summarise(mean_neighbor_change = sum(delta_libdem, na.rm = TRUE) / n(), 
            mean_neighbor_change1 = sum(delta_liberal, na.rm = TRUE) / n(), 
            mean_neighbor_change2 = sum(delta_polyarchy, na.rm = TRUE) / n(), 
            mean_neighbor_change_rev = sum(delta_libdem_rev, na.rm = TRUE) / n(), 
            mean_neighbor_change1_rev = sum(delta_liberal_rev, na.rm = TRUE) / n(),
            mean_neighbor_change2_rev = sum(delta_polyarchy_rev, na.rm = TRUE) / n())

View(cow_vdem_mean)

#### Lagging the variables ####

## Lagging with 1 year
cow_vdem_mean$mean_neighbor_change_lag1 <- ave(cow_vdem_mean$mean_neighbor_change,
                                               cow_vdem_mean$state1no,
                                               FUN = function(x){dplyr::lag(x, 1)})
cow_vdem_mean$mean_neighbor_change1_lag1 <- ave(cow_vdem_mean$mean_neighbor_change1,
                                               cow_vdem_mean$state1no,
                                               FUN = function(x){dplyr::lag(x, 1)})
cow_vdem_mean$mean_neighbor_change2_lag1 <- ave(cow_vdem_mean$mean_neighbor_change2,
                                               cow_vdem_mean$state1no,
                                               FUN = function(x){dplyr::lag(x, 1)})

cow_vdem_mean$mean_neighbor_change_rev_lag1 <- ave(cow_vdem_mean$mean_neighbor_change_rev,
                                               cow_vdem_mean$state1no,
                                               FUN = function(x){dplyr::lag(x, 1)})
cow_vdem_mean$mean_neighbor_change1_rev_lag1 <- ave(cow_vdem_mean$mean_neighbor_change1_rev,
                                                cow_vdem_mean$state1no,
                                                FUN = function(x){dplyr::lag(x, 1)})
cow_vdem_mean$mean_neighbor_change2_rev_lag1 <- ave(cow_vdem_mean$mean_neighbor_change2_rev,
                                                cow_vdem_mean$state1no,
                                                FUN = function(x){dplyr::lag(x, 1)})

## Inspecting the new dataset 
View(cow_vdem_mean)
length(unique(cow_vdem_mean$state1no)) # 190 countries
range(cow_vdem_mean$year) # 1900-2016
summary(cow_vdem_mean)

#### Merging cow_vdem_mean with vdem3 ####

## Merging with vdem3 again 
neighbor_mean <- left_join(x = cow_vdem_mean, # includes all rows in cow_vdem_mean
                           y = vdem3, 
                           by = c("state1no"="COWcode", "year")) # state1no in first dataset equals COWcode in second. 
                                                                 # Year is the same

View(neighbor_mean)

