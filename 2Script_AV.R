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

## Histogram over liberal democracy index to show distribution 
ggplot(vdem2, aes(v2x_libdem)) +
  geom_histogram(fill = "grey60", col = "white") +
  theme_classic() + 
  labs(x = "Liberal democracy index score", 
       y = "Number of observations", 
       title = "Histogram over Liberal Democracy Index (LDI)", 
       caption = "Source: Coppedge et al., (2020)")

#### Creating a new variable measuring change in level of democracy #### 

## Variable measuring change in the liberal democracy index for each country from year to year, 
## making it possible to measure autocratization
vdem2 <- vdem2 %>%
  group_by(country_id) %>%
  mutate(delta_libdem = v2x_libdem - dplyr::lag(v2x_libdem))

View(vdem2)

## Brief summary
summary(vdem2$delta_libdem)

#### Graphical illustrations ####

## Histogram 
ggplot(vdem2, aes(delta_libdem)) +
  geom_histogram(fill = "grey60") +
  theme_classic() + 
  labs(x = "Change", 
       y = "Number of observations", 
       title = "Histogram over yearly change in LDI")
# The majority of the country-year observations do not experience major change in level of democracy 
# Supports and matches the median, which is 0.000

ggsave("histogram_delta_libdem.png", 
       plot = last_plot(), 
       width = 200, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Scatterplot across time
ggplot(vdem2, aes(year, delta_libdem)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  labs(x = "Year",
       t = "Change in LDI",
       title = "Scatterplot for yearly change in LDI over time")
# Outliers before and after WW1, WW2 and around the dissolution of the USSR

ggsave("scatterplot_delta_libdem_time.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots") 

#### Creating dummy variables ####

## Dummy measuring whether there was negative or non-negative change in democracy level
vdem2 <- vdem2 %>%
  mutate(autocrat_dummy_libdem = ifelse(delta_libdem < -0.01, 1, 0)) # Cutoff point at -0.01

View(vdem2)

## Changing class to factor variables  CANNOT CHANGE TO FACTOR BEFORE AUTOCRAT_TREND
#vdem2$autocrat_dummy_libdem <- as.factor(vdem2$autocrat_dummy_libdem)
summary(vdem2$autocrat_dummy_libdem)

## Frequency tables, absolute and relative distribution
table_libdem <- table(vdem2$autocrat_dummy_libdem, useNA = "ifany")
table_libdem
prop.table(table_libdem)*100

#### Creating data frame for number of autocratization episodes ####

## Creating a new data frame measuring how many countries each year have experienced autocratization, 
## when basing this on all negative change in democracy level below -0.01
autocrat_trend <- vdem2 %>%
  group_by(year) %>%
  summarise(count_autocrat = sum(autocrat_dummy_libdem, na.rm = TRUE))  

View(autocrat_trend)

## Scatterplot for autocratic_trend, showing autocratization episodes over time 
ggplot(autocrat_trend, aes(year, count_autocrat)) +
  geom_smooth(col = "lightcoral", method = "loess") + 
  geom_point() +
  theme_classic() + 
  labs(x = "Year", 
       y = "Number of autocratic downturns", 
       title = "Scatterplot for annual number of autocratic downturns over time", 
       subtitle = "Liberal Democracy Index (LDI)")
# The number of countries that during a year experienced autocratization, 
# measured as decline in level of democracy (negative change) below -0.01,
# has increased steadily over time, with an upsurge since the start of the 1990s.

#################################
#### Liberal Component Index ####
#################################

#### Descriptive statistics first ####

## Brief summary 
summary(vdem2$v2x_liberal)

## Histogram over liberal component index to show distribution 
ggplot(vdem2, aes(v2x_liberal)) +
  geom_histogram(fill = "grey60", col = "white") +
  theme_classic() + 
  labs(x = "Liberal component index score", 
       y = "Numer of observations", 
       title = "Histogram over Liberal Component Index (LCI)", 
       caption = "Source: Coppedge et al., (2020)")

#### Creating a new variable measuring change in level of liberal component index ####

## Variable measuring change in level of liberal component for each country from year to year, 
## making it possible to measure autocratization in this component
vdem2 <- vdem2 %>%
  group_by(country_id) %>%
  mutate(delta_liberal = v2x_liberal - dplyr::lag(v2x_liberal))

## Brief summary
summary(vdem2$delta_liberal)

#### Graphical illustrations ####

## Scatterplot across time
ggplot(vdem2, aes(year, delta_liberal)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  labs(x = "Year", 
       y = "Change in LCI", 
       title = "Scatterplot for yearly change in LCI over time")
# Outliers before and after WW1, WW2 and around the dissolution of the USSR

ggsave("scatterplot_delta_liberal_time.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

#### Creating dummy variables ####

## Dummy measuring whether there was negative or non-negative change 
vdem2 <- vdem2 %>%
  mutate(autocrat_dummy_liberal = ifelse(delta_liberal < -0.01, 1, 0)) # Cutoff point at -0.01

View(vdem2)

## Frequency tables, absolute and relative distribution
table_liberal <- table(vdem2$autocrat_dummy_liberal, useNA = "ifany")
table_liberal
prop.table(table_liberal)*100

###################################
#### Electoral Democracy Index ####
###################################

#### Descriptive statistics first ####

## Brief summary
summary(vdem2$v2x_polyarchy)

## Histogram over electoral democracy index to show distribution 
ggplot(vdem2, aes(v2x_polyarchy)) +
  geom_histogram(fill = "grey60", col = "white") +
  theme_classic() + 
  labs(x = "Electoral democracy index score", 
       y = "Amount of observations", 
       title = "Histogram over Electoral Democracy Index (EDI)", 
       caption = "Source: Coppedge et al., (2020)")

#### Creating a variable measuring change in level of electoral democracy ####

## Variable measuring change in level of electoral democracy for each country from year to year, 
## making it possible to measure autocratization in this component
vdem2 <- vdem2 %>%
  group_by(country_id) %>%
  mutate(delta_polyarchy = v2x_polyarchy - dplyr::lag(v2x_polyarchy))

## Brief summary
summary(vdem2$delta_polyarchy)

#### Graphical illustrations ####

## Scatterplot across time
ggplot(vdem2, aes(year, delta_polyarchy)) +
  geom_point(alpha = 0.3) +
  theme_classic() + 
  labs(x = "Year", 
       y = "Change in EDI", 
       title = "Scatterplot for yearly change in EDI over time")
# Outliers before and after WW1, WW2 and around the dissolution of the USSR

ggsave("scatterplot_delta_polyarchy_time.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

#### Creating dummy variables ####

## Dummy measuring whether there was negative or non-negative change
vdem2 <- vdem2 %>%
  mutate(autocrat_dummy_polyarchy = ifelse(delta_polyarchy < -0.01, 1, 0)) # Cutoff point at -0.01

View(vdem2)

## Frequency tables, absolute and relative distribution
table_polyarchy <- table(vdem2$autocrat_dummy_polyarchy, useNA = "ifany")
table_polyarchy
prop.table(table_polyarchy)*100

