############################################
####                                    ####
####    Creating dependent variables    ####
####                                    ####
############################################

#################################
#### Liberal Democracy Index ####
#################################

#### Descriptive statistics ####

## Brief summary 
summary(vdem2$v2x_libdem)

## Reversing v2x_libdem
vdem2$v2x_libdem_rev <- vdem2$v2x_libdem*-1+max(vdem2$v2x_libdem, na.rm = TRUE)
summary(vdem2$v2x_libdem_rev)

## Histogram over liberal democracy index to show distribution 
ggplot(vdem2, aes(v2x_libdem_rev)) +
  geom_histogram(fill = "grey60", col = "white") +
  theme_classic() + 
  labs(x = "Liberal democracy index score", 
       y = "Number of observations", 
       title = "Histogram over Liberal Democracy Index (LDI)")

ggsave("histogram_LDI.png",
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

#### Creating dependent variable measuring change in level of democracy #### 

## Variable measuring change in LDI for each country from year to year
vdem2 <- vdem2 %>%
  group_by(country_id) %>%
  mutate(delta_libdem = v2x_libdem - dplyr::lag(v2x_libdem),
         delta_libdem_rev = v2x_libdem_rev - dplyr::lag(v2x_libdem_rev))

## Brief summary
summary(vdem2$delta_libdem)
summary(vdem2$delta_libdem_rev)

View(vdem2)

#### Graphical illustrations ####

## Histogram 
ggplot(vdem2, aes(delta_libdem_rev)) +
  geom_histogram(fill = "grey60", col = "white") +
  theme_classic() + 
  labs(x = "Change", 
       y = "Number of observations", 
       title = "Histogram over yearly change in LDI")

ggsave("histogram_delta_libdem.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Scatterplot across time
ggplot(vdem2, aes(year, delta_libdem_rev)) +
  geom_point(alpha = 0.3, size = 1) +
  theme_classic() + 
  labs(x = "Year",
       y = "Change in LDI",
       title = "Scatterplot for yearly change in LDI over time")
# Outliers before and after WW1, WW2 and around the dissolution of the USSR

ggsave("scatterplot_delta_libdem_time.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots") 

#### Creating dummy variables ####

## Dummy measuring whether there was a change in democracy level below -0.01 or not
vdem2 <- vdem2 %>%
  mutate(autocrat_dummy_libdem = ifelse(delta_libdem < -0.01, 1, 0), 
         autocrat_dummy_libdem_rev = ifelse(delta_libdem < -0.01, 1, 0)) 

View(vdem2)

## Frequency tables, absolute and relative distribution
table_libdem <- table(vdem2$autocrat_dummy_libdem, useNA = "ifany")
table_libdem
prop.table(table_libdem)*100

table_libdem_rev <- table(vdem2$autocrat_dummy_libdem_rev, useNA = "ifany")
table_libdem_rev
prop.table(table_libdem_rev)*100

#### Creating data frame for number of autocratization episodes ####

## Creating a new data frame measuring how many countries each year have 
## experienced autocratization, when basing this on all negative change in 
## democracy level below -0.01
autocrat_trend <- vdem2 %>%
  group_by(year) %>%
  summarise(count_autocrat = sum(autocrat_dummy_libdem_rev, na.rm = TRUE))  

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

ggsave("autocrat_trend.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

#################################
#### Liberal Component Index ####
#################################

#### Descriptive statistics first ####

## Brief summary 
summary(vdem2$v2x_liberal)

## Reversing v2x_liberal
vdem2$v2x_liberal_rev <- vdem2$v2x_liberal*-1+max(vdem2$v2x_liberal, na.rm = TRUE)
summary(vdem2$v2x_liberal_rev)

## Histogram over liberal component index to show distribution 
ggplot(vdem2, aes(v2x_liberal_rev)) +
  geom_histogram(fill = "grey60", col = "white") +
  theme_classic() + 
  labs(x = "Liberal component index score", 
       y = "Numer of observations", 
       title = "Histogram over Liberal Component Index (LCI)")

ggsave("histogram_LCI.png",
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

#### Creating dependent variable measuring change in level of democracy #### 

## Variable measuring change in LCI for each country from year to year
vdem2 <- vdem2 %>%
  group_by(country_id) %>%
  mutate(delta_liberal = v2x_liberal - dplyr::lag(v2x_liberal),
         delta_liberal_rev = v2x_liberal_rev - dplyr::lag(v2x_liberal_rev))

## Brief summary
summary(vdem2$delta_liberal)
summary(vdem2$delta_liberal_rev)

#### Graphical illustrations ####

## Scatterplot across time
ggplot(vdem2, aes(year, delta_liberal_rev)) +
  geom_point(alpha = 0.3, size = 1) +
  theme_classic() + 
  labs(x = "Year", 
       y = "Change in LCI", 
       title = "Scatterplot for yearly change in LCI over time")
# Outliers before and after WW1, WW2 and around the dissolution of the USSR

ggsave("scatterplot_delta_liberal_time.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

#### Creating dummy variable ####

## Dummy measuring whether there was a change in democracy level below -0.01 or not
vdem2 <- vdem2 %>%
  mutate(autocrat_dummy_liberal = ifelse(delta_liberal < -0.01, 1, 0), 
         autocrat_dummy_liberal_rev = ifelse(delta_liberal < -0.01, 1, 0))

View(vdem2)

## Frequency tables, absolute and relative distribution
table_liberal <- table(vdem2$autocrat_dummy_liberal, useNA = "ifany")
table_liberal
prop.table(table_liberal)*100

table_liberal_rev <- table(vdem2$autocrat_dummy_liberal_rev, useNA = "ifany")
table_liberal_rev
prop.table(table_liberal_rev)*100

###################################
#### Electoral Democracy Index ####
###################################

#### Descriptive statistics first ####

## Brief summary
summary(vdem2$v2x_polyarchy)

## Reversing v2x_polyarchy
vdem2$v2x_polyarchy_rev <- vdem2$v2x_polyarchy*-1+max(vdem2$v2x_polyarchy, na.rm = T)
summary(vdem2$v2x_polyarchy_rev)

## Histogram over electoral democracy index to show distribution 
ggplot(vdem2, aes(v2x_polyarchy_rev)) +
  geom_histogram(fill = "grey60", col = "white") +
  theme_classic() + 
  labs(x = "Electoral democracy index score", 
       y = "Number of observations", 
       title = "Histogram over Electoral Democracy Index (EDI)")

ggsave("histogram_EDI.png",
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

#### Creating dependent variable measuring change in level of democracy #### 

## Variable measuring change in EDI for each country from year to year
vdem2 <- vdem2 %>%
  group_by(country_id) %>%
  mutate(delta_polyarchy = v2x_polyarchy - dplyr::lag(v2x_polyarchy), 
         delta_polyarchy_rev = v2x_polyarchy_rev - dplyr::lag(v2x_polyarchy_rev))

## Brief summary
summary(vdem2$delta_polyarchy)
summary(vdem2$delta_polyarchy_rev)

#### Graphical illustrations ####

## Scatterplot across time
ggplot(vdem2, aes(year, delta_polyarchy_rev)) +
  geom_point(alpha = 0.3, size = 1) +
  theme_classic() + 
  labs(x = "Year", 
       y = "Change in EDI", 
       title = "Scatterplot for yearly change in EDI over time")
# Outliers before and after WW1, WW2 and around the dissolution of the USSR

ggsave("scatterplot_delta_polyarchy_time.png", 
       plot = last_plot(), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

#### Creating dummy variable ####

## Dummy measuring whether there was a change in democracy level below -0.01 or not
vdem2 <- vdem2 %>%
  mutate(autocrat_dummy_polyarchy = ifelse(delta_polyarchy < -0.01, 1, 0), 
         autocrat_dummy_polyarchy_rev = ifelse(delta_polyarchy < -0.01, 1, 0))

View(vdem2)

## Frequency tables, absolute and relative distribution
table_polyarchy <- table(vdem2$autocrat_dummy_polyarchy, useNA = "ifany")
table_polyarchy
prop.table(table_polyarchy)*100

table_polyarchy_rev <- table(vdem2$autocrat_dummy_polyarchy_rev, useNA = "ifany")
table_polyarchy_rev
prop.table(table_polyarchy_rev)*100

