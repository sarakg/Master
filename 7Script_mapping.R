#####################
####             ####
####   Mapping   ####
####             ####
#####################

world_map <- map_data("world")

ggplot(world_map, aes(long, lat, group = group)) + 
  geom_polygon(fill = "grey80", col = "black") + 
  theme_void()

## Comparing country names between world map and vdem
setdiff(world_map$region, vdem3$country_name)
setdiff(vdem3$country_name, world_map$region)

#### Autocratization Map 2019 ####

## LDI
aut_map <- vdem3 %>%
  dplyr::filter(year == 2019) %>%
  dplyr::select(country_name, autocrat_dummy_libdem) %>%
  dplyr::rename(region = country_name, value = autocrat_dummy_libdem) %>%
  dplyr::mutate(region = ifelse(region == "United States of America", "USA", 
                                ifelse(region == "Burma/Myanmar", "Myanmar", 
                                       ifelse(region == "United Kingdom", "UK",
                                              ifelse(region == "Republic of the Congo", "Republic of Congo", 
                                                     ifelse(region == "The Gambia", "Gambia", 
                                                            ifelse(region == "Eswatini", "Swaziland", 
                                                                   ifelse(region == "Trinidad og Tobago", "Trinidad", 
                                                                          ifelse(region == "North Macedonia", "Macedonia", region))))))))) 

aut_map_map <- left_join(aut_map, world_map, by = "region")

ggplot(aut_map_map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = as.factor(value)), color = "white") + 
  theme_void() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) + 
  scale_fill_discrete(name = "", labels = c("No autocratization", "Autocratization"))

ggsave("autocratization_map_LDI.png", 
       plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"),
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## LCI
LCI_map_dich <- vdem3 %>%
  filter(year == 2019) %>%
  select(country_name, autocrat_dummy_liberal) %>%
  rename(region = country_name, value = autocrat_dummy_liberal) %>%
  mutate(region = ifelse(region == "United States of America", "USA", 
                         ifelse(region == "Burma/Myanmar", "Myanmar", 
                                ifelse(region == "United Kingdom", "UK",
                                       ifelse(region == "Republic of the Congo", "Republic of Congo", 
                                              ifelse(region == "The Gambia", "Gambia", 
                                                     ifelse(region == "Eswatini", "Swaziland", 
                                                            ifelse(region == "Trinidad og Tobago", "Trinidad", 
                                                                   ifelse(region == "North Macedonia", "Macedonia", region)))))))))

LCI_map_dich_map <- left_join(LCI_map_dich, world_map, by = "region")

ggplot(LCI_map_dich_map, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = as.factor(value)), color = "white") + 
  theme_void() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) + 
  scale_fill_discrete(name = "", labels = c("No autocratization", "Autocratization")) +
  labs(title = "Autocratization LCI 2019")

ggsave("autocratization_map_LCI.png", 
       plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"),
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## EDI
EDI_map_dich <- vdem3 %>%
  filter(year == 2019) %>%
  select(country_name, autocrat_dummy_polyarchy) %>%
  rename(region = country_name, value = autocrat_dummy_polyarchy) %>%
  mutate(region = ifelse(region == "United States of America", "USA", 
                         ifelse(region == "Burma/Myanmar", "Myanmar", 
                                ifelse(region == "United Kingdom", "UK",
                                       ifelse(region == "Republic of the Congo", "Republic of Congo", 
                                              ifelse(region == "The Gambia", "Gambia", 
                                                     ifelse(region == "Eswatini", "Swaziland", 
                                                            ifelse(region == "Trinidad og Tobago", "Trinidad", 
                                                                   ifelse(region == "North Macedonia", "Macedonia", region)))))))))

EDI_map_dich_map <- left_join(EDI_map_dich, world_map, by = "region")

ggplot(EDI_map_dich_map, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = as.factor(value)), color = "white") + 
  theme_void() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse = TRUE)) + 
  scale_fill_discrete(name = "", labels = c("No autocratization", "Autocratization")) +
  labs(title = "Autocratization EDI 2019")

ggsave("autocratization_map_EDI.png", 
       plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"),
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")


#### LDI, LCI and EDI scores in 2019 ####

## Map showing each country's LDI score
LDI_map <- vdem3 %>%
  dplyr::filter(year == 2019) %>%
  dplyr::select(country_name, v2x_libdem) %>%
  dplyr::rename(region = country_name, value = v2x_libdem) %>%
  dplyr::mutate(region = ifelse(region == "United States of America", "USA", 
                          ifelse(region == "Burma/Myanmar", "Myanmar", 
                           ifelse(region == "United Kingdom", "UK",
                            ifelse(region == "Republic of the Congo", "Republic of Congo", 
                             ifelse(region == "The Gambia", "Gambia", 
                              ifelse(region == "Eswatini", "Swaziland", 
                               ifelse(region == "Trinidad og Tobago", "Trinidad", 
                                ifelse(region == "North Macedonia", "Macedonia", region))))))))) 

LDI_map_map <- left_join(LDI_map, world_map, by = "region")

ggplot(LDI_map_map, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = value), color = "white") + 
  theme_void() +
  labs(fill = "LDI", 
       title = "LDI score in 2019")

ggsave("world_map_LDI_score.png", plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Map showing each country's LCI score
LCI_map <- vdem3 %>%
  dplyr::filter(year == 2019) %>%
  dplyr::select(country_name, v2x_liberal) %>%
  dplyr::rename(region = country_name, value = v2x_liberal) %>%
  dplyr::mutate(region = ifelse(region == "United States of America", "USA", 
                          ifelse(region == "Burma/Myanmar", "Myanmar", 
                           ifelse(region == "United Kingdom", "UK", 
                            ifelse(region == "Republic of the Congo", "Republic of Congo", 
                             ifelse(region == "The Gambia", "Gambia", 
                              ifelse(region == "Eswatini", "Swaziland", 
                               ifelse(region == "Trinidad og Tobago", "Trinidad", 
                                ifelse(region == "North Macedonia", "Macedonia", region))))))))) 

LCI_map_map <- left_join(LCI_map, world_map, by = "region")

ggplot(LCI_map_map, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = value), color = "white") + 
  theme_void() +
  labs(fill = "LCI", 
       title =  "LCI score in 2019")

ggsave("world_map_LCI_score.png", plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

## Map showing each country's EDI score
EDI_map <- vdem3 %>%
  dplyr::filter(year == 2019) %>%
  dplyr::select(country_name, v2x_polyarchy) %>%
  dplyr::rename(region = country_name, value = v2x_polyarchy) %>%
  dplyr::mutate(region = ifelse(region == "United States of America", "USA", 
                          ifelse(region == "Burma/Myanmar", "Myanmar", 
                           ifelse(region == "United Kingdom", "UK", 
                            ifelse(region == "Republic of the Congo", "Republic of Congo", 
                             ifelse(region == "The Gambia", "Gambia", 
                              ifelse(region == "Eswatini", "Swaziland", 
                               ifelse(region == "Trinidad og Tobago", "Trinidad", 
                                ifelse(region == "North Macedonia", "Macedonia", region))))))))) 

EDI_map_map <- left_join(EDI_map, world_map, by = "region")

ggplot(EDI_map_map, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = value), color = "white") + 
  theme_void() +
  labs(fill = "EDI", 
       title = "EDI score in 2019")

ggsave("world_map_EDI_score.png", plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")

#### Map Regimes of the World 2019 ####

RoW_map <- vdem3 %>%
  filter(year == 2019) %>%
  select(country_name, v2x_regime) %>%
  rename(region = country_name, value = v2x_regime) %>%
  mutate(region = ifelse(region == "United States of America", "USA", 
                         ifelse(region == "Burma/Myanmar", "Myanmar",
                                ifelse(region == "United Kingdom", "UK", 
                                       ifelse(region == "Republic of the Congo", "Republic of Congo", 
                                              ifelse(region == "The Gambia", "Gambia", 
                                                     ifelse(region == "Eswatini", "Swaziland", 
                                                            ifelse(region == "Trinidad og Tobago", "Trinidad", 
                                                                   ifelse(region == "North Macedonia", "Macedonia", region))))))))) 

RoW_map_map <- left_join(RoW_map, world_map, by = "region")

ggplot(RoW_map_map, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = as.factor(value)), color = "white") + 
  theme_void() +
  theme(legend.position = "bottom") +
  scale_fill_discrete(name = "", labels = c("Closed Autocracy", 
                                            "Electoral Autocracy", 
                                            "Electoral Democracy",
                                            "Liberal Democracy")) +
  labs(title = "Regimes of the World 2019")

ggsave("world_map_RoW.png", 
       plot = last_plot(), 
       width = 300, 
       height = 150, 
       units = c("mm"), 
       path = "C:/Users/sk_gr/OneDrive - Universitetet i Oslo/5. år statsvitenskap/STV4992/R/Masteroppgave_R/Plots")


