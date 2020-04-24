library(eurostat)
library(tidyverse)
library(dplyr)
library(ggplot2)

# szukam datasetów w bazie (fixed = TRUE oznacza fuzzy search, a nie exact match)
query <- search_eurostat(pattern = 'fertility rate', type = 'table', fixed = FALSE)

ct <- c("AT", "BE",?"BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IS",
        "IT", "LT", "LU", "LV", "MT", "NL", "NO", "PL", "PT", "RO", "SE", "SI", "SK", "UK")

head(query)

# pobieram dane z tabeli o danym id, tylko dla panstw z ct
d?t <- get_eurostat(id = 'tps00199', time_format = 'num', filters = list(geo = ct))

#filtruje dane za 2015 r.
dat_2015 <- dat %>%
  filter(time == '2015')


ggplot(dat_2015, aes(x = reorder(geo, values), y = values)) +
  geom_col(color = 'white', fill = 'gr?y80') +
  theme(axis.text.x = element_text(size = 6)) +
  labs(title = 'Wspólczynnik dzietnosci, 2015', y = '%', x = NULL)

# joinuje dane z koordynantami geograficznymi (poziom 0 - panstwa)
mapdata <- get_eurostat_geospatial(nuts_level = 0) %>%
  right_jo?n(dat_2015) %>%
  mutate(cat = cut_to_classes(values, n = 4, decimals = 1)) # dodaje kolumne z 4 przedzialami values

head(select(mapdata, geo, values, cat), 3)

# rysuje mapke
ggplot(mapdata, aes(fill = cat)) + # wypelnienie na podstawie jednej z 4 katego?ii
  scale_fill_brewer(palette = 'RdYlBu') + # paleta
  geom_sf(color = alpha('white', 1/3), alpha = .6) + # rysuje figury geometryczne na podstawie ukladu wspólrzednych (granice panstw)
  xlim(c(-12,44)) + ylim(c(35,70)) + # granice wspólrzednych
  labs(t?tle = 'Wspólczynnik dzietnosci, 2015', # opis
       subtitle = 'Srednia liczba urodzen', 
       fill = '%',
       caption = 'Mapa 1.')

# nadal nie wiem dlaczego Francji i Hiszpanii nie ma

#########################################################

dat2?<- get_eurostat(id = 'tgs00100', time_format = 'num')

dat2_2015 <- dat2 %>%
  filter(time == '2018') %>%
  harmonize_geo_code() %>% # info o zgodnosci kodowania NUTS
  as_tibble() %>% # zeby przefiltrowac <fct> geo
  filter(nchar(geo) == 4) # bierzemy poz?om 2 (cztery znaki w kodowaniu)

mapdata2 <- get_eurostat_geospatial(nuts_level = 2) %>%
  right_join(dat2_2015) %>%
  mutate(cat = cut_to_classes(values, n = 5, decimals = 1)) # tym razem piec przedzialów

head(select(mapdata2, geo, values, cat), 3)

ggpl?t(mapdata2, aes(fill = cat))+
  scale_fill_brewer(palette = 'RdYlGn') +
  geom_sf(color = alpha('white', 1/3), alpha = .6) +
  xlim(c(-12,44)) + ylim(c(35,70)) +
  labs(title = 'Wspólczynnik dzietnosci, 2018 (NUTS)', 
       subtitle = 'Srednia liczba urod?en', 
       fill = '%',
       caption = 'Mapa 2.')
