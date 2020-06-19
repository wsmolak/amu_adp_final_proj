#install.packages('eurostat')
#install.packages('tidyverse')

library(eurostat)
library(tidyverse)
library(dplyr)
library(ggplot2)

# szukam datasetów w bazie (fixed = TRUE oznacza fuzzy search, a nie exact match)
query <- search_eurostat(pattern = 'road accidents', type = 'table', fixed = FALSE)

# listuje sobie europe
ct <- c("AT", "BE","BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IS",
        "IT", "LT", "LU", "LV", "MT", "NL", "NO", "PL", "PT", "RO", "SE", "SI", "SK", "UK")

head(query)

# pobieram dane z tabeli o danym id, tylko dla panstw z ct
dat <- get_eurostat(id = 'sdg_11_40', time_format = 'num', filters = list(geo = ct))

#filtruje dane za 2015 r.
  # ratio / 100 000
dat_2015_nr <- dat %>%
  filter(time == '2015') %>%
  filter(unit == 'NR')
  # number
dat_2015_rt <- dat %>%
  filter(time == '2015') %>%
  filter(unit == 'RT')


ggplot(dat_2015_nr, aes(x = reorder(geo, values), y = values)) +
  geom_col(color = 'white', fill = 'grey80') +
  theme(axis.text.x = element_text(size = 6)) +
  labs(title = 'Wypadki drogowe, 2015', y = '%', x = NULL)

ggplot(dat_2015_rt, aes(x = reorder(geo, values), y = values)) +
  geom_col(color = 'white', fill = 'grey80') +
  theme(axis.text.x = element_text(size = 6)) +
  labs(title = 'Wypadki drogowe na 100 000 mieszk., 2015', y = '%', x = NULL)

# joinuje dane z koordynantami geograficznymi (poziom 0 - panstwa)
mapdata_nr_2015 <- get_eurostat_geospatial(nuts_level = 0) %>%
  right_join(dat_2015_nr) %>%
  mutate(cat = cut_to_classes(values, n = 5, decimals = 1)) # dodaje kolumne z 5 przedzialami values

mapdata_rt_2015 <- get_eurostat_geospatial(nuts_level = 0) %>%
  right_join(dat_2015_rt) %>%
  mutate(cat = cut_to_classes(values, n = 5, decimals = 1))

head(select(mapdata_nr_2015, geo, values, cat), 3)
head(select(mapdata_rt_2015, geo, values, cat), 3)

# rysuje mapki
ggplot(mapdata_nr_2015, aes(fill = cat)) + # wypelnienie na podstawie jednej z 5 katego?ii
  scale_fill_brewer(palette = 'RdYlBu') + # paleta
  geom_sf(color = alpha('white', 1/3), alpha = .6) + # rysuje figury geometryczne na podstawie ukladu wspólrzednych (granice panstw)
  xlim(c(-12,44)) + ylim(c(35,70)) + # granice wspólrzednych
  labs(title = 'Wypadki drogowe, 2015', # opis
       subtitle = 'Srednia liczba wypadkow', 
       fill = '%',
       caption = 'Mapa 1.1')

ggplot(mapdata_rt_2015, aes(fill = cat)) + # wypelnienie na podstawie jednej z 4 katego?ii
  scale_fill_brewer(palette = 'RdYlBu') + # paleta
  geom_sf(color = alpha('white', 1/3), alpha = .6) + # rysuje figury geometryczne na podstawie ukladu wspólrzednych (granice panstw)
  xlim(c(-12,44)) + ylim(c(35,70)) + # granice wspólrzednych
  labs(title = 'Wypadki drogowe, 2015', # opis
       subtitle = 'Srednia liczba wypadkow na 100 000 mieszkanców', 
       fill = '%',
       caption = 'Mapa 1.2')

# a u mnie nie pokazuje Bialorusi i Ukrainy

#########################################################

#filtruje dane za 2018 r.
# number
dat_2018_nr <- dat %>%
  filter(time == '2018') %>%
  filter(unit == 'NR')
# ratio / 100 000
dat_2018_rt <- dat %>%
  filter(time == '2018') %>%
  filter(unit == 'RT')


ggplot(dat_2018_nr, aes(x = reorder(geo, values), y = values)) +
  geom_col(color = 'white', fill = 'grey80') +
  theme(axis.text.x = element_text(size = 6)) +
  labs(title = 'Wypadki drogowe, 2018', y = '%', x = NULL)

ggplot(dat_2018_rt, aes(x = reorder(geo, values), y = values)) +
  geom_col(color = 'white', fill = 'grey80') +
  theme(axis.text.x = element_text(size = 6)) +
  labs(title = 'Wypadki drogowe na 100 000 mieszk., 2018', y = '%', x = NULL)

# joinuje dane z koordynantami geograficznymi (poziom 0 - panstwa)
mapdata_nr_2018 <- get_eurostat_geospatial(nuts_level = 0) %>%
  right_join(dat_2018_nr) %>%
  mutate(cat = cut_to_classes(values, n = 5, decimals = 1)) # dodaje kolumne z 5 przedzialami values

mapdata_rt_2018 <- get_eurostat_geospatial(nuts_level = 0) %>%
  right_join(dat_2018_rt) %>%
  mutate(cat = cut_to_classes(values, n = 5, decimals = 1))

head(select(mapdata_nr_2018, geo, values, cat), 3)
head(select(mapdata_rt_2018, geo, values, cat), 3)

# rysuje mapki
ggplot(mapdata_nr_2018, aes(fill = cat)) + # wypelnienie na podstawie jednej z 5 katego?ii
  scale_fill_brewer(palette = 'RdYlBu') + # paleta
  geom_sf(color = alpha('white', 1/3), alpha = .6) + # rysuje figury geometryczne na podstawie ukladu wspólrzednych (granice panstw)
  xlim(c(-12,44)) + ylim(c(35,70)) + # granice wspólrzednych
  labs(title = 'Wypadki drogowe, 2018', # opis
       subtitle = 'Srednia liczba wypadkow', 
       fill = '%',
       caption = 'Mapa 2.1')

ggplot(mapdata_rt_2018, aes(fill = cat)) + # wypelnienie na podstawie jednej z 5 katego?ii
  scale_fill_brewer(palette = 'RdYlBu') + # paleta
  geom_sf(color = alpha('white', 1/3), alpha = .6) + # rysuje figury geometryczne na podstawie ukladu wspólrzednych (granice panstw)
  xlim(c(-12,44)) + ylim(c(35,70)) + # granice wspólrzednych
  labs(title = 'Wypadki drogowe, 2018', # opis
       subtitle = 'Srednia liczba wypadkow na 100 000 mieszkanców', 
       fill = '%',
       caption = 'Mapa 2.2')


#########################################################
#########################################################
#########################################################


query <- search_eurostat(pattern = 'national', type = 'table', fixed = FALSE)

head(query)

# pobieram dane z tabeli o danym id, tylko dla panstw z ct
dat2 <- get_eurostat(id = 'tec00133', time_format = 'num', filters = list(geo = ct))

dat_2015 <- dat2 %>%
  filter(time == '2015')

ggplot(dat_2015, aes(x = reorder(geo, values), y = values)) +
  geom_col(color = 'white', fill = 'grey80') +
  theme(axis.text.x = element_text(size = 6)) +
  labs(title = 'dochód narodowy netto', y = '%', x = NULL)

mapdata_2015 <- get_eurostat_geospatial(nuts_level = 0) %>%
  right_join(dat_2015) %>%
  mutate(cat = cut_to_classes(values, n = 5, decimals = 1)) # dodaje kolumne z 5 przedzialami values

ggplot(mapdata_2015, aes(fill = cat)) + # wypelnienie na podstawie jednej z 5 katego?ii
  scale_fill_brewer(palette = 'RdYlBu') + # paleta
  geom_sf(color = alpha('white', 1/3)) + # rysuje figury geometryczne na podstawie ukladu wspólrzednych (granice panstw)
  xlim(c(-12,44)) + ylim(c(35,70)) + # granice wspólrzednych
  labs(title = 'Wypadki drogowe, 2018', # opis
       subtitle = 'Srednia liczba wypadkow', 
       fill = '%',
       caption = 'Mapa 2.1')


query <- search_eurostat(pattern = 'motorways', type = 'table', fixed = FALSE)

head(query)
