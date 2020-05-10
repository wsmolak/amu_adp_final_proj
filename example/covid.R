library(tidyverse)
library(dplyr)
library(ggplot2)
library(countrycode)
library(eurostat)
library(ggpubr)
library(ggalt)


# https://ourworldindata.org/5059252d-dd2e-4c48-9cf2-7b8154015c39
dat_covid <- read_csv('coronavirus-cfr.csv')

eu_countries <- c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IS",
        "IT", "LT", "LU", "LV", "MT", "NL", "NO", "PL", "PT", "RO", "SE", "SI", "SK", "UK")

latest_eu_data <- dat_covid %>%
  filter(Date == 'May 3, 2020') %>%
  filter(Code != 'OWID_WRL' & Code != 'OWID_KOS') %>%
  drop_na() %>%
  mutate(geo = countrycode(Code, 'iso3c', 'eurostat')) %>%
  filter(geo %in% eu_countries) 

colnames(latest_eu_data)[4] <- 'mortality'
  
mapdata <- get_eurostat_geospatial(nuts_level = 0) %>%
  right_join(latest_eu_data) %>%
  mutate(cat = cut_to_classes(mortality, n = 4, decimals = 1))

ggplot(mapdata, aes(fill = mortality))+
  geom_sf(color = alpha('white', 1/3)) +
  xlim(c(-12,44)) + ylim(c(35,70)) +
  labs(title = 'Œmiertelnoœæ COVID-19 w UE', 
       subtitle = 'Stan na 03.05.2020', 
       fill = '%',
       caption = 'Mapa 1')

ggplot(mapdata, aes(fill = cat))+
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  scale_fill_brewer(palette = 'YlOrRd') +
  xlim(c(-16,37)) + ylim(c(35,70)) + 
  labs(title = 'Œmiertelnoœæ COVID-19 w UE', 
       subtitle = 'Stan na 03.05.2020', 
       fill = '%',
       caption = 'Mapa 2')

beds_2017 <- get_eurostat(id = 'tps00168', time_format = 'num', filters = list(time = '2017'))

cor_data <- latest_eu_data %>%
  left_join(beds_2017) %>%
  select(geo, mortality, values) %>%
  rename(beds = values) %>%
  mutate(country = label_eurostat(geo, dic = "geo", custom_dic = c(DE = "Germany")))


excluded = c('BE', 'IE', 'MT', 'LV', 'CY', 'NO', '?H', 'EE', 'HR', 'PT', 'IS', 'LU')

ggplot(filter(cor_data, !geo %in% c('BE', 'IE', 'MT', 'LV', 'CY', 'NO', '?H', 'EE', 'HR', 'PT', 'IS', 'LU')), 
       aes(x = beds, y = mortality)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  geom_text(aes(label = country), hjust = -.3, vjust = -.3) +
  stat_cor(method = "pearson", label.x = 400)

circle.df <- cor_data %>%
  filter(geo %in% c('FR', 'NL', 'IT', 'UK', 'SE', 'ES'))

ggplot(filter(cor_data, !geo %in% excluded), aes(x = beds, y = mortality)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  geom_text(aes(label = country), hjust = -.3, vjust = -.3) +
  stat_cor(method = "pearson", label.x = 400) +
  geom_encircle(data = circle.df, linetype = 2) +
  labs(x = 'Liczba ³ó¿ek na 100 tys.', y = 'Procent zmar³ych',
       title = 'Mijesca na OIOM vs œmiertelnoœæ Covid-19', 
       subtitle = 'stan na 03.05.2020')

######################################################################################################################


dat_covid_total <- read.csv('total-confirmed-cases-of-covid-19-per-million-people.csv')
colnames(dat_covid_total)[4] <- 'cases_per_mln'

latest_eu_data2 <- dat_covid_total %>%
  filter(Date == 'May 3, 2020') %>%
  filter(Code != 'OWID_WRL' & Code != 'OWID_KOS') %>%
  drop_na() %>%
  mutate(geo = countrycode(Code, 'iso3c', 'eurostat')) %>%
  filter(geo %in% eu_countries) %>%
  mutate(is_pl = ifelse(geo == 'PL', T, F))

mapdata2 <- get_eurostat_geospatial(nuts_level = 0) %>%
  right_join(latest_eu_data2) %>%
  mutate(cat = cut_to_classes(cases_per_mln, n = 4, decimals = 1))


ggplot(mapdata2, aes(fill = cases_per_mln))+
  geom_sf(color = alpha('white', 1/3)) +
  xlim(c(-16,37)) + ylim(c(35,70)) +
  labs(title = 'COVID-19 w UE', 
       subtitle = 'Stan na 03.05.2020', 
       fill = 'Liczba przypadków na 1 mln.',
       caption = 'Mapa 1')

ggplot(mapdata2, aes(fill = cat))+
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  scale_fill_brewer(palette = 'YlOrRd') +
  xlim(c(-16,37)) + ylim(c(35,70)) + 
  labs(title = 'COVID-19 w UE', 
       subtitle = 'Stan na 03.05.2020', 
       fill = 'Liczba przypadków na 1 mln.',
       caption = 'Mapa 2')

library(gghighlight)

ggplot(latest_eu_data2, aes(x = reorder(geo, cases_per_mln), y = cases_per_mln)) +
  geom_col(color = 'white', fill = 'grey60') +
  theme(axis.text.x = element_text(size = 6)) +
  gghighlight(geo == 'PL') +
  labs(title = 'Przypadki COVID-19 w UE', y = 'Liczba zaka¿eñ na 1 mln.', x = NULL)

