library(eurostat)
library(tidyverse)
library(dplyr)
library(ggplot2)

############################ ogólne dane dot. wypadków drogowych w Europie ############################################

# id: zabici w wypadkach drogowych
dat <- get_eurostat(id = 'sdg_11_40', time_format = 'num', filters = list(time = '2018')) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany")))

# całkowita liczba zabitych (wykluczenie Turcji)
dat_2018_nr <- dat %>%
  filter(unit == "NR" & !geo %in% c('EU28', 'EU27_2020', 'TR')) %>%
  select(geo, country, values)

# zabici na 100 tys. mieszkańców (gotowe wyliczenie)
dat_2018_rt <- dat %>%
  filter(unit == "RT" & !geo %in% c('EU28', 'EU27_2020', 'TR')) %>%
  select(geo, country, values)

ggplot(dat_2018_nr, aes(x = reorder(country, values), y = values, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar śmiertelnych wypadków drogowych, 2018', y = 'Ofiary', x = NULL) +
  coord_flip() 

ggplot(dat_2018_rt, aes(x = reorder(country, values), y = values, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar śmiertelnych wypadków drogowych, 2018', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip() 

# dane geograficzne dla zobrazowania współczynnika śmiertelności 
mapdata <- get_eurostat_geospatial(nuts_level = 0, resolution = 20, output_class = "sf") %>%
  right_join(dat_2018_rt) %>%
  mutate(cat = cut_to_classes(values, n = 4, decimals = 1))

# mapa (wypełnienie w skali dyskretnej - podział na 4 klasy)
ggplot(mapdata, aes(fill = cat)) + 
  scale_fill_brewer(palette = 'Reds') + 
  geom_sf(color = alpha('black', 1/3), alpha = .6) + 
  coord_sf(xlim = c(-20,44), ylim = c(30,70)) +
  labs(title = 'Ofiary śmiertelne wypadków drogowych, 2018', 
       subtitle = '(na 100 tys. mieszkańców)', 
       fill = 'Ofiary',
       caption = 'Mapa 1.')

# mapa (wypełnienie w skali ciągłej)
ggplot(mapdata, aes(fill = values)) +
  geom_sf(color = alpha('black', 1/3), alpha = .6) + 
  coord_sf(xlim = c(-20,44), ylim = c(30,70)) +
  labs(title = 'Ofiary śmiertelne wypadków drogowych, 2018', 
       subtitle = 'Średnia na 100 tys. mieszkańców', 
       fill = 'Wynik',
       caption = 'Mapa 2.')


####################################### dane nt. rozmiaru populacji do obliczeń ###############################

# dane wymagają przeliczenia w stosunku do populacji w celu uzsykania miarodajnego 
# porównania sytuacji w poszczególnych państwach
dat_population <- get_eurostat(id = 'tps00001', time_format = 'num', filters = list(time = '2017')) %>%
  rename(population = values)


####################################### ofiary wypadków wg pojazdu #############################################

# id: tran_sf_roadve
dat_vehicle <- get_eurostat(id = 'tran_sf_roadve', time_format = 'num', filters = list(time = '2017')) %>%
  left_join(dat_population) %>%
  mutate(rate = values / population * 100000) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany"))) %>%
  drop_na()

# zabici w wypadkach uzytkownicy samochodów osobowych
ggplot(filter(dat_vehicle, vehicle == 'CAR'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar smiertelnych wypadków z udziałem samochodów osobowych, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip() 

# zabici w wypadkach rowerzyści
ggplot(filter(dat_vehicle, vehicle == 'BIKE'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar smiertelnych wypadków z udziałem rowerzystów, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip() 

# brakuje danych obrazujÄ…cych ofiary wypadków z udziałem innych typów pojazdów, w tym pojazdów transportowych lub motocykli

####################################### ofiary wypadków wg uĹĽytkowników dróg #########################################################

# id: tran_sf_roadus
dat_users <- get_eurostat(id = 'tran_sf_roadus', time_format = 'num', filters = list(time = '2017')) %>%
  left_join(dat_population) %>%
  mutate(rate = values / population * 100000) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany"))) %>%
  drop_na()

# zabici w wypadkach piesi
ggplot(filter(dat_users, pers_inv == 'PED'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar śmiertelnych wśród pieszych uczestników ruchu, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach pasaĹĽerowie pojazdów 
ggplot(filter(dat_users, pers_inv == 'PAS'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar śmiertelnych wśród pasaĹĽerów pojazdów, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach kierujÄ…cy pojazdami 
ggplot(filter(dat_users, pers_inv == 'DRIV'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar śmiertelnych wśród kierujÄ…cych pojazdami, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

####################################### ofiary wypadków wg rodzaju infrastruktury drogowej ############################################

# id: tran_sf_roadro
dat_road <- get_eurostat(id = 'tran_sf_roadro', time_format = 'num', filters = list(time = '2017')) %>%
  left_join(dat_population) %>%
  mutate(rate = values / population * 100000) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany"))) %>%
  drop_na()

# zabici w wypadkach na autostratach
ggplot(filter(dat_road, tra_infr == 'MWAY'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar śmiertelnych wypadków na autorstradach, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach na drogach obszarów zabudowanych
ggplot(filter(dat_road, tra_infr == 'RD_URB'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar śmiertelnych wypadków drogowych w obszarze zabudowanym, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach na drogach wiejskich
ggplot(filter(dat_road, tra_infr == 'RD_RUR'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar śmiertelnych wypadków na drogach wiejskich, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

####################################### ofiary śmiertelne na 1 tys. km autostrady ######################################################

# długość sieci autostrad na poziomie państw (dane niepełne)
dat_motorways <- get_eurostat(id = 'ttr00002', time_format = 'num', filters = list(time = '2017')) %>%
  filter(tra_infr == 'MWAY') %>%
  dplyr::rename(mlenght = values)

dat_motorways_deaths <- dat_road %>%
  filter(tra_infr == 'MWAY') %>%
  select(geo, values, country) %>%
  right_join(dat_motorways) %>%
  mutate(mrate = values / mlenght * 1000) %>%
  drop_na()

# zabici w wypadkach na autostratach na 1 tys. km autostrady
ggplot(dat_motorways_deaths, aes(x = reorder(country, mrate), y = mrate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar śmiertelnych wypadków na 1 tys. km autostrad, 2017', y = 'Ofiary', x = NULL) +
  coord_flip()

####################################### zmiana w czasie ################################################################################

# porównanie zmiany liczby ofiar śmiertelnych wypadków drogowych na przestrzeni lat 2000-2018 w wybranych państwach UE
dat_series <-  get_eurostat(id = 'sdg_11_40', time_format = 'num') %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany"))) %>%
  filter(geo %in% c('PL', 'DE', 'FR', 'UK', 'ES', 'SE', 'IT', 'AT', 'BE', 'PT')) 

# zabici na 100 tys. pop.
dat_series_rt <- filter(dat_series, unit == "RT")

ggplot(dat_series_rt, aes(x = time, y = values, color = geo, label = country)) +
  geom_line(size = .9, alpha = .5) +
  geom_text(data = dat_series_rt %>% group_by(geo) %>% filter(time == max(time)), size = 3) + 
  theme(legend.position = 'none') +
  labs(title = "Ofiary śmiertelne wypadków drogowych, 2000-2018 (na 100 tys.)", x = "Rok", y = "Liczba (na 100 tys.)")

# zabici ogółem
dat_series_nr <- filter(dat_series, unit == "NR")

ggplot(dat_series_nr, aes(x = time, y = values, color = geo, label = country)) +
  geom_line(size = .9, alpha = .5) +
  geom_text(data = dat_series_nr %>% group_by(geo) %>% filter(time == max(time)), size = 3) + 
  theme(legend.position = 'none') +
  labs(title = "Ofiary śmiertelne wypadków drogowych, 2000-2018", x = "Rok", y = "Liczba")

####################################### ofiary wypadków na poziomie NUTS-2 #############################################################

dat_nuts <- get_eurostat(id = 'tran_r_acci', time_format = 'num')

mapdata_nuts <- get_eurostat_geospatial(nuts_level = 2) %>%
  left_join(dat_nuts) %>%
  filter(time == '2018', victim == 'KIL', unit == 'P_MHAB') %>%
  mutate(cat = cut_to_classes(values, n = 5, decimals = 1)) 


ggplot(mapdata_nuts, aes(fill = cat))+
  scale_fill_brewer(palette = 'Reds') +
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  xlim(c(-12,44)) + ylim(c(35,70)) +
  labs(title = 'Ofiary śmiertelne wypadków drogowych, 2018 (NUTS-2)', 
       subtitle = 'Ofiary na 1 mln.', 
       fill = 'Liczba',
       caption = 'Mapa 3.')

ggplot(mapdata_nuts, aes(fill = values))+
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  xlim(c(-12,44)) + ylim(c(35,70)) +
  labs(title = 'Ofiary śmiertelne wypadków drogowych, 2018 (NUTS-2)', 
       subtitle = 'Ofiary na 1 mln.', 
       fill = 'Liczba',
       caption = 'Mapa 4.')


# podział Niemiec

mapdata_nuts_de <- mapdata_nuts %>%
  filter(CNTR_CODE == 'DE') %>%
  mutate(cat = cut_to_classes(values, n = 3, decimals = 1))

ggplot(mapdata_nuts_de, aes(fill = cat)) +
  scale_fill_brewer(palette = 'Reds') +
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  xlim(c(5,15)) + ylim(c(47,55)) +
  labs(title = 'Ofiary śmiertelne wypadków drogowych w Niemczech, 2018 (NUTS-2)', 
       subtitle = 'Ofiary na 1 mln.', 
       fill = 'Liczba',
       caption = 'Mapa 5.')

ggplot(mapdata_nuts_de, aes(fill = values)) +
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  xlim(c(5,15)) + ylim(c(47,55)) +
  labs(title = 'Ofiary śmiertelne wypadków drogowych w Niemczech, 2018 (NUTS-2)', 
       subtitle = 'Ofiary na 1 mln.', 
       fill = 'Liczba',
       caption = 'Mapa 6.')

# historyczne granice NRD (dziwny obiekt, nie wiem jak go przetworzyć)
library(cshapes)
hist_geo <- cshapes::cshp(date = as.Date('1/1/1989', format='%m/%d/%Y'))

# Cytowanie (wymagane przez autora biblioteki):
# Weidmann, Nils B., Doreen Kuse, and Kristian Skrede Gleditsch. 2010. The Geography of the International System: The CShapes Dataset. International Interactions 36 (1).