library(eurostat)
library(tidyverse)
library(dplyr)
library(ggplot2)

############################ ogólne dane dot. wypadków drogowych w Europie ############################################

# id: zabici w wypadkach drogowych
dat <- get_eurostat(id = 'sdg_11_40', time_format = 'num', filters = list(time = '2018')) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany")))

# ca³kowita liczba zabitych (wykluczenie Turcji)
dat_2018_nr <- dat %>%
  filter(unit == "NR" & !geo %in% c('EU28', 'EU27_2020', 'TR')) %>%
  select(geo, country, values)

# zabici na 100 tys. mieszkañców (gotowe wyliczenie)
dat_2018_rt <- dat %>%
  filter(unit == "RT" & !geo %in% c('EU28', 'EU27_2020', 'TR')) %>%
  select(geo, country, values)

ggplot(dat_2018_nr, aes(x = reorder(country, values), y = values, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar œmiertelnych wypadków drogowych, 2018', y = 'Ofiary', x = NULL) +
  coord_flip() 

ggplot(dat_2018_rt, aes(x = reorder(country, values), y = values, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar smiertelnych wypadków drogowych, 2018', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip() 

# dane geograficzne dla zobrazowania wspó³czynnika œmiertelnoœci 
mapdata <- get_eurostat_geospatial(nuts_level = 0, resolution = 20, output_class = "sf") %>%
  right_join(dat_2018_rt) %>%
  mutate(cat = cut_to_classes(values, n = 4, decimals = 1))

# mapa (wype³nienie w skali dyskretnej - podzia³ na 4 klasy)
ggplot(mapdata, aes(fill = cat)) + 
  scale_fill_brewer(palette = 'Reds') + 
  geom_sf(color = alpha('black', 1/3), alpha = .6) + 
  coord_sf(xlim = c(-20,44), ylim = c(30,70)) +
  labs(title = 'Ofiary œmiertelne wypadków drogowych, 2018', 
       subtitle = '(na 100 tys. mieszkañców)', 
       fill = 'Ofiary',
       caption = 'Mapa 1.')

# mapa (wype³nienie w skali ci¹g³ej)
ggplot(mapdata, aes(fill = values)) +
  geom_sf(color = alpha('black', 1/3), alpha = .6) + 
  coord_sf(xlim = c(-20,44), ylim = c(30,70)) +
  labs(title = 'Ofiary œmiertelne wypadków drogowych, 2018', 
       subtitle = 'Œrednia na 100 tys. mieszkañców', 
       fill = 'Wynik',
       caption = 'Mapa 2.')


####################################### dane nt. rozmiaru populacji do obliczeñ ###############################

# dane wymagaj¹ przeliczenia w stosunku do populacji w celu uzsykania miarodajnego 
# porównania sytuacji w poszczególnych pañstwach
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
  labs(title = 'Liczba ofiar smiertelnych wypadków z udzia³em samochodów osobowych, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip() 

# zabici w wypadkach rowerzyœci
ggplot(filter(dat_vehicle, vehicle == 'BIKE'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar smiertelnych wypadków z udzia³em rowerzystów, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip() 

# brakuje danych obrazuj¹cych ofiary wypadków z udzia³em innych typów pojazdów, w tym pojazdów transportowych lub motocykli

####################################### ofiary wypadków wg u¿ytkowników dróg #########################################################

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
  labs(title = 'Liczba ofiar œmiertelnych wœród pieszych uczestników ruchu, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach pasa¿erowie pojazdów 
ggplot(filter(dat_users, pers_inv == 'PAS'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar œmiertelnych wœród pasa¿erów pojazdów, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach kieruj¹cy pojazdami 
ggplot(filter(dat_users, pers_inv == 'DRIV'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar œmiertelnych wœród kieruj¹cych pojazdami, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
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
  labs(title = 'Liczba ofiar œmiertelnych wypadków na autorstradach, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach na drogach obszarów zabudowanych
ggplot(filter(dat_road, tra_infr == 'RD_URB'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar œmiertelnych wypadków drogowych w obszarze zabudowanym, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach na drogach wiejskich
ggplot(filter(dat_road, tra_infr == 'RD_RUR'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar œmiertelnych wypadków na drogach wiejskich, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()


######################################## ofiary wypadków drogowych ze wzglêdu na wiek ##################################################

####################################### zmiana w czasie ################################################################################

# porównanie zmiany liczby ofiar œmiertelnych wypadków drogowych na przestrzeni lat 2000-2018 w wybranych pañstwach UE
dat_series <-  get_eurostat(id = 'sdg_11_40', time_format = 'num') %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany"))) %>%
  filter(geo %in% c('PL', 'DE', 'FR', 'UK', 'ES', 'SE', 'IT', 'AT', 'BE', 'PT')) 

# zabici na 100 tys. pop.
dat_series_rt <- filter(dat_series, unit == "RT")

ggplot(dat_series_rt, aes(x = time, y = values, color = geo, label = country)) +
  geom_line(size = .9, alpha = .5) +
  geom_text(data = dat_series_rt %>% group_by(geo) %>% filter(time == max(time)), size = 3) + 
  theme(legend.position = 'none') +
  labs(title = "Ofiary œmiertelne wypadków drogowych, 2000-2018 (na 100 tys.)", x = "Rok", y = "Liczba (na 100 tys.)")

# zabici ogó³em
dat_series_nr <- filter(dat_series, unit == "NR")

ggplot(dat_series_nr, aes(x = time, y = values, color = geo, label = country)) +
  geom_line(size = .9, alpha = .5) +
  geom_text(data = dat_series_nr %>% group_by(geo) %>% filter(time == max(time)), size = 3) + 
  theme(legend.position = 'none') +
  labs(title = "Ofiary œmiertelne wypadków drogowych, 2000-2018", x = "Rok", y = "Liczba")



# 1. liczba samochodów osobowych na 1000 mieszkanców: road_eqs_carhab
# 2. liczba pojazdów z podzialem na kategorie i NUTS 2: tran_r_vehst
# 3. dlugosc autostrad i dróg szybkiego ruchu: road_if_motorwa

####################################### liczba samochodów na 1000 mieszkancow ############################################
# NOTE: usunac Turcje


dat_pass_cars_2018 <- get_eurostat(id = 'road_eqs_carhab', time_format = 'num', filters = list(time = '2018')) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany", XK = "Kosovo"))) 


ggplot(dat_pass_cars_2018, aes(x = reorder(country, values), y = values, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba samochodow na 1000 mieszkancow, 2018', y = 'Samochody', x = NULL) +
  coord_flip() 

mapdata_pass_cars_2018 <- get_eurostat_geospatial(nuts_level = 0, resolution = 20, output_class = "sf") %>%
  right_join(dat_pass_cars_2018) %>%
  mutate(cat = cut_to_classes(values, n = 4, decimals = 1))

ggplot(mapdata_pass_cars_2018, aes(fill = cat)) + 
  scale_fill_brewer(palette = 'Reds') + 
  geom_sf(color = alpha('black', 1/3), alpha = .6) + 
  coord_sf(xlim = c(-20,44), ylim = c(30,70)) +
  labs(title = 'Liczba samochodów, 2018', 
       subtitle = '(na 1000 mieszkancow)', 
       fill = 'Samochody',
       caption = 'Mapa 1.')

####################################### dlugosc autostrad ############################################
# NOTE: usuniete drogi ekspresowe, bo duzo N/A

dat_motorway_2018 <- get_eurostat(id = 'road_if_motorwa', time_format = 'num', filters = list(time = '2018')) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany", XK = "Kosovo")))
  
dat_motorway_2018[is.na(dat_motorway_2018)] <- 0  

dat_motorway_2018 <- dat_motorway_2018 %>%
  filter(tra_infr == 'MWAY') 
  
ggplot(dat_motorway_2018, aes(x = reorder(country, values), y = values, fill = ifelse(country == "Poland", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Dlugosc autostrad, 2018', y = 'Km', x = NULL) +
  coord_flip() 

mapdata_motorway_2018 <- get_eurostat_geospatial(nuts_level = 0, resolution = 20, output_class = "sf") %>%
  right_join(dat_motorway_2018) %>%
  mutate(cat = cut_to_classes(values, n = 4, decimals = 1))

ggplot(mapdata_motorway_2018, aes(fill = cat)) + 
  scale_fill_brewer(palette = 'Reds') + 
  geom_sf(color = alpha('black', 1/3), alpha = .6) + 
  coord_sf(xlim = c(-20,44), ylim = c(30,70)) +
  labs(title = 'Dlugosc autostrad, 2018', 
       fill = 'Km',
       caption = 'Mapa 1.')
