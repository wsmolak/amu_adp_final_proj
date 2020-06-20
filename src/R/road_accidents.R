library(eurostat)
library(tidyverse)
library(dplyr)
library(ggplot2)

############################ ogÃ³lne dane dot. wypadkÃ³w drogowych w Europie ############################################

# id: zabici w wypadkach drogowych
dat <- get_eurostat(id = 'sdg_11_40', time_format = 'num', filters = list(time = '2018')) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany")))

# caÅ‚kowita liczba zabitych (wykluczenie Turcji)
dat_2018_nr <- dat %>%
  filter(unit == "NR" & !geo %in% c('EU28', 'EU27_2020', 'TR')) %>%
  select(geo, country, values)

# zabici na 100 tys. mieszkaÅ„cÃ³w (gotowe wyliczenie)
dat_2018_rt <- dat %>%
  filter(unit == "RT" & !geo %in% c('EU28', 'EU27_2020', 'TR')) %>%
  select(geo, country, values)

ggplot(dat_2018_nr, aes(x = reorder(country, values), y = values, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar Å›miertelnych wypadkÃ³w drogowych, 2018', y = 'Ofiary', x = NULL) +
  coord_flip() 

ggplot(dat_2018_rt, aes(x = reorder(country, values), y = values, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar Å›miertelnych wypadkÃ³w drogowych, 2018', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip() 

# dane geograficzne dla zobrazowania wspÃ³Å‚czynnika Å›miertelnoÅ›ci 
mapdata <- get_eurostat_geospatial(nuts_level = 0, resolution = 20, output_class = "sf") %>%
  right_join(dat_2018_rt) %>%
  mutate(cat = cut_to_classes(values, n = 4, decimals = 1))

# mapa (wypeÅ‚nienie w skali dyskretnej - podziaÅ‚ na 4 klasy)
ggplot(mapdata, aes(fill = cat)) + 
  scale_fill_brewer(palette = 'Reds') + 
  geom_sf(color = alpha('black', 1/3), alpha = .6) + 
  coord_sf(xlim = c(-20,44), ylim = c(30,70)) +
  labs(title = 'Ofiary Å›miertelne wypadkÃ³w drogowych, 2018', 
       subtitle = '(na 100 tys. mieszkaÅ„cÃ³w)', 
       fill = 'Ofiary',
       caption = 'Mapa 1.')

# mapa (wypeÅ‚nienie w skali ciÄ…gÅ‚ej)
ggplot(mapdata, aes(fill = values)) +
  geom_sf(color = alpha('black', 1/3), alpha = .6) + 
  scale_fill_gradient(low="white", high="red") +
  coord_sf(xlim = c(-20,44), ylim = c(30,70)) +
  labs(title = 'Ofiary Å›miertelne wypadkÃ³w drogowych, 2018', 
       subtitle = 'Åšrednia na 100 tys. mieszkaÅ„cÃ³w', 
       fill = 'Wynik',
       caption = 'Mapa 2.')


####################################### dane nt. rozmiaru populacji do obliczeÅ„ ###############################

# dane wymagajÄ… przeliczenia w stosunku do populacji w celu uzsykania miarodajnego 
# porÃ³wnania sytuacji w poszczegÃ³lnych paÅ„stwach
dat_population <- get_eurostat(id = 'tps00001', time_format = 'num', filters = list(time = '2017')) %>%
  rename(population = values)


####################################### ofiary wypadkÃ³w wg pojazdu #############################################

# id: tran_sf_roadve
dat_vehicle <- get_eurostat(id = 'tran_sf_roadve', time_format = 'num', filters = list(time = '2017')) %>%
  left_join(dat_population) %>%
  mutate(rate = values / population * 100000) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany"))) %>%
  drop_na()

# zabici w wypadkach uzytkownicy samochodÃ³w osobowych
ggplot(filter(dat_vehicle, vehicle == 'CAR'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar smiertelnych wypadkÃ³w z udziaÅ‚em samochodÃ³w osobowych, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip() 

# zabici w wypadkach rowerzyÅ›ci
ggplot(filter(dat_vehicle, vehicle == 'BIKE'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar smiertelnych wypadkÃ³w z udziaÅ‚em rowerzystÃ³w, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip() 

# brakuje danych obrazujÃ„â€¦cych ofiary wypadkÃ³w z udziaÅ‚em innych typÃ³w pojazdÃ³w, w tym pojazdÃ³w transportowych lub motocykli

####################################### ofiary wypadkÃ³w wg uÄ¹Ä½ytkownikÃ³w drÃ³g #########################################################

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
  labs(title = 'Liczba ofiar Å›miertelnych wÅ›rÃ³d pieszych uczestnikÃ³w ruchu, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach pasaÄ¹Ä½erowie pojazdÃ³w 
ggplot(filter(dat_users, pers_inv == 'PAS'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar Å›miertelnych wÅ›rÃ³d pasaÄ¹Ä½erÃ³w pojazdÃ³w, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach kierujÃ„â€¦cy pojazdami 
ggplot(filter(dat_users, pers_inv == 'DRIV'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar Å›miertelnych wÅ›rÃ³d kierujÃ„â€¦cych pojazdami, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

####################################### ofiary wypadkÃ³w wg rodzaju infrastruktury drogowej ############################################

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
  labs(title = 'Liczba ofiar Å›miertelnych wypadkÃ³w na autorstradach, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach na drogach obszarÃ³w zabudowanych
ggplot(filter(dat_road, tra_infr == 'RD_URB'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar Å›miertelnych wypadkÃ³w drogowych w obszarze zabudowanym, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

# zabici w wypadkach na drogach wiejskich
ggplot(filter(dat_road, tra_infr == 'RD_RUR'), aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar Å›miertelnych wypadkÃ³w na drogach wiejskich, 2017', y = 'Ofiary (na 100 tys.)', x = NULL) +
  coord_flip()

####################################### ofiary Å›miertelne na 1 tys. km autostrady ######################################################

# dÅ‚ugoÅ›Ä‡ sieci autostrad na poziomie paÅ„stw (dane niepeÅ‚ne)
dat_motorways <- get_eurostat(id = 'ttr00002', time_format = 'num', filters = list(time = '2017')) %>%
  filter(tra_infr == 'MWAY') %>%
  rename(mlenght = values)

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
  labs(title = 'Liczba ofiar Å›miertelnych wypadkÃ³w na 1 tys. km autostrad, 2017', y = 'Ofiary', x = NULL) +
  coord_flip()


# inna biblioteka, latwiej ja znalezc, wyniki te same
dat_motorways2 <- get_eurostat(id = 'road_if_motorwa', time_format = 'num', filters = list(time = '2017')) %>%
  filter(tra_infr == 'MWAY') 

dat_motorways_deaths2 <- dat_road %>%
  filter(tra_infr == 'MWAY') %>%
  select(geo, values, country) %>%
  right_join(dat_motorways) %>%
  mutate(mrate = values / mlenght * 1000) %>%
  drop_na()

ggplot(dat_motorways_deaths2, aes(x = reorder(country, mrate), y = mrate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba ofiar Å›miertelnych wypadkÃ³w na 1 tys. km autostrad, 2017', y = 'Ofiary', x = NULL) +
  coord_flip()



####################################### zmiana w czasie ################################################################################

# porÃ³wnanie zmiany liczby ofiar Å›miertelnych wypadkÃ³w drogowych na przestrzeni lat 2000-2018 w wybranych paÅ„stwach UE
dat_series <-  get_eurostat(id = 'sdg_11_40', time_format = 'num') %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany"))) %>%
  filter(geo %in% c('PL', 'DE', 'FR', 'UK', 'ES', 'SE', 'IT', 'AT', 'BE', 'PT')) 

# zabici na 100 tys. pop.
dat_series_rt <- filter(dat_series, unit == "RT")

ggplot(dat_series_rt, aes(x = time, y = values, color = geo, label = country)) +
  geom_line(size = .9, alpha = .5) +
  geom_text(data = dat_series_rt %>% group_by(geo) %>% filter(time == max(time)), size = 3) + 
  theme(legend.position = 'none') +
  labs(title = "Ofiary Å›miertelne wypadkÃ³w drogowych, 2000-2018 (na 100 tys.)", x = "Rok", y = "Liczba (na 100 tys.)")

# zabici ogÃ³Å‚em
dat_series_nr <- filter(dat_series, unit == "NR")

ggplot(dat_series_nr, aes(x = time, y = values, color = geo, label = country)) +
  geom_line(size = .9, alpha = .5) +
  geom_text(data = dat_series_nr %>% group_by(geo) %>% filter(time == max(time)), size = 3) + 
  theme(legend.position = 'none') +
  labs(title = "Ofiary Å›miertelne wypadkÃ³w drogowych, 2000-2018", x = "Rok", y = "Liczba")

####################################### ofiary wypadkÃ³w na poziomie NUTS-2 #############################################################

dat_nuts <- get_eurostat(id = 'tran_r_acci', time_format = 'num')

mapdata_nuts <- get_eurostat_geospatial(nuts_level = 2) %>%
  left_join(dat_nuts) %>%
  filter(time == '2018', victim == 'KIL', unit == 'P_MHAB') %>%
  mutate(cat = cut_to_classes(values, n = 5, decimals = 1)) 


ggplot(mapdata_nuts, aes(fill = cat))+
  scale_fill_brewer(palette = 'Reds') +
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  xlim(c(-12,44)) + ylim(c(35,70)) +
  labs(title = 'Ofiary Å›miertelne wypadkÃ³w drogowych, 2018 (NUTS-2)', 
       subtitle = 'Ofiary na 1 mln.', 
       fill = 'Liczba',
       caption = 'Mapa 3.')

ggplot(mapdata_nuts, aes(fill = values))+
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  scale_fill_gradient(low="white", high="red") +
  xlim(c(-12,44)) + ylim(c(35,70)) +
  labs(title = 'Ofiary Å›miertelne wypadkÃ³w drogowych, 2018 (NUTS-2)', 
       subtitle = 'Ofiary na 1 mln.', 
       fill = 'Liczba',
       caption = 'Mapa 4.')


# podziaÅ‚ Niemiec

mapdata_nuts_de <- mapdata_nuts %>%
  filter(CNTR_CODE == 'DE') %>%
  mutate(cat = cut_to_classes(values, n = 3, decimals = 1))

ggplot(mapdata_nuts_de, aes(fill = cat)) +
  scale_fill_brewer(palette = 'Reds') +
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  xlim(c(5,15)) + ylim(c(47,55)) +
  labs(title = 'Ofiary Å›miertelne wypadkÃ³w drogowych w Niemczech, 2018 (NUTS-2)', 
       subtitle = 'Ofiary na 1 mln.', 
       fill = 'Liczba',
       caption = 'Mapa 5.')

ggplot(mapdata_nuts_de, aes(fill = values)) +
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  scale_fill_gradient(low="white", high="red") +
  xlim(c(5,15)) + ylim(c(47,55)) +
  labs(title = 'Ofiary Å›miertelne wypadkÃ³w drogowych w Niemczech, 2018 (NUTS-2)', 
       subtitle = 'Ofiary na 1 mln.', 
       fill = 'Liczba',
       caption = 'Mapa 6.')

# historyczne granice NRD
library(cshapes)
hist_geo <- cshapes::cshp(as.Date("1989-1-1"), useGW = TRUE)
hist_geo@data$geo_code <- as.character(hist_geo@data$ISO1AL3)
div_de <- hist_geo[hist_geo@data$geo_code %in% c('DDR'), ]


ggplot() +
  geom_sf(data = mapdata_nuts_de, mapping = aes(fill = cat), color = alpha('white', 1/3)) +
  scale_fill_brewer(palette = 'Oranges', direction = 1, guide = 'legend') +
  geom_polygon(data = div_de, mapping = aes(long, lat, group = group, fill = '9 ~< 30', alpha = 1), color = 'black', show.legend = FALSE) +
  labs(title = 'Ofiary Å›miertelne wypadkÃ³w drogowych w Niemczech, 2018 (NUTS-2)', 
       subtitle = 'Naniesione granice dawnego NRD', 
       fill = 'Liczba',
       caption = 'Mapa 7.')

ggplot() +
  geom_sf(data = mapdata_nuts_de, mapping = aes(fill = values), color = alpha('white', 1/3)) +
  scale_fill_gradient(low="white", high="red") +
  geom_polygon(data = div_de, mapping = aes(long, lat, group = group, fill = 0, alpha = 1), color = 'black', show.legend = FALSE) +
  labs(title = 'Ofiary Å›miertelne wypadkÃ³w drogowych w Niemczech, 2018 (NUTS-2)', 
       subtitle = 'Naniesione granice dawnego NRD', 
       fill = 'Liczba',
       caption = 'Mapa 7.')

####################################### liczba samochodów osobowych ############################################

<<<<<<< HEAD
####################################### liczba samochod�w osobowych ############################################


# liczba samochod�w z podzialem na wiek - id: road_eqs_carage
=======
# liczba samochodów z podzialem na wiek - id: road_eqs_carage
>>>>>>> 4c239dfaa9c99e7e0b740bd92e4d55b31115068e
dat_pass_cars_tot <- get_eurostat(id = 'road_eqs_carage', time_format = 'num', filters = list(time = '2018')) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany", XK = "Kosovo"))) %>%
  filter(age == "TOTAL" & !geo %in% c('EU28', 'EU27_2020', 'TR')) %>%
  dplyr::rename(cars_qty = values) %>%
  select(geo, cars_qty, country) %>% 
  drop_na()


# liczba samochodów osobowych na 1000 mieszkancow - id: road_eqs_carhab
dat_pass_cars <- get_eurostat(id = 'road_eqs_carhab', time_format = 'num', filters = list(time = '2018')) %>%
  mutate(country = label_eurostat(geo, dic = "geo", lang = 'en', custom_dic = c(DE = "Germany", XK = "Kosovo"))) %>%
  filter(!geo %in% c('EU28', 'EU27_2020', 'TR')) %>%
  drop_na() %>% 
  select(geo, values, country)

ggplot(dat_pass_cars, aes(x = reorder(country, values), y = values, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'Liczba samochodow osobowych na 1000 mieszkancow, 2018', y = 'Samochody', x = NULL) +
  coord_flip()

# stosunek liczby zgonow w wypadkach drogowych do liczby samochodow
dat_deaths_pass_cars <- dat %>%
  filter(unit == "NR" & !geo %in% c('EU28', 'EU27_2020', 'TR')) %>%
  select(geo, country, values)  %>%
  dplyr::rename(dat_values = values) %>%
  left_join(dat_pass_cars_tot) %>%
  mutate(rate = dat_values / cars_qty * 100000) %>% 
  drop_na() 

ggplot(dat_deaths_pass_cars, aes(x = reorder(country, rate), y = rate, fill = ifelse(geo == "PL", "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + 
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = 'stosunek liczby zgonow w wypadkach drogowych do liczby samochodow, 2018', y = 'liczba zgonów na 100000 samochodow', x = NULL) +
  coord_flip()


mapdata_deaths_pass_cars <- get_eurostat_geospatial(nuts_level = 0, resolution = 20, output_class = "sf") %>%
  right_join(dat_deaths_pass_cars) %>%
  mutate(cat = cut_to_classes(rate, n = 4, decimals = 1))

ggplot(mapdata_deaths_pass_cars, aes(fill = cat)) + 
  scale_fill_brewer(palette = 'Reds') + 
  geom_sf(color = alpha('black', 1/3), alpha = .6) + 
  coord_sf(xlim = c(-20,44), ylim = c(30,70)) +
  labs(title = 'liczba zgonów, 2017', 
       subtitle = '(na 100000 samochodow osobowych)', 
       fill = 'zgony',
       caption = 'Mapa 8.')

# Cytowanie (wymagane przez autora biblioteki):
# Weidmann, Nils B., Doreen Kuse, and Kristian Skrede Gleditsch. 2010. The Geography of the International System: The CShapes Dataset. International Interactions 36 (1).