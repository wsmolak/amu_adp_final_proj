library(tidyverse)
library(data.table)
library(sf)

getwd()
setwd("~/Documents/szkoła/sem4/ADP/amu_adp_final_proj/src/R/GUS/")

pojazdy.polska <- read.csv('../../../data/GUS/pojazdy-polska.csv', sep = ";")
ludnosc.polska <- read.csv('../../../data/GUS/ludnosc-polska.csv', sep = ';')

powiaty <- st_read(dsn='../../../data/GUS/powiaty.gml', quiet = TRUE)


summary(pojazdy.polska)

pojazdy.polska %>% 
  select(Pojazdy) %>% 
  distinct()

pojazdy.polska %>% 
  select(Grupy.wieku) %>% 
  distinct()

pojazdy.polska %>% 
  select(Wartosc, Rok, Grupy.wieku) %>% 
  filter(Pojazdy == 'samochody osobowe') %>% 
  group_by(Rok, Grupy.wieku) %>% 
  transmute(suma = sum(Wartosc)) %>% 
  drop_na() %>% 
  distinct() -> pojazdy_wiek_trend_polska

pojazdy.polska %>% 
  filter(str_detect(Nazwa, "^[\\p{Lu}\\-]+$")) %>% #nie można użyć [A-Z] - nie łapie znaków unicode ŚĆŻŹ itp
  select(Nazwa, Wartosc, Rok, Grupy.wieku) %>% 
  group_by(Rok, Nazwa, Grupy.wieku) %>% 
  transmute(suma = sum(Wartosc)) %>% 
  drop_na() %>% 
  distinct() -> pojazdy_wiek_trend_woj

pojazdy.polska %>% 
  select(Wartosc, Rok, Grupy.wieku, Kod, Nazwa) %>% 
  filter(Grupy.wieku == 'ogółem', str_detect(Nazwa, "^Powiat")) %>% 
  group_by(Rok, Kod, Nazwa) %>% 
  mutate(suma = sum(Wartosc)) %>% 
  drop_na() %>% 
  distinct() -> pojazdy.ogolem.powiaty
  

pojazdy.polska %>% 
  select(Nazwa, Kod, Rok, Wartosc, Grupy.wieku) %>% 
  filter(Kod == 201000, Rok == 2015, Grupy.wieku == 'ogółem') 
  
#47682
  
powiaty %>% 
  filter(nazwaJednostki == "powiat bolesławiecki") %>% 
  head()

pojazdy.ogolem.powiaty$suma

#pojazdy ogółem rok 2015 na mapie powiatów
pojazdy.ogolem.powiaty %>% 
  select(Rok, Kod, suma) %>% 
  filter(Rok==2015) %>% 
  mutate(KodJoin = Kod / 1000) %>% 
  left_join(powiaty, by=c("KodJoin"="kodJednostki")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill=suma))


powiaty %>% 
  ggplot(aes(geometry = geometry))



ludnosc.polska %>% 
  select(Kod, Nazwa, Wiek, Płeć, Wartosc, Rok) %>% 
  filter(Wiek == 'ogółem', Płeć == 'ogółem', str_detect(Nazwa, "^Powiat")) %>% 
  drop_na() -> ludnosc.polska.ogolem.powiaty

pojazdy.ogolem.powiaty %>% 
  select(Rok, Kod, suma) %>% 
  left_join(ludnosc.polska.ogolem.powiaty, by=c("Kod"="Kod", "Rok"="Rok")) %>% 
  drop_na() %>% 
  transmute(liczba.pojazdow=suma, liczba.ludnosci=Wartosc, pojazdy.na.100tysm=liczba.pojazdow/(liczba.ludnosci/100000)) %>% 
  distinct() -> pojazdy.ludnosc.powiaty

#pojazdy na 100 tys. mieszkańców rok 2015 na mapie powiatów
pojazdy.ludnosc.powiaty %>% 
  select(Rok, Kod, pojazdy.na.100tysm) %>% 
  filter(Rok==2015) %>% 
  mutate(KodJoin = Kod / 1000) %>% 
  left_join(powiaty, by=c("KodJoin"="kodJednostki")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill=pojazdy.na.100tysm))
