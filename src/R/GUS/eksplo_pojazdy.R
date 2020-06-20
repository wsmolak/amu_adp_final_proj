library(tidyverse)
library(data.table)
library(sf)

getwd()
setwd("~/Documents/szkoła/sem4/ADP/amu_adp_final_proj/src/R/GUS/")

pojazdy.polska <- read.csv('../../../data/GUS/pojazdy-polska.csv', sep = ";", encoding = "UTF-8")
ludnosc.polska <- read.csv('../../../data/GUS/ludnosc-polska.csv', sep = ';', encoding = "UTF-8")

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
  
#pojazdy ogółem rok 2015 na mapie powiatów
pojazdy.ogolem.powiaty %>% 
  select(Rok, Kod, suma) %>% 
  filter(Rok==2017) %>% 
  mutate(KodJoin = Kod / 1000) %>% 
  left_join(powiaty, by=c("KodJoin"="kodJednostki")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill=suma))


powiaty %>% 
  select(-waznyDo, -waznyOd, -gml_id, -lokalnyId, -przestrzenNazw, -rodzajJednostki, -nazwaJednostki, -organAdmPublicznej, -wersjaId, -poczatekWersjiObiektu) -> powiaty

ludnosc.polska %>% 
  select(Kod, Nazwa, Wiek, Płeć, Wartosc, Rok) %>% 
  filter(Wiek == 'ogółem', Płeć == 'ogółem', str_detect(Nazwa, "^Powiat")) %>% 
  drop_na() -> ludnosc.polska.ogolem.powiaty

pojazdy.ogolem.powiaty %>% 
  select(Rok, Kod, Nazwa, suma) %>% 
  left_join(ludnosc.polska.ogolem.powiaty, by=c("Kod"="Kod", "Rok"="Rok")) %>% 
  drop_na() %>% 
  transmute(Nazwa=Nazwa.x, liczba.pojazdow=suma, liczba.ludnosci=Wartosc, pojazdy.na.100tysm=liczba.pojazdow/(liczba.ludnosci/100000)) %>% 
  distinct() -> pojazdy.ludnosc.powiaty

quantiles <- summary(pojazdy.ludnosc.powiaty$pojazdy.na.100tysm)

pojazdy.ludnosc.powiaty$klasy.pojazdy.na.100tys = case_when(
  pojazdy.ludnosc.powiaty$pojazdy.na.100tysm < quantiles[[2]] ~ sprintf("0 ~< %.2f", quantiles[[2]]),
  pojazdy.ludnosc.powiaty$pojazdy.na.100tysm >= quantiles[[2]] & pojazdy.ludnosc.powiaty$pojazdy.na.100tysm < quantiles[[4]] ~ sprintf("%.2f ~< %.2f", quantiles[[2]], quantiles[[4]]),
  pojazdy.ludnosc.powiaty$pojazdy.na.100tysm >= quantiles[[4]] & pojazdy.ludnosc.powiaty$pojazdy.na.100tysm < quantiles[[5]] ~ sprintf("%.2f ~< %.2f", quantiles[[4]], quantiles[[5]]),
  pojazdy.ludnosc.powiaty$pojazdy.na.100tysm >= quantiles[[5]] & pojazdy.ludnosc.powiaty$pojazdy.na.100tysm <= quantiles[[6]] ~ sprintf("%.2f ~< %.2f", quantiles[[5]], quantiles[[6]]),
)

pojazdy.ludnosc.powiaty$klasy.pojazdy.na.100tys = factor(pojazdy.ludnosc.powiaty$klasy.pojazdy.na.100tys)

pojazdy.ludnosc.powiaty %>% 
  filter(Kod==221000) %>% 
  head()

#pojazdy na 100 tys. mieszkańców rok 2017 na mapie powiatów
pojazdy.ludnosc.powiaty %>% 
  select(Rok, Kod, pojazdy.na.100tysm) %>% 
  filter(Rok==2017) %>% 
  mutate(KodJoin = Kod / 1000) %>% 
  left_join(powiaty, by=c("KodJoin"="kodJednostki")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill=pojazdy.na.100tysm)) +
  scale_fill_gradient(low="White", high = "Red") +
  labs(title = "Liczba pojazdów na 100 tys. mieszkańców",
       subtitle = "Rok 2017",
       fill = 'pojazdy')

#Wykres top 10 pojazdów na 100 tys. mieszkańców
pojazdy.ludnosc.powiaty %>%
  filter(Rok==2017) %>% 
  select(Nazwa, pojazdy.na.100tysm) %>% 
  arrange(-pojazdy.na.100tysm) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(Nazwa, pojazdy.na.100tysm), y = pojazdy.na.100tysm)) + 
  geom_bar(stat = "identity") +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = "10 powiatów z najwyższą liczbą pojazdów na 100 tys. mieszkańców", 
       subtitle = "Rok 2017",
       y="Liczba pojazdów na 100 tys. mieszkańców") +
  coord_flip()

#Wykres bottom 10 pojazdów na 100 tys. mieszkańców
pojazdy.ludnosc.powiaty %>%
  filter(Rok==2017) %>% 
  select(Nazwa, pojazdy.na.100tysm) %>% 
  arrange(pojazdy.na.100tysm) %>% 
  head(10) %>% 
  ggplot(aes(x = reorder(Nazwa, -pojazdy.na.100tysm), y = pojazdy.na.100tysm)) + 
  geom_bar(stat = "identity") +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = "10 powiatów z najniższą liczbą pojazdów na 100 tys. mieszkańców", 
       subtitle = "Rok 2017",
       y="Liczba pojazdów na 100 tys. mieszkańców") +
  coord_flip()

#pojazdy na 100 tys. mieszkańców rok 2017 na mapie powiatów - klasy
pojazdy.ludnosc.powiaty %>% 
  select(Rok, Kod, klasy.pojazdy.na.100tys) %>% 
  filter(Rok==2017) %>% 
  mutate(KodJoin = Kod / 1000) %>% 
  left_join(powiaty, by=c("KodJoin"="kodJednostki")) %>% 
  ggplot(aes(geometry = geometry, fill=klasy.pojazdy.na.100tys)) +
  scale_fill_brewer(palette = 'Reds') +
  geom_sf(color = alpha('black', 1/3), alpha = .6) +
  labs(title = "Liczba pojazdów na 100 tys. mieszkańców",
       subtitle = "Rok 2017",
       fill = 'pojazdy')
