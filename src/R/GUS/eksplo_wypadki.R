library(dplyr)
library(ggplot2)
library(sf)
library(data.table)
library(tidyverse)

getwd()
setwd("~/Documents/szkoła/sem4/ADP/amu_adp_final_proj/src/R/GUS/")


drogi <- read.csv2("../../../data/GUS/drogi-polska.csv", sep = ";", encoding = "UTF-8")
wypadki <- read.csv2("../../../data/GUS/wypadki-polska.csv", sep = ";", encoding = "UTF-8")

pojazdy.polska <- read.csv('../../../data/GUS/pojazdy-polska.csv', sep = ";", encoding = "UTF-8")
ludnosc.polska <- read.csv('../../../data/GUS/ludnosc-polska.csv', sep = ';', encoding = "UTF-8")

powiaty <- st_read(dsn='../../../data/GUS/powiaty.gml', quiet = TRUE)

wypadki %>% 
  mutate(nowyKod = Kod / 1000, rodzaj.jst = if_else(nowyKod %% 100 == 0, 'woj', 'pow')) %>%
  filter(rodzaj.jst == 'woj', Wskaźniki == 'wypadki drogowe na 100 tys. ludności', Wartosc != '') %>% 
  select(Wartosc, Rok) %>% 
  group_by(Rok) %>% 
  transmute(suma = sum(Wartosc)) %>% 
  distinct() -> wypadki_trend

wypadki %>% 
  mutate(nowyKod = Kod / 1000, rodzaj.jst = if_else(nowyKod %% 100 == 0, 'woj', 'pow')) %>%
  filter(rodzaj.jst == 'pow', Wskaźniki == 'ofiary śmiertelne na 100 tys. pojazdów', Wartosc != '') %>% 
  select(-Wskaźniki, -Jednostka.miary, -Atrybut, -X, -rodzaj.jst) -> ofiary.na.100tysp

wypadki %>% 
  mutate(nowyKod = Kod / 1000, rodzaj.jst = if_else(nowyKod %% 100 == 0, 'woj', 'pow')) %>%
  filter(rodzaj.jst == 'pow', Wskaźniki == 'ofiary śmiertelne na 100 wypadków', Wartosc != '') %>% 
  select(-Wskaźniki, -Jednostka.miary, -Atrybut, -X,-rodzaj.jst) -> ofiary.na.100wyp
  
wypadki %>% 
  mutate(nowyKod = Kod / 1000, rodzaj.jst = if_else(nowyKod %% 100 == 0, 'woj', 'pow')) %>%
  filter(rodzaj.jst == 'pow', Wskaźniki == 'wypadki drogowe na 100 tys. ludności', Wartosc != '') %>% 
  select(-Wskaźniki, -Jednostka.miary, -Atrybut, -X, -rodzaj.jst) -> wypadki.na.100tysp

drogi %>% 
  mutate(nowyKod = Kod / 1000) %>%
  filter(Rodzaje.dróg == 'autostrady', Wartosc != '') %>% 
  select(Wartosc, Rok) %>% 
  group_by(Rok) %>% 
  transmute(suma = sum(Wartosc)) %>% 
  distinct() -> drogi_trend

summary(wypadki_trend)
summary(drogi_trend)

ylim.prim <- c(0, 2500)   # kilometry autostrad
ylim.sec <- c(1000, 2500)    # liczba wypadków

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])


ggplot() +
  geom_col(data = drogi_trend, aes(x=Rok, y=suma)) +
  geom_line(data = wypadki_trend, aes(x=Rok, y=suma), color = "red") +
  scale_y_continuous("Długość autostrad [km]", sec.axis = sec_axis(~ (. - a)/b, name = "Liczba wypadków na 100 tys. mieszkańców")) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")
  ) 


#wypadki na 100 tys. mieszkańców na mapie powiatów
wypadki.na.100tysp %>% 
  select(Rok, nowyKod, Wartosc) %>% 
  filter(Rok==2017) %>% 
  left_join(powiaty, by=c("nowyKod"="kodJednostki")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill=Wartosc)) +
  scale_fill_gradient(low="White", high = "Red") +
  labs(title = "Liczba wypadków drogowych na 100 tys. mieszkańców",
       subtitle = "Rok 2017",
       fill = 'wypadki')

#wykresy
wypadki.na.100tysp %>% 
  select(Rok, Nazwa, Wartosc) %>% 
  filter(Rok==2017) %>% 
  arrange(-Wartosc) %>% 
  head(10) %>% 
  ggplot(aes(x=reorder(Nazwa, Wartosc), y=Wartosc)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = "10 powiatów z najwyższą liczbą wypadków na 100 tys. mieszkańców", 
       subtitle = "Rok 2017",
       y="Liczba wypadków na 100 tys. mieszkańców") +
  coord_flip()


wypadki.na.100tysp %>% 
  select(Rok, Nazwa, Wartosc) %>% 
  filter(Rok==2017) %>% 
  arrange(-Wartosc) %>% 
  tail(10) %>% 
  ggplot(aes(x=reorder(Nazwa, -Wartosc), y=Wartosc)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = "10 powiatów z najniższą liczbą wypadków na 100 tys. mieszkańców", 
       subtitle = "Rok 2017",
       y="Liczba wypadków na 100 tys. mieszkańców") +
  coord_flip()

#ofiary śmiertelne na 100 tys. pojazdów
ofiary.na.100tysp %>% 
  select(Rok, nowyKod, Wartosc) %>% 
  filter(Rok==2017) %>% 
  left_join(powiaty, by=c("nowyKod"="kodJednostki")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill=Wartosc)) +
  scale_fill_gradient(low="White", high = "Red") +
  labs(title = "Liczba ofiar śmiertelnych wypadków drogowych na 100 tys. pojazdów",
       subtitle = "Rok 2017",
       fill = 'zgony')

#wykresy
ofiary.na.100tysp %>% 
  select(Rok, Nazwa, Wartosc) %>% 
  filter(Rok==2017) %>% 
  arrange(-Wartosc) %>% 
  head(10) %>% 
  ggplot(aes(x=reorder(Nazwa, Wartosc), y=Wartosc)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = "10 powiatów z najwyższą liczbą ofiar śmiertelnych na 100 tys. pojazdów", 
       subtitle = "Rok 2017",
       y="Liczba ofiar śmiertelnych na 100 tys. pojadów") +
  coord_flip()


ofiary.na.100tysp %>% 
  select(Rok, Nazwa, Wartosc) %>% 
  filter(Rok==2017) %>% 
  arrange(-Wartosc) %>% 
  tail(10) %>% 
  ggplot(aes(x=reorder(Nazwa, -Wartosc), y=Wartosc)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = "10 powiatów z najniższą liczbą ofiar śmiertelnych na 100 tys. pojazdów", 
       subtitle = "Rok 2017",
       y="Liczba ofiar śmiertelnych na 100 tys. pojadów") +
  coord_flip()

#ofiary śmiertelne na 100 wypadków
ofiary.na.100wyp %>% 
  select(Rok, nowyKod, Wartosc) %>% 
  filter(Rok==2017) %>% 
  left_join(powiaty, by=c("nowyKod"="kodJednostki")) %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(aes(fill=Wartosc)) +
  scale_fill_gradient(low="White", high = "Red") +
  labs(title = "Liczba ofiar śmiertelnych wypadków drogowych na 100 wypadków",
       subtitle = "Rok 2017",
       fill = 'zgony')

#wykresy
ofiary.na.100wyp %>% 
  select(Rok, Nazwa, Wartosc) %>% 
  filter(Rok==2017) %>% 
  arrange(-Wartosc) %>% 
  head(10) %>% 
  ggplot(aes(x=reorder(Nazwa, Wartosc), y=Wartosc)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = "10 powiatów z najwyższą liczbą ofiar śmiertelnych na 100 wypadków", 
       subtitle = "Rok 2017",
       y="Liczba ofiar śmiertelnych na 100 wypadków") +
  coord_flip()


ofiary.na.100wyp %>% 
  select(Rok, Nazwa, Wartosc) %>% 
  filter(Rok==2017) %>% 
  arrange(-Wartosc) %>% 
  tail(10) %>% 
  ggplot(aes(x=reorder(Nazwa, -Wartosc), y=Wartosc)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "none", axis.title.y = element_blank()) +
  labs(title = "10 powiatów z najniższą liczbą ofiar śmiertelnych na 100 wypadków", 
       subtitle = "Rok 2017",
       y="Liczba ofiar śmiertelnych na 100 wypadków") +
  coord_flip()
