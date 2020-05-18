library(dplyr)
library(ggplot2)

getwd()
setwd("~/Documents/szkoła/sem4/ADP/amu_adp_final_proj/src/R/GUS/")


drogi <- read.csv2("../../../data/GUS/drogi-polska.csv", sep = ";")
wypadki <- read.csv2("../../../data/GUS/wypadki-polska.csv")

wypadki %>% 
  mutate(nowyKod = Kod / 1000, rodzaj.jst = if_else(nowyKod %% 100 == 0, 'woj', 'pow')) %>%
  filter(rodzaj.jst == 'woj', Wskaźniki == 'wypadki drogowe na 100 tys. ludności', Wartosc != '') %>% 
  select(Wartosc, Rok) %>% 
  group_by(Rok) %>% 
  transmute(suma = sum(Wartosc)) -> wypadki_trend

drogi %>% 
  mutate(nowyKod = Kod / 1000) %>%
  filter(Rodzaje.dróg == 'autostrady', Wartosc != '') %>% 
  select(Nazwa, Wartosc, Rok) %>% 
  ggplot(aes(x=Rok, y=Wartosc)) +
  geom_col() +
  geom_line(data = wypadki_trend, aes(x=Rok, y=suma))