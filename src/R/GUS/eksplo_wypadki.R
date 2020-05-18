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
  transmute(suma = sum(Wartosc)) %>% 
  distinct() -> wypadki_trend

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

