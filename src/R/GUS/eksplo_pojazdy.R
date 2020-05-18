library(tidyverse)

pojazdy.polska <- read.csv('../../data/GUS/pojazdy-polska.csv', sep = ";")


summary(pojazdy.polska)

pojazdy.polska %>% 
  filter(Kod == 203000, Grupy.wieku == 'ogółem', Pojazdy == 'samochody osobowe') %>% 
  head()


ludnosc.polska <- read.csv('../../data/GUS/ludnosc-polska.csv', sep = ';')

ludnosc.polska %>% 
  filter(Rok == 2016) %>% 
  head()
