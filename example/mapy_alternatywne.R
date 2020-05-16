library(eurostat)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(broom)
library(ggrepel) # dla ³adnego umieszczenia labelek

dat <- get_eurostat(id = 'sdg_11_40', time_format = 'num', filters = list(time = '2018'))

dat_2018_rt <- dat %>%
  filter(unit == "RT" & !geo %in% c('EU28', 'EU27_2020', 'TR'))

geo <- get_eurostat_geospatial(output_class = 'df', nuts_level = 0)

#install package maptools
map_df <- broom::tidy(geo, region = "NUTS_ID")

country_maps_df <- geo %>%
  # poziom pañstw
  filter(nchar(NUTS_ID) == 2) %>%
  filter(lat > 30) # ucinamy wyspy na Atlantyku

plot_data <- left_join(dat_2018_rt, country_maps_df, by = c("geo" = "NUTS_ID")) %>%
  mutate(cat = cut_to_classes(values, n = 4, decimals = 1))

label_eurostat(plot_data, lang = 'en')

county_maps_means_df <- plot_data %>%
  group_by(geo) %>%
  mutate(mlong = mean(long), mlat = mean(lat)) %>%
  ungroup() %>%
  select(geo, mlong, mlat, values, NUTS_NAME) %>%
  distinct()


#install package mapproj

ggplot() +
  geom_polygon(data = country_maps_df, aes(long, lat, group = group, fill = id),
               show.legend = FALSE, color = alpha('black', 1/3), alpha = .6) +
  geom_label_repel(data = county_maps_means_df, aes(mlong, mlat, label = geo)) +
  coord_map() +
  labs(x = "", y ="") +
  theme(axis.text = element_blank())


ggplot() +
  geom_polygon(data = plot_data,
               aes(long, lat, group = group, fill = cat),
               color = alpha('black', 1/3), show.legend = TRUE) +
  geom_label_repel(data = county_maps_means_df,
                   aes(mlong, mlat, label = paste0(geo, ": ", values))) +
  coord_sf(xlim = c(-20,44), ylim = c(30,70)) +
  scale_fill_brewer(palette = 'RdYlBu') +
  labs(title = "Ofiary œmiertelne wypadków drogowych",
       subtitle = "Na podstawie danych Eurostat, stan na koniec 2018 roku",
       x = "", y = "") +
  theme(axis.text = element_blank())