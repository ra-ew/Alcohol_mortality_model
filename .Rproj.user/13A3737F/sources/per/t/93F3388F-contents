library(sf) #st_as_sf
library(rnaturalearth)
library(hrbrthemes)
library(RColorBrewer)

final_data_imputed <- read_csv("final_data.csv", 
                       col_types = cols(X1 = col_skip()))

data_viz <- final_data_imputed %>% 
  group_by(COUNTRY) %>% 
  summarize(mean_beer = mean(BEER),
            mean_other = mean(OTHER),
            mean_spirits = mean(SPIRITS),
            mean_wine = mean(WINE),
            mean_mortality = mean(MORTALITY), 
            mean_income = mean(INCOME),
            mean_unemp = mean(UNEMP)) %>% 
  rename(sovereignt = COUNTRY)
View(data_viz)

#creating a map - based on https://bhaskarvk.github.io/user2017.geodataviz/notebooks/02-Static-Maps.nb.html
europe <- st_as_sf(rnaturalearth::countries110) %>% 
  filter(region_un=="Europe" & name!='Russia')

europe_bbox <- st_polygon(list(
  matrix(c(-25,29,45,29,45,75,-25,75,-25,29),byrow = T,ncol = 2)))

europe_clipped <- suppressWarnings(st_intersection(europe, st_sfc(europe_bbox, crs=st_crs(europe))))%>% 
  left_join(data_viz, by = c("sovereignt"))

#MAP 1 - mean income
ggplot(europe_clipped, aes(fill=mean_income)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  hrbrthemes::theme_ipsum_rc() +
  labs(x=NULL, y=NULL, title=NULL)+
  scale_fill_gradient(low = "#CDDEE2", high = "#3E5F68", name="Średnie realne \nwynagrodzenie")

#MAP 2 - mean unemployment
ggplot(europe_clipped, aes(fill=mean_unemp)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  hrbrthemes::theme_ipsum_rc() +
  labs(x=NULL, y=NULL, title=NULL)+
  scale_fill_gradient(low = "#FAC1C0", high = "#EB1712", name="Średni wskaźnik \nbezrobocia")

#MAP 3 - mean mortality
ggplot(europe_clipped, aes(fill=mean_mortality)) +
  geom_sf(alpha=0.8,col='white') +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  hrbrthemes::theme_ipsum_rc() +
  labs(x=NULL, y=NULL, title=NULL)+
  scale_fill_gradient(low = "#E9A4F0", high = "#540F5B", name="Średni wskaźnik \nśmiertelności")
