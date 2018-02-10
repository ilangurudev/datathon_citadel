pacman::p_load(tidyverse, rebus, janitor)

uber_2014 <- read_csv("data/uber_trips_2014.csv")
uber_2015 <- read_csv("data/uber_trips_2015.csv")

geography <- read_csv("data/geography.csv")
nta <- read_csv("data/zones.csv")


geography %>% 
  mutate(coordinate = rep(c("lat", "lon"), nrow(.)/2),
         vertice = rep(1:(nrow(.)/2), each = 2)) %>% 
  gather(nta_code, value, -vertice, -coordinate) %>% 
  spread(coordinate, value) %>% 
  arrange(nta_code, vertice) 



# %>% 
#   group_by(nta_code) %>% 
#   summarise(lat = mean(lat), 
#             lon = mean(lon))



geography