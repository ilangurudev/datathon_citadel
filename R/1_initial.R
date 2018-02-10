pacman::p_load(tidyverse, rebus, janitor, sp)

uber_2014 <- read_csv("data/uber_trips_2014.csv")
uber_2015 <- read_csv("data/uber_trips_2015.csv")

geography <- read_csv("data/geography.csv")
nta <- read_csv("data/zones.csv")


geography <- 
  geography %>% 
  mutate(coordinate = rep(c("lat", "lon"), nrow(.)/2),
         vertice = rep(1:(nrow(.)/2), each = 2)) %>% 
  gather(nta_code, value, -vertice, -coordinate) %>% 
  spread(coordinate, value) %>% 
  arrange(nta_code, vertice) 



find_nta <- function(lat, lon){
  
  
    locations <- 
      geography %>% 
      split(.$nta_code) %>% 
      map_df(function(x){
        tibble(inside = point.in.polygon(lat, lon, x$lat, x$lon),
               nta = x$nta_code[1])
      })
    
    locations %>% filter(inside == 1) %>% pull(nta)
}

find_nta(lat, lon)



# %>% 
#   group_by(nta_code) %>% 
#   summarise(lat = mean(lat), 
#             lon = mean(lon))



geography