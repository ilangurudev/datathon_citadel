pacman::p_load(tidyverse, rebus, janitor, sp, lubridate, hms)

uber_2014 <- read_csv("data/uber_trips_2014.csv")
uber_2015 <- read_csv("data/uber_trips_2015.csv")
green_taxis <- read_csv("data/green_trips.csv")
geography <- read_csv("data/geography.csv")
zones <- read_csv("data/zones.csv")
demographics <- read_csv("data/demogaphics.csv")


zones <- 
  zones %>% 
  rename(zone_id = location_id)

geography <- 
  geography %>% 
  mutate(coordinate = rep(c("lat", "lon"), nrow(.)/2),
         vertice = rep(1:(nrow(.)/2), each = 2)) %>% 
  gather(nta_code, value, -vertice, -coordinate) %>% 
  spread(coordinate, value) %>% 
  arrange(nta_code, vertice) 

find_nta_code <- function(lat, lon){
  
  
    locations <- 
      geography %>% 
      split(.$nta_code) %>% 
      map_df(function(x){
        tibble(inside = point.in.polygon(lat, lon, x$lat, x$lon),
               nta = x$nta_code[1])
      })
    
    nta <- locations %>% filter(inside == 1) %>% pull(nta)
    
    if(length(nta) > 0){
      nta
    } else {
      NA
    }
}

uber_2014 <- 
  uber_2014 %>% 
    mutate(pickup_datetime = mdy_hm(pickup_datetime),
           pickup_date = as.Date(pickup_datetime),
           pickup_hour = hour(pickup_datetime),
           nta_code = map2_chr(pickup_latitude, pickup_longitude, find_nta_code)) %>% 
  left_join(zones)

uber_2015 <- 
  uber_2015 %>%
    mutate(pickup_datetime = mdy_hm(pickup_datetime),
           pickup_date = as.Date(pickup_datetime),
           pickup_hour = hour(pickup_datetime)) %>% 
    rename(zone_id = pickup_location_id) %>% 
    left_join(zones)
  
green_taxis <-     
  
  



