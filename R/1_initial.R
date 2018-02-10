pacman::p_load(tidyverse, rebus, janitor, sp, lubridate, hms)

uber_2014 <- read_csv("data/uber_trips_2014.csv")
uber_2015 <- read_csv("data/uber_trips_2015.csv")
green_taxis <- read_csv("data/green_trips_new_2.csv")
yellow_taxis <- read_csv("data/yellow_trips_new.csv")
geography <- read_csv("data/geographic.csv")
zones <- read_csv("data/zones.csv")
demographics <- read_csv("data/demographics.csv")
weather <- read_csv("data/weather.csv")


zones <- 
  zones %>% 
  rename(zone_id = location_id)

geography <- 
  geography %>% 
  mutate(coordinate = rep(c("lon", "lat"), nrow(.)/2),
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
    
    nta <- locations %>% filter(inside != 0) %>% pull(nta)
    # print(nta)
    
    if(length(nta) > 0){
      nta
    } else {
      NA
    }
}

# nta_code = map2_chr(pickup_latitude, pickup_longitude, find_nta_code)
uber_2014_1 <- 
  uber_2014 %>%
    mutate(pickup_date = pickup_datetime %>% str_extract(START %R% one_or_more(or(DGT, PUNCT)) %R% SPACE) %>% str_trim(),
           pickup_hour = pickup_datetime %>% str_replace(pickup_date, "") %>% str_extract(one_or_more(DIGIT)) %>% as.integer(),
           pickup_date = mdy(pickup_date),
           service = "uber") %>%
  select(-base, -pickup_datetime)
  

uber_2015_1 <- 
  uber_2015 %>%
    mutate(pickup_date = as.Date(pickup_datetime),
           pickup_hour = hour(pickup_datetime),
           service = "uber") %>% 
    rename(zone_id = pickup_location_id) %>%
    select(-pickup_datetime, -dispatch_base, -affiliate_base)

green_taxis_1 <-
  green_taxis %>% 
  mutate(pickup_date = as.Date(pickup_datetime),
         pickup_hour = hour(pickup_datetime),
         service = "green") %>% 
  select(-total_amount, -trip_distance, -passenger_count, -starts_with("dropoff"), -pickup_datetime)
  
yellow_taxis_1 <-
  yellow_taxis %>% 
  mutate(pickup_date = as.Date(pickup_datetime),
         pickup_hour = hour(pickup_datetime),
         service = "yellow") %>% 
  select(-total_amount, -trip_distance, -passenger_count, -starts_with("dropoff"), -pickup_datetime)

combined <- bind_rows(uber_2014_1, uber_2015_1, yellow_taxis_1, green_taxis_1)

weather <- 
  weather %>% 
  mutate(pickup_date = mdy(date)) 
  
distinct_weather_locs <- 
  weather %>% 
  select(latitude, longitude, location) %>% 
  distinct() %>% 
  mutate(location_abc = c("A","B","C"))

weather <- 
  weather %>% 
  left_join(distinct_weather_locs)

calc_location_abc <-function(lat, lon){
  distinct_weather_locs %>% 
    mutate(distance_abc = map2_dbl(latitude, longitude, function(x, y){
      ((x - lat)^2 + (y-lon)^2)^(0.5)
    })) %>% 
    filter(distance_abc == min(distance_abc)) %>% 
    slice(1) %>% 
    pull(location_abc)
} 


combined %>% 
 sample_n(10) %>% 
  filter(!is.na(pickup_latitude)) %>% 
  mutate(location_abc = map2_chr(pickup_latitude, pickup_longitude, possibly(calc_location_abc, NA)))



combined %>% write_csv("data/combined_trip.csv")






# 
# mta_1 <- 
#   mta %>% 





