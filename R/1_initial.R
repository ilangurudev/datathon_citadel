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

weather_1 <- 
  weather %>% 
  mutate(pickup_date = mdy(date)) %>% 
  group_by(pickup_date) %>% 
  summarise_if(is.numeric,mean, na.rm = T)

combined_weather_taxi <- 
  combined %>% 
  left_join(weather_1) %>% 
  select(-latitude, - longitude)


count_days <- 
  combined_weather_taxi %>% 
  count(pickup_date, service)


count_days %>% 
  ggplot(aes(pickup_date, n,  fill = service)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("green" = "chartreuse4", "uber" = "gray14","yellow" =  "darkgoldenrod1")) +
  theme_minimal()

ggsave("plots/service_date.png")
  

count_days %>% 
  ggplot(aes(pickup_date, n,  fill = service)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("green" = "chartreuse4", "uber" = "gray14","yellow" =  "darkgoldenrod1")) +
  theme_minimal()

ggsave("plots/service_date_fill.png")

count_days %>%
  mutate(year = year(pickup_date)) %>% 
  ggplot(aes(pickup_date, n,  col = service)) +
  geom_line(stat = "identity") +
  scale_color_manual(values = c("green" = "chartreuse4", "uber" = "gray14","yellow" =  "darkgoldenrod1")) +
  theme_minimal()

ggsave("plots/service_date_line.png")



combined_weather_taxi %>%
  mutate(year = year(pickup_date),
         temp_bins = case_when(
           avg_temp <= quantile(avg_temp, .25) ~ "low",
           avg_temp <= quantile(avg_temp, .75) ~ "medium",
           TRUE ~ "high"
         ) %>% fct_reorder2(low, medium, high)) %>% 
  count(year, service, temp_bins) %>% 
  ggplot(aes(n, fill = temp_bins)) +
  geom_density() +
  facet_grid(service~.) +
  scale_fill_manual(values = c("green" = "chartreuse4", "uber" = "gray14","yellow" =  "darkgoldenrod1")) +
  theme_minimal()


  
# distinct_weather_locs <- 
#   weather %>% 
#   select(latitude, longitude, location) %>% 
#   distinct() %>% 
#   mutate(location_abc = c("A","B","C"))
# 
# weather <- 
#   weather %>% 
#   left_join(distinct_weather_locs)
# # 
# calc_location_abc <-function(lat, lon, loc_id = NA){
#   if(is.na(loc)){
#     distinct_weather_locs %>% 
#       mutate(distance_abc = map2_dbl(latitude, longitude, function(x, y){
#         ((x - lat)^2 + (y-lon)^2)^(0.5)
#       })) %>% 
#       filter(distance_abc == min(distance_abc)) %>% 
#       slice(1) %>% 
#       pull(location_abc)
#   } else {
#     zones %>% 
#       filter(zone_id == loc) %>% 
#       left_join(geography, by = "nta_code") %>% 
#       summarise(latitude = mean(lat), )
#     
#     
#   }
#   
# } 


# taxi_weather_2014 <- 
#   combined %>%
#   filter(!is.na(pickup_latitude)) %>%
#   filter(year(pickup_date) == 2014) %>% 
#   sample_n(1000000) %>% 
#   mutate(location_abc = map2_chr(pickup_latitude, pickup_longitude, possibly(calc_location_abc, NA))) %>% 
#   left_join(weather)
# 
# taxi_weather_2015 <- 
#   combined %>%
#   filter(!is.na(pickup_latitude)) %>%
#   filter(year(pickup_date) == 2015) %>%
#   sample_n(1000000) %>% 
#   mutate(location_abc = map2_chr(pickup_latitude, pickup_longitude, possibly(calc_location_abc, NA))) %>% 
#   left_join(weather)

combined %>% write_csv("data/combined_trip.csv")






# 
# mta_1 <- 
#   mta %>% 





