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
  count(pickup_date, service) %>% 
  mutate(n = case_when(
    service == "green" ~ n * 5,
    service == "yellow" ~ n * 20,
    TRUE ~ as.double(n)
  ))


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
         ) %>% factor(levels = c("low", "medium", "high"), ordered = TRUE)) %>% 
  
  ggplot(aes(avg_temp, fill = temp_bins)) +
  geom_density() +
  scale_fill_manual(values = c("green" = "chartreuse4", "uber" = "gray14","yellow" =  "darkgoldenrod1")) +
  theme_minimal()

combined_weather_taxi %>% 
  group_by(service, pickup_date) %>% 
  summarise(snowfall = mean(snow_depth), n = n()) %>% 
  mutate(snowfall_yes = if_else(snowfall > 0, "yes", "no"),
         n = case_when(
           service == "green" ~ n * 5,
           service == "yellow" ~ n * 20,
           TRUE ~ as.double(n)
         )) %>% 
  ungroup() %>% 
  filter(!is.na(snowfall_yes)) %>% 
  ggplot(aes(n, col = factor(snowfall_yes))) + 
  geom_density() +
  facet_wrap(service~.)

combined_weather_taxi %>%  
  mutate(temperature_bin = cut(avg_temp, breaks = quantile(avg_temp, seq(0, 1, 0.2)))) %>% 
  select(temperature_bin, everything()) %>% 
  arrange(avg_temp) %>% mutate(temperature_bin = temperature_bin %>% as.factor() ) %>%
  filter(!is.na(temperature_bin)) %>% 
  count(temperature_bin, service) %>% 
  mutate(n = case_when(
           service == "green" ~ n * 5,
           service == "yellow" ~ n * 20,
           TRUE ~ as.double(n)
         )) %>% 
  ggplot(aes(temperature_bin, n, col = service)) +
    geom_line(stat = "identity") +
  scale_color_manual(values = c("green" = "chartreuse4", "uber" = "gray14","yellow" =  "darkgoldenrod1")) +
  labs(x = "precipitation",
       y = "Proportion of cabs")

ggsave("plots/avg_temp_usage.png")


combined_weather_taxi %>%  
  mutate(temperature_bin = cut(snowfall, breaks = quantile(snowfall, seq(0, 1, 0.2)))) %>% 
  select(temperature_bin, everything()) %>% 
  arrange(snowfall) %>% mutate(temperature_bin = temperature_bin %>% as.factor() ) %>%
  filter(!is.na(temperature_bin)) %>% 
  count(temperature_bin, service) %>% 
  mutate(n = case_when(
    service == "green" ~ n * 5,
    service == "yellow" ~ n * 20,
    TRUE ~ as.double(n)
  )) %>% 
  ggplot(aes(temperature_bin, n, fill = service)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("green" = "chartreuse4", "uber" = "gray14","yellow" =  "darkgoldenrod1")) +
  labs(x = "precipitation",
       y = "Proportion of cabs")

ggsave("plots/snowfall_usage.png")


x <- 
combined_weather_taxi %>%  
  mutate(temperature_bin = cut(avg_temp, breaks = quantile(avg_temp, seq(0, 1, 0.2)))) %>% 
  select(temperature_bin, everything()) %>% 
  arrange(avg_temp) %>% mutate(temperature_bin = temperature_bin %>% as.factor() ) %>%
  filter(!is.na(temperature_bin)) %>% 
  count(temperature_bin, service) %>% 
  mutate(n = case_when(
    service == "green" ~ n * 5,
    service == "yellow" ~ n * 20,
    TRUE ~ as.double(n)
  )) %>%
  group_by(temperature_bin) %>% 
  mutate(prop = n/sum(n))

y <- 
  combined_weather_taxi %>% 
  mutate(temperature_bin = cut(avg_temp, breaks = quantile(avg_temp, seq(0, 1, 0.2)))) %>% 
  select(temperature_bin, everything()) %>% 
  arrange(avg_temp) %>% mutate(temperature_bin = temperature_bin %>% as.factor() ) %>%
  filter(!is.na(temperature_bin)) %>% 
  group_by(pickup_date, temperature_bin, service)

y %>% 
  mutate(n = case_when(
    service == "green" ~ n * 5,
    service == "yellow" ~ n * 20,
    TRUE ~ as.double(n)
  ))

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





