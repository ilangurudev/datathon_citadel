# Mo 1

boroData <- read.table("nybb.csv", skip = 1)

#load raw data
weatherData <- read.table("data/weather.csv", header = TRUE, sep=",")
green <- read.table("data/green_trips_new_2.csv", header = TRUE, sep=",")

# Combined data of rides and weather
combined <- as.data.frame(c(green[1:2190,c(1, 3,4,7)]  ,weatherData[,1:7])) 



require(xgboost)


# precipitation range
precip_range = range( combined["precipitation"], na.rm = TRUE)

precipData = as.numeric(complete.cases(combined[c("pickup_datetime", "precipitation")])) 



breaks = c(0, seq(0.5, 1, by = .1))


precip_level <- cut(precipData , breaks =  seq(from=precip_range[1], to=precip_range[2], length.out = 10))

