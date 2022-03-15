##Load the data and wrangle. 
df <- read.csv('data/activities.csv')
df <- df |> select(Start.Time, End.Time, Duration..h.m.s., 
                   Activity.Type, Average.Speed..km.h., 
                   Average.Speed..km.h.or.min.km., Max..Speed..km.h., Max..Speed..km.h.or.min.km.,
                   Elevation.Gain..m., Elevation.Loss..m., Max..Heart.Rate..bpm., Average.Heart.Rate..bpm., 
                   Max..Run.Cadence, Avg..Run.Cadence, Calories, VO2max, Stride.Length, Steps)
runs <- df |> filter(Activity.Type == 'Running') |> select(-Activity.Type)

#Format start and end times appropriately. 
runs$Start.Time <- ymd_hms(runs$Start.Time, tz = "America/Los_Angeles")
runs$End.Time <- ymd_hms(runs$End.Time, tz = "America/Los_Angeles")

#Format the duration
runs$Duration..h.m.s. <- as.duration(hms(runs$Duration..h.m.s.))
library(tidyverse)
library(lubridate)

#Function to get graph labels formated at min/km. 
#Taken from: https://stackoverflow.com/questions/52140261/ggplot2-plot-seconds-in-hms-format-in-y-axi/52140888
fmt_ms <- function(x, digits.secs=NULL) {
  if (!is.null(digits.secs)) {
    oopts <- options(digits.secs = digits.secs)
    on.exit(options(oopts), add=TRUE)
  }
  format(as.POSIXct(x, origin="1970-01-01 00:00:00"), format="%M:%S", tz="UTC")
}


#Make the names less unwieldy. 
runs <- rename(runs, Duration = Duration..h.m.s.,
               Avg.Speed = Average.Speed..km.h., 
               Avg.Pace = Average.Speed..km.h.or.min.km.,
               Max.Speed = Max..Speed..km.h., 
               Max.Pace = Max..Speed..km.h.or.min.km.,
               Elevation.Gain = Elevation.Gain..m., 
               Elevation.Loss = Elevation.Loss..m., 
               Max.Heart.Rate = Max..Heart.Rate..bpm., 
               Avg.Heart.Rate = Average.Heart.Rate..bpm., 
               Max.Cadence = Max..Run.Cadence, 
               Avg.Cadence = Avg..Run.Cadence)
#Format the pace
runs$Avg.Pace <- as.duration(ms(runs$Avg.Pace))
runs$Max.Pace <- as.duration(ms(runs$Max.Pace))

#Filter out runs saying max speed was over 30 km/h. 
runs <- filter(runs, Max.Speed < 30, Max.Speed > 7, as.integer(year(Start.Time)) > 2014)

yearly <- aggregate(cbind(Max.Heart.Rate, Avg.Heart.Rate, Max.Cadence, Avg.Cadence, Max.Pace, Avg.Pace) ~ year(Start.Time), data = runs, mean, na.rm = TRUE)
yearly <- rename(yearly, Year = `year(Start.Time)`)

runs$Week <- week(runs$Start.Time)
weekly <- aggregate(cbind(Max.Heart.Rate, Avg.Heart.Rate, Max.Cadence,
                           Avg.Cadence, Max.Pace, Avg.Pace) ~ Week + month(Start.Time) + year(Start.Time),
                            data = runs, mean, na.rm = TRUE) |> slice_tail(n = 12)
weekly$Week <- 1:12

monthly <- aggregate(cbind(Max.Heart.Rate, Avg.Heart.Rate, Max.Cadence,
                           Avg.Cadence, Max.Pace, Avg.Pace) ~  month(Start.Time) + year(Start.Time),
                            data = runs, mean, na.rm = TRUE) |> slice_tail(n = 12)
monthly$Month <- 1:12
  

write.csv(yearly, "data/yearly.csv", row.names = FALSE)

write.csv(monthly, "data/monthly.csv", row.names = FALSE)

write.csv(weekly, "data/weekly.csv", row.names = FALSE)
