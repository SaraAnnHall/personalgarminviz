library(tidyverse)
library(lubridate)
library(MASS)
library(mclust)

df <- read.csv('data/project/2022-03-11_garmin_connect_export/activities.csv')
df <- df |> dplyr::select(Start.Time, End.Time, Duration..h.m.s., 
                   Activity.Type, Average.Speed..km.h., 
                   Average.Speed..km.h.or.min.km., Max..Speed..km.h., Max..Speed..km.h.or.min.km.,
                   Elevation.Gain..m., Elevation.Loss..m., Max..Heart.Rate..bpm., Average.Heart.Rate..bpm., 
                   Max..Run.Cadence, Avg..Run.Cadence, Calories, VO2max, Stride.Length, Steps)
runs <- df |> dplyr::filter(Activity.Type == 'Running') |> dplyr::select(-Activity.Type)

#Format start and end times appropriately. 
runs$Start.Time <- ymd_hms(runs$Start.Time, tz = "America/Los_Angeles")
runs$End.Time <- ymd_hms(runs$End.Time, tz = "America/Los_Angeles")

#Format the duration
runs$Duration..h.m.s. <- as.duration(hms(runs$Duration..h.m.s.))

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
runs <- filter(runs, Max.Speed < 30, Max.Speed > 7, Avg.Speed > 7, as.integer(year(Start.Time)) > 2014)

Expected.Max.Heart.Rate <- 220 - (as.integer(year(runs$Start.Time)) - 1998)

#Making some plots. 
ggplot(runs, aes(x = year(Start.Time), y = Avg.Heart.Rate)) +
  geom_line(stat = 'summary', fun = mean) + 
  geom_point(stat = 'summary', fun = mean)

format(as.Date(df$Date), "%Y-%m")

ggplot(runs, aes(x = Start.Time, y = Avg.Heart.Rate)) +
  geom_point()

ggplot(runs, aes(x = year(Start.Time), y = Max.Heart.Rate)) +
  geom_line(stat = 'summary', fun = mean) + 
  geom_point(stat = 'summary', fun = mean)

ggplot(runs, aes(year(Start.Time))) + 
  geom_line(stat = 'summary', fun = mean, aes(y = Max.Heart.Rate, colour = "Max.Heart.Rate")) + 
  geom_line(aes(y = Expected.Max.Heart.Rate, colour = "Expected.Max.Heart.Rate"))

ggplot(runs, aes(x = year(Start.Time), y = Avg.Cadence)) +
  geom_line(stat = 'summary', fun = mean) + 
  geom_point(stat = 'summary', fun = mean)

ggplot(runs, aes(x = year(Start.Time), y = VO2max)) +
  geom_line(stat = 'summary', fun = mean) + 
  geom_point(stat = 'summary', fun = mean)

ggplot(runs, aes(x = year(Start.Time), y = Stride.Length)) +
  geom_line(stat = 'summary', fun = mean) + 
  geom_point(stat = 'summary', fun = mean)

#Function to get graph labels formated at min/km. 
fmt_ms <- function(x, digits.secs=NULL) {
  if (!is.null(digits.secs)) {
    oopts <- options(digits.secs = digits.secs)
    on.exit(options(oopts), add=TRUE)
  }
  format(as.POSIXct(x, origin="1970-01-01 00:00:00"), format="%M:%S", tz="UTC")
}

p1 <- ggplot(runs, aes(x = year(Start.Time), y = Avg.Pace)) +
  geom_line(stat = 'summary', fun = mean, color = "#AB0E86") +
  scale_y_continuous(labels=fmt_ms)

p2 <- ggplot(runs, aes(x = year(starts), y = Max.Pace)) +
  geom_line(stat = 'summary', fun = mean) +
  scale_y_continuous(labels=fmt_ms)

p1+p2

#Looking at durations. 
dseconds(mean(runs$Duration))
dseconds(mean(runs$Duration))

#Look at pace
dseconds(mean(runs$Avg.Pace, na.rm = TRUE))

##Now, from these plots It's clear to me that there are some issues. I have no way of knowing
##which runs were workouts and which were just aerobic, which would make a difference in how
##I analyze them. I want to see if I can somehow pick groups out of the data even though I have
##No record of types of workout (so no response variable. )

# Models:

#Note: ran first just filtering with max speed between 7 and 30 km/H. When I ran clustering,
#I ended up with just a few observations on on branch, and all the rest on another. Looking at these
#observations, I found the average pace was ridiculously slow. As a result, I removed all "runs" with average pace less
#than 7 km/h, since these are probably not actually runs. Now the following suggests that there are
#clearly 2 groups in the data. 

runs2 <- runs|> dplyr::select(Max.Heart.Rate, Avg.Heart.Rate, Max.Cadence,
                      Avg.Cadence, Max.Pace, Avg.Pace)

eucdist <- dist(runs2)
clus1 <- hclust(eucdist, method="complete")
plot(clus1)

##Next, I'll plot a color coded pairwise matrix to see if anything stands out between these two groups. 
clusters <- cutree(clus1, 2)

pairs(runs2, col = clusters)
runs2$clusters <- clusters

group1 <- runs2 |> filter(clusters == 1)
group2 <- runs2 |> filter(clusters == 2)

#Looking at the max paces, it's possible one cluster corresponds to workouts and the other to 
#hard runs, though I'm suspicious looking at the pair plots, where there is a very clear boundary that
#doesn't make visual sense. 
dseconds(mean(group1$Max.Pace))
dseconds(mean(group2$Max.Pace))

## What happens if I do a mixture model instead?
runs2 <- runs|> dplyr::select(Max.Heart.Rate, Avg.Heart.Rate, Max.Cadence,
                              Avg.Cadence, Max.Pace, Avg.Pace) |> drop_na()
mruns <- Mclust(runs2, G=1:12)
pairs(runs2, col = mruns$classification)

# Trying different values of G, the model seems to lean toward 8 components. Let's looks at the means across these groups.
runs2$clusters <- mruns$classification
runs2 |>
  group_by(clusters) |>
  summarise_at(vars("Max.Heart.Rate", "Avg.Heart.Rate", "Max.Cadence",
                    "Avg.Cadence", "Max.Pace", "Avg.Pace"), mean)

#I imagine one issue I'm having is that while I removed the date column, runs closer to eachother in time are probably more similar. 
#I also switch watches in 2017 which might affect data quality. 

#What happens if I run mclust on just data from the past year?

runs2 <- runs|> filter(as.integer(year(Start.Time)) == 2021) |> dplyr::select(Max.Heart.Rate, Avg.Heart.Rate, Max.Cadence,
                              Avg.Cadence, Max.Pace, Avg.Pace) |> drop_na()
mruns <- Mclust(runs2, G=1:12)
pairs(runs2, col = mruns$classification)

# Trying different values of G, the model seems to lean toward 8 components. Let's looks at the means across these groups.
runs2$clusters <- mruns$classification
sum_runs<- runs2 |>
  group_by(clusters) |>
  summarise_at(vars("Max.Heart.Rate", "Avg.Heart.Rate", "Max.Cadence",
                    "Avg.Cadence", "Max.Pace", "Avg.Pace"), mean)

sum_runs$Avg.Pace <- dseconds(sum_runs$Avg.Pace)
sum_runs$Max.Pace <- dseconds(sum_runs$Max.Pace)

##What if I just look at the last 6 months?

##Last thing I'm going to try is running a PCA. 