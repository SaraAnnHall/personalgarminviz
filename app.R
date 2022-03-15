library(tidyverse)
library(lubridate)
library(plotly)
library(dash)


yearly <- read_csv("data/yearly.csv")
yearly <- rename(yearly, Date = Year)

monthly <- read_csv("data/monthly.csv")
monthly <- rename(monthly, Date = Month)

weekly <- read_csv("data/weekly.csv")
weekly <- rename(weekly, Date = `Week`)


data <- list("yearly" = yearly, "monthly" = monthly, "weekly" = weekly)
dataLabels <- list("yearly" = yearly$Date, "monthly" = paste(month.abb[monthly$Date], monthly$`year(Start.Time)`), 
                   "weekly" = c("This week", "Last Week", "-2 Weeks", "-3 Weeks", "-4 Weeks", "-5 Weeks",
                                "-6 Weeks", "-7 Weeks", "-8 Weeks", "-9 Weeks", "-10 Weeks", "-11 Weeks"))

paste(month.abb[monthly$Date], monthly$`year(Start.Time)`)


#Function to get graph labels formated at min/km. 
#Taken from: https://stackoverflow.com/questions/52140261/ggplot2-plot-seconds-in-hms-format-in-y-axi/52140888
fmt_ms <- function(x, digits.secs=NULL) {
  if (!is.null(digits.secs)) {
    oopts <- options(digits.secs = digits.secs)
    on.exit(options(oopts), add=TRUE)
  }
  format(as.POSIXct(x, origin="1970-01-01 00:00:00"), format="%M:%S", tz="UTC")
}
## Subtitle code from https://rpubs.com/bcd/subplot-titles. 
# font style
f <- list(
  family = "verdana",
  size = 18,
  color = "black")

# annotations
a <- list(
  text = "Max Heart Rate",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

b <- list(
  text = "Average Heart Rate",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

c <- list(
  text = "Max Cadence",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

d <- list(
  text = "Average Cadence",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)


e <- list(
  text = "Max Pace",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

f <- list(
  text = "Average Pace",
  font = f,
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)


makePlots <- function(df, selection){
  mhr <- ggplotly(ggplot(df, aes(x = Date, y = Max.Heart.Rate)) +
    geom_point(stat = 'summary', fun = mean, color = "#AB0E86") +
    geom_line(stat = 'summary', fun = mean, color = "#AB0E86") +
    scale_x_discrete(labels= dataLabels[selection])
    
    ) |> layout(annotations = a)
  ahr <- ggplotly(ggplot(df, aes(x = Date, y = Avg.Heart.Rate)) +
    geom_line(stat = 'summary', fun = mean, color = "#AB0E86") + 
    geom_point(stat = 'summary', fun = mean, color = "#AB0E86")
    ) |> layout(annotations = b)
  mca <- ggplotly(ggplot(df, aes(x = Date, y = Max.Cadence)) +
    geom_line(stat = 'summary', fun = mean, color = "#59057B") + 
    geom_point(stat = 'summary', fun = mean, color = "#59057B")
    ) |> layout(annotations = c)
  aca <- ggplotly(ggplot(df, aes(x = Date, y = Avg.Cadence)) +
    geom_line(stat = 'summary', fun = mean, color = "#59057B") + 
    geom_point(stat = 'summary', fun = mean, color = "#59057B")
    ) |> layout(annotations = d)
  mpa <- ggplotly(ggplot(df, aes(x = Date, y = Max.Pace)) +
    geom_line(stat = 'summary', fun = mean, color = "#0F0766") + 
    geom_point(stat = 'summary', fun = mean, color = "#0F0766")  +
    scale_y_continuous(labels=fmt_ms)
    ) |> layout(annotations = e)
  apa <- ggplotly(ggplot(df, aes(x = Date, y = Avg.Pace)) +
    geom_line(stat = 'summary', fun = mean, color = "#0F0766") + 
    geom_point(stat = 'summary', fun = mean, color = "#0F0766") +
    scale_y_continuous(labels=fmt_ms)
    ) |> layout(annotations = f)
  
  p <- subplot(mhr, ahr, mca, aca, mpa, apa, nrows = 3, shareX = TRUE, titleX = FALSE)

}


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccRadioItems(
        id = "period-select",
        options = list(list(label = "Last 7 Years", value = "yearly"),
                       list(label = "Last 12 Months", value = "monthly"), 
                       list(label = "Last 12 Weeks", value = "weekly")),
        value = 'yearly',
        labelStyle = list(display = "inline-block")
      ),
      dccGraph(id='plot-area')
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('period-select', 'value')),
  function(period) {
    makePlots(data[[period]])
  }
)


app$run_server(host = '0.0.0.0')