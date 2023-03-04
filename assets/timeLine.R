library("tidyverse")
library("plotly")
library("forecast")
library("patchwork")
library("tidyr")
library("lubridate")
library("ggridges")
library("viridis")
library("GGally")
library("ggExtra")
library("ggplot2")
library("timelineS")
library("maps")
library("ggthemes")


# detaching helps to clear environment 
detach()
rm(list = ls(all.names = TRUE))

# store today date
today <- as.Date(Sys.time())
# extract year as integer
year <- as.integer(format(today, "%Y"))

#create dataframe with timeline properties

years <- seq(2013, as.numeric(format(today, "%Y")), by = 1)

career <- data.frame(
  job_title = c("Field Service Engineer", "Slurry Pumps Engineer",
                "Product Manager"),
  company = c("Metso", "Metso", "Metso"),
  country = c("Mexico", "Mexico", "Mexico"),
  start_date = c("2013-10-01", "2016-10-01", "2017-08-01"),
  end_date = c("2016-10-01", "2017-08-01", "2019-09-01")
)

#convert to date time format
career$start_datePOS <- ymd(career$start_date) 
career$end_datePOS <- ymd(career$end_date) 
#difference between days
career$diffdays <-  career$end_datePOS - career$start_datePOS
#mid date between installation and removal
career$midDate <- (career$diffdays /2) + career$start_datePOS

# plot career timeline using ggplot2

career %>%
  mutate(start_datePOS = as.Date(start_datePOS),
         end_datePOS = as.Date(end_datePOS)) %>%
  arrange(start_datePOS) %>%
  ggplot() +
  geom_segment(aes(x = start_datePOS, xend = end_datePOS, y = job_title, yend = job_title),
               size = 3, alpha = 0.5, show.legend = FALSE,
               color = "#D9D9D9") +
  labs(title = "Career Timeline", x = "", y = "") +
  geom_point(aes(x = start_datePOS, y = job_title), size = 3, color = "#1a9641") +
  geom_point(aes(x = end_datePOS, y = job_title), size = 3, color = "#d7191c") +
  geom_label(aes(x = midDate, y = job_title, label = company), color = "black",
             size = 3, vjust = -0.5, hjust = 0.5, fill = "white",
             label.padding = unit(0.2, "lines"), label.size = 0.1, show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  annotate(geom="text", x=today, y=min(career$job_title),
           label=paste("Updated:", format(today, "%b %d, %Y")), color="black", size=3, vjust=5, hjust=-.1) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "#444653")) +
  theme_bw()


