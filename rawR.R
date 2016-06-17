setwd("/Users/mattias/Documents/OneDrive/Data science/Assignments/week2 reproducable research/Reproducible Research Course Project 1")

install.packages("downloader")
library(downloader)
library(tidyr)
library(dplyr)
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./")
d <- read.csv("activity.csv")
str(d)
tail(d)
hist(dd$steps)

dd <- d %>% 
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarise(no_of_steps = sum(steps))

hist(dd$no_of_steps, main = "Total Steps", xlab = "No of Steps", col = "blue")
plot(dd$date,dd$no_of_steps)
as.integer(mean(dd$no_of_steps))
?round
median(dd$no_of_steps)

# del 2

dd <- d %>% 
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarise(no_of_steps = mean(steps))

as.integer(mean(d$steps, na.rm=TRUE))
plot(dd$interval, dd$no_of_steps, type = "l", main = "Average No. of Steps per Interval", xlab = "Interval", ylab ="No. of Steps", col = "blue")

max_interval <- dd[which.max(dd$no_of_steps),]
max_interval$interval
average_steps <- as.integer(mean(d$steps, na.rm=TRUE))

# del 3
is.na(d$steps)

t <- sum(is.na(d$steps))
t

d_na <- d
d_na$steps[which(is.na(d_na$steps))] <-as.integer(mean(d$steps, na.rm=TRUE))
head(d_na)
install.packages("lubridate")
library(lubridate)

mydate <- weekdays(ymd(as.character(d_na$date)),abbreviate = TRUE)
tt <- factor(ifelse(mydate %in% c("Mån", "Tis", "Ons", "Tor", "Fre"), "weekday" , "weekend"))

str(tt)
d_na$test <- tt
tail(d_na, 20)


dd_na <- d_na %>%
  mutate(wd = weekdays(ymd(as.character(date)),abbreviate = TRUE), wtype = factor(ifelse(wd %in% c("Mån", "Tis", "Ons", "Tor", "Fre"), "weekday" , "weekend"))) %>%
  #mutate(wd = ifelse(weekdays(ymd(as.character(date)),abbreviate = TRUE) %in% c("Mån", "Tis", "Ons", "Tor", "Fre"), "weekday" , "weekend"))) %>%
  group_by(wtype, interval) %>%
  summarise(no_of_steps = mean(steps))

dd_na

library(ggplot2)
g <- ggplot(data=dd_na, aes(x=interval, y=no_of_steps)) 
g <- g +  geom_line(color="blue")
g <- g + facet_wrap( ~ wtype, ncol = 1)
g <- g + ggtitle("TEST")
g <- g + xlab("Interval")
g <- g + ylab("No of Steps")
g
g <- g + theme(axis.text.x=element_blank())
g <- g + theme(legend.position = "bottom")
g <- g + theme(strip.text.x = element_text(colour = "blue", angle = 35, size = 8, hjust = 0.5, vjust = 0.5))
g 
