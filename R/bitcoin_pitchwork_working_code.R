#########################################################################################################################
#                                                                                                                       #
#  Title: Pitchfork Analysis
#  Author: Christopher Maerzluft
#  Description: Helps write the code for the pitchfork analysis methods
#  Last Edit: 12/28/17
#                                                                                                                       #
#########################################################################################################################
##### Libraries, Directories, and Other #####
# Clean global environment
rm(list = ls(all = TRUE))
gc()
options(stringsAsFactors = FALSE, scipen = 999)
work.dir <- "~/Desktop/Miscellaneous/Data Blog/Cryptocurrency/"
data.dir <- "Data"
scrp.dir <- "Webscraping Data"
date <- gsub("-", "", Sys.Date())
datetime <- paste(gsub("-", "", Sys.Date()), format(Sys.time(), "%H%M"), sep = "_")
setwd(work.dir)
library(ggplot2)
library(lubridate)

#########################################################################################################################
##### Load Webscraping Data #####
data.files <- list.files(paste(data.dir, scrp.dir, sep = "/"))
data.file.dates <- regexpr("[0-9]+_[0-9]+", data.files)
date.time <- substr(data.files, 
                    start = as.numeric(data.file.dates), 
                    stop = as.numeric(data.file.dates) + attributes(data.file.dates)$match.length - 1)
# start and stop locations of the date and time are known beforehand (you can also print date.time to see them)
date <- as.numeric(substr(date.time, start = 1, stop = 8))
time <- as.numeric(substr(date.time, start = 10, stop = 13))
most.recent.download <- date == max(date) & grepl("mktcap_all_historical_data", data.files)
if (sum(most.recent.download) > 1) {
  most.recent.download <- most.recent.download & time == max(time[most.recent.download])
}
crypto <- read.csv(paste(data.dir, scrp.dir, data.files[most.recent.download], sep = "/"))
crypto$date.comp <- as.Date(crypto$date.comp, format = "%Y-%m-%d")
crypto <- crypto[!is.na(crypto$Market.Cap), ]
# Set up information for later
crypto$day_loss <- crypto$Open > crypto$Close

##### Find Peaks and Troughs #####
bitcoin <- crypto[crypto$name.full == "bitcoin", ]
bitcoin <- bitcoin[year(bitcoin$date.comp) <= 2016, ]
ggplot(bitcoin, aes(x = date.comp))+
  geom_linerange(aes(ymin = Low, ymax = High)) +
  geom_rect(aes(xmin = date.comp - 1/2*0.9, xmax = date.comp + 1/2*0.9, 
                ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = day_loss)) + 
  guides(fill = FALSE, colour = FALSE) + 
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "darkgreen"))

# setup search ranges
x <- 5
valid.dates <- seq(bitcoin$date.comp[bitcoin$date.comp == (min(bitcoin$date.comp) + x)],
                   bitcoin$date.comp[bitcoin$date.comp == (max(bitcoin$date.comp) - x)], by = 1)
bitcoin <- bitcoin[order(bitcoin$date.comp), ]
rownames(bitcoin) <- NULL

# The peaks are defined as having the largest High for the previous x periods and the next x periods
bitcoin$peak <- c(
  rep(FALSE, x),
  do.call(c, lapply(valid.dates, function(date, range, data) {
    date.row <- which(data$date.comp == date)
    comp.dates <- (date.row - range):(date.row + range)
    date.high <- data$High[date.row]
    comp.highs <- data$High[comp.dates]
    date.high == max(comp.highs)
  }, range = x, data = bitcoin)),
  rep(FALSE, x)
)
# The lows are defined as having the smallest Low for the previous x periods and the next x periods
bitcoin$valley <- c(
  rep(FALSE, x),
  do.call(c, lapply(valid.dates, function(date, range, data) {
    date.row <- which(data$date.comp == date)
    comp.dates <- (date.row - range):(date.row + range)
    date.low <- data$Low[date.row]
    comp.lows <- data$Low[comp.dates]
    date.low == min(comp.lows)
  }, range = x, data = bitcoin)),
  rep(FALSE, x)
)
PickOut <- bitcoin$peak == TRUE & bitcoin$valley == TRUE
bitcoin$peak[PickOut] <- FALSE
bitcoin$valley[PickOut] <- FALSE
# 
ggplot(bitcoin, aes(x = date.comp))+
  geom_linerange(aes(ymin = Low, ymax = High)) +
  geom_rect(aes(xmin = date.comp - 1/2*0.9, xmax = date.comp + 1/2*0.9, 
                ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = day_loss)) + 
  geom_point(data = bitcoin[bitcoin$peak == TRUE, ], aes(x = date.comp, y = High), color = "green") +
  geom_point(data = bitcoin[bitcoin$valley == TRUE, ], aes(x = date.comp, y = Low), color = "red") +
  guides(fill = FALSE, colour = FALSE) + 
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "darkgreen"))



