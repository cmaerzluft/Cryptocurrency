#########################################################################################################################
#                                                                                                                       #
#  Title: Cryptocurrency Exploration
#  Author: Christopher Maerzluft
#  Description: Produces initial exploration of data produced by crypto_web_scrape.R
#  Last Edit: 9/22/17
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
#                                                                                                                       #
# This is pretty straight foward. I simply search the file names in the data directory and then pull the most recent    #
# file. Clean up the date and remove some weak observations (weak being missing Market Capitalization) and Viola.       #
#                                                                                                                       #
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

##### Market Cap #####
#########################################################################################################################
#                                                                                                                       #
# Information on what Market Capitalization is can be found here:                                                       #
# https://www.fidelity.com/learning-center/trading-investing/fundamental-analysis/understanding-market-capitalization   #
#                                                                                                                       #
#########################################################################################################################
PickOut <- crypto$date.comp == max(crypto$date.comp, na.rm = TRUE)
crypto.last <- crypto[PickOut, ]
# When we plot the histogram of the log(Market.Cap) it appears to be bimodal. This to me indicates that there might be
# two different types of currency. I wonder if you were to split them, if they would predict success. Also I am interested
# in seeing how this changes over time.
ggplot() +
  geom_histogram(data = crypto.last, aes(x = Market.Cap), binwidth = max(crypto.last$Market.Cap)/60)

ggplot() +
  geom_histogram(data = crypto.last, aes(x = log(Market.Cap)), binwidth = max(log(crypto$Market.Cap))/60)+
  scale_x_continuous(labels = function(x) {scales::dollar(exp(x))}) +
  theme(plot.background = element_rect(colour = "white"))
# To do: Solve looping plot capability

# top 15 is based on last market cap
crypto.top <- crypto.last[order(crypto.last$Market.Cap, decreasing = TRUE), ]
crypto.top <- crypto.top$name.full[1:10]
crypto.top <- crypto[crypto$name.full %in% crypto.top, ]
crypto.top$year <- year(crypto.top$date.comp)
crypto.top <- crypto.top[year(crypto.top$date.comp) == 2017, ]

ggplot() +
  geom_line(data = crypto.top, aes(x = date.comp, y = log(Market.Cap), group = name.full, color = name.full))

top.minus.bit <- crypto.top[crypto.top$name.full != "bitcoin", ]
ggplot() +
  geom_line(data = top.minus.bit, aes(x = date.comp, y = log(Market.Cap), group = name.full, color = name.full))

##### Day to Day #####
# Could use tidyquant
# Export photos at 1600 x 800... the distance between x points depends on the size of the plot... once exported this would be fixed
# Use y = mx + b to find formulat for size when photo is 1600 x 800
#########################################################################################################################
#                                                                                                                       #
# The function bar_size() comes from manually selecting the size the bars should be at the dimensions of 1600 x 800     #
# using Rstudio's export function (I believe this is in pixels - probably need to resolve for pdf output). It assumes   #
# the x is inversely related to y and then solves for m and b in the equation y = m(1/x) + b. We used the following     #
# two sets of values to solve.                                                                                          #
# 1) y = 16.0, x = 030                                                                                                  #
# 2) y = 02.5, x = 195                                                                                                  #
# x is the number of individual x values (or dates that are being plotted). y is the bar size we liked the best.        #
#                                                                                                                       #
#########################################################################################################################
bar_size <- function(x) {
  (5265/11)*(1/x) + (1/22)
}
coin <- crypto[crypto$name.full == "ripple" & year(crypto$date.comp) >= 2017 & month(crypto$date.comp) >= 6, ]
ggplot(coin) +
  geom_errorbar(aes(x = date.comp, ymin = Low, ymax = High, color = day_loss), 
                size = 0.5, width = 0.9) +
  geom_errorbar(aes(x = date.comp, ymin = Open, ymax = Close, color = day_loss), 
                size = bar_size(length(unique(coin$date.comp))), width = 0) +
  scale_color_manual(name = "Daily Gain", values = c("dark green", "dark red"), labels = c("Yes", "No"))
# https://stackoverflow.com/questions/19420903/width-of-error-bars-in-ggplot2
# vs
# coin <- crypto[crypto$name.full == "ripple" & year(crypto$date.comp) >= 2017 & month(crypto$date.comp) == 6, ]
# ggplot(coin) +
#   geom_errorbar(aes(x = date.comp, ymin = Low, ymax = High, color = day_loss), 
#                 size = 0.5, width = 0.9) +
#   geom_errorbar(aes(x = date.comp, ymin = Open, ymax = Close, color = day_loss), 
#                 size = bar_size(length(unique(coin$date.comp))), width = 0) +
#   scale_color_manual(name = "Daily Gain", values = c("dark green", "dark red"), labels = c("Yes", "No"))

#########################################################################################################################
#                                                                                                                       #
# The plot above is kept as an example of a failed attempt. I couldn't find out how to make a candlestick plot without  #
# using a random package which I want to stray away from at the moment. My best attempt at making this type of plot     #
# is shown above. I created this before I knew it was called a candlestick plot. After doing more research I found its  #
# name then found a stack overflow answer with the method below which works really well at keeping the distances        #
# properly spaced.                                                                                                      #
#                                                                                                                       #
#########################################################################################################################
# Candle chart:
ggplot(coin, aes(x = date.comp))+
  geom_linerange(aes(ymin = Low, ymax = High)) +
  geom_rect(aes(xmin = date.comp - 1/2*0.9, xmax = date.comp + 1/2*0.9, 
                ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = day_loss)) + 
  guides(fill = FALSE, colour = FALSE) + 
  scale_fill_manual(values = c("TRUE" = "darkred", "FALSE" = "darkgreen"))


coin <- crypto[crypto$name.full == "ripple" & year(crypto$date.comp) >= 2017, ]
ggplot(coin) +
  geom_errorbar(aes(x = date.comp, ymin = Low, ymax = High, color = day_loss), 
                size = 0.5, width = 0.9) +
  geom_errorbar(aes(x = date.comp, ymin = Open, ymax = Close, color = day_loss), 
                size = bar_size(length(unique(coin$date.comp))), width = 0) +
  scale_color_manual(name = "Daily Gain", values = c("dark green", "dark red"), labels = c("Yes", "No"))

# To do:
# https://www.babypips.com/learn/forex
# https://steemit.com/investing/@elderfinancial/my-in-depth-outline-of-how-i-analyze-cryptocurrencies
# https://masterthecrypto.com/fundamental-analysis-for-cryptocurrencies/
# https://blog.patricktriest.com/analyzing-cryptocurrencies-python/
# https://cryptopotato.com/bitcoin-crypto-technical-analysis-beginners/

# https://www.investopedia.com/articles/forex/11/10-ways-avoid-losing-money-forex.asp

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################