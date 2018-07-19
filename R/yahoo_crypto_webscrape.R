#########################################################################################################################
#                                                                                                                       #
#  Title: Yahoo Crytopcurrency Web Scraper                                                                              #
#  Author: Christopher, Maerzluft                                                                                       #
#  Description: Webscrapes https://coinmarketcap.com to pull all historical data and current data snapshot              #
#  Last Edit: 12/12/17                                                                                                  #
#                                                                                                                       #
#########################################################################################################################
# https://github.com/cran/pdfetch
# or quantmod
# see https://www.computerworld.com/article/3109890/data-analytics/these-r-packages-import-sports-weather-stock-data-and-more.html

##### Libraries, Directories, and Other #####
# Clean global environment
rm(list = ls(all = TRUE))
gc()
# Overall options
options(stringsAsFactors = FALSE, scipen = 999)
work.dir <- "~/Desktop/Miscellaneous/Data Blog/Cryptocurrency/"
data.dir <- "Data"
scrp.dir <- "Webscraping Data"
date <- gsub("-", "", Sys.Date())
datetime <- paste(gsub("-", "", Sys.Date()), format(Sys.time(), "%H%M"), sep = "_")
setwd(work.dir)
library(rvest)
library(curl)
library(httr)

##### Find All Currencies #####
i1 <- 1
count <- 100
offset <- (i1 - 1)*count
yah.html <- read_html(paste("https://finance.yahoo.com/cryptocurrencies?offset=", offset, "&count=", count, sep = ""))
all.currencies <- yah.html %>%
  html_table('//*[@id="scr-res-table"]/table', header = TRUE, trim = TRUE) %>%
  data.frame()
while((dim(all.currencies)[1] %% count) == 0) {
  i1 <- i1 + 1
  offset <- (i1 - 1)*count
  yah.html <- read_html(paste("https://finance.yahoo.com/cryptocurrencies?offset=", offset, "&count=", count, sep = ""))
  yah.temp <- yah.html %>%
    html_table('//*[@id="scr-res-table"]/table', header = TRUE, trim = TRUE) %>%
    data.frame()
  all.currencies <- rbind(all.currencies, yah.temp)
}

# Find columns wish to save
ac.columns <- colnames(all.currencies)
symbol.column <- grep("Symbol", x = ac.columns, ignore.case = TRUE, value = TRUE)
name.column <- grep("Name", x = ac.columns, ignore.case = TRUE, value = TRUE)
price.column <- grep("Price", x = ac.columns, ignore.case = TRUE, value = TRUE)
change.column <- grep("^Change$", x = ac.columns, ignore.case = TRUE, value = TRUE)
pct.change.column <- grep("X.+Change", x = ac.columns, ignore.case = TRUE, value = TRUE)
mrkt.cap.column <- grep("Market", x = ac.columns, ignore.case = TRUE, value = TRUE)
volume.column <- grep("^Volume$", x = ac.columns, ignore.case = TRUE, value = TRUE)
volume.in.currency.column <- grep("Volume.in", x = ac.columns, ignore.case = TRUE, value = TRUE)
total.volume.column <- grep("Total", x = ac.columns, ignore.case = TRUE, value = TRUE)
supply.column <- grep("Supply", x = ac.columns, ignore.case = TRUE, value = TRUE)
ac.data <- all.currencies[, c(name.column,
                              symbol.column,
                              mrkt.cap.column, 
                              price.column, 
                              supply.column, 
                              total.volume.column, 
                              change.column, 
                              pct.change.column, 
                              volume.column,
                              volume.in.currency.column)]
# Control names
colnames(ac.data) <- c("name", 
                       "symbol", 
                       "market.cap", 
                       "price", 
                       "circ.supply", 
                       "volume.24h", 
                       "change.val", 
                       "change.pct", 
                       "volume",
                       "volumn.in.currency")
# Clean Formats
clean.numbers <- function(x) {
  x <- gsub(",", "", x)
  x <- gsub("\\+", "", x)
  x <- ifelse(grepl("\\%", x = x), as.numeric(gsub("\\%", "", x))/100, x)
  x <- suppressWarnings(
    ifelse(grepl("T", x = x), as.numeric(gsub("T", "", x))*1e12,
           ifelse(grepl("B", x = x), as.numeric(gsub("B", "", x))*1e9,
                  ifelse(grepl("M", x = x), as.numeric(gsub("M", "", x))*1e6,
                         as.numeric(x))))
  )
  
  return(x)
}

ac.data$market.cap <- clean.numbers(ac.data$market.cap)
ac.data$price <- clean.numbers(ac.data$price)
ac.data$circ.supply <- clean.numbers(ac.data$circ.supply)
ac.data$volume.24h <- clean.numbers(ac.data$volume.24h)
ac.data$change.val <- clean.numbers(ac.data$change.val)
ac.data$change.pct <- clean.numbers(ac.data$change.pct)
ac.data$volume <- clean.numbers(ac.data$volume)
ac.data$volumn.in.currency <- clean.numbers(ac.data$volumn.in.currency)
# Save current market snapshot in case we decide to analyze it later
write.csv(ac.data, paste(paste(data.dir, scrp.dir, sep = "/"), "/yahoo_current_mrkt_snapshot_", datetime, ".csv", sep = ""), row.names = FALSE)

##### Pull Historical Data #####
ac.data <- read.csv(paste(paste(data.dir, scrp.dir, sep = "/"), "/yahoo_current_mrkt_snapshot_", datetime, ".csv", sep = ""))
test <- do.call(rbind, lapply(ac.data$symbol[1:2], function(y) {
  # yah.html <- read_html(paste("https://finance.yahoo.com/quote/", y, "/history?period1=1323590400&period2=1512979200&interval=1d&filter=history&frequency=1d", sep = ""))
  yah.html <- read_html(paste("https://finance.yahoo.com/quote/", y, "/history?period1=1323590400&period2=1512979200&interval=1d&filter=history&frequency=1d", sep = ""))
  historical <- yah.html %>%
    html_table('//*[@id="Col1-1-HistoricalDataTable-Proxy"]/section/div[2]/table', header = TRUE, trim = TRUE) %>%
    data.frame()
  historical$symbol <- y
  historical
}))
# yah.html <- read_html(paste("https://finance.yahoo.com/quote/BTC-USD/history?period1=1323590400&period2=1512979200&interval=1d&filter=history&frequency=1d", sep = ""))
# historical <- yah.html %>%
#   html_table('//*[@id="Col1-1-HistoricalDataTable-Proxy"]/section/div[2]/table', header = TRUE, trim = TRUE) %>%
#   data.frame()

# period is a measurement of seconds
# "https://finance.yahoo.com/quote/BTC-USD/history?period1=1323590400&period2=1512979200&interval=1d&filter=history&frequency=1d"

cook <- cookies(GET("https://finance.yahoo.com/quote/BTC-USD/history?p=BTC-USD"))$value
tmp <- tempfile()
curl_download(paste("https://query1.finance.yahoo.com/v7/finance/download/BTC-USD?period1=1510467895&period2=1513059895&interval=1d&events=history&crumb=", 
                    cook, sep = ""), tmp)
cat(readLines(tmp), sep = "\n")

library(quantmod)
yhoo1 <- getSymbols("GOOG", src="google", env = NULL, auto.assign = FALSE)
yhoo2 <- getSymbols("GOOG", src="yahoo", env = NULL, auto.assign = FALSE)
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################