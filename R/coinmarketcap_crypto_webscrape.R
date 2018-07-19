#########################################################################################################################
#                                                                                                                       #
#  Title: Coin Market Cap Web Scraper                                                                                    #
#  Author: Christopher, Maerzluft                                                                                       #
#  Description: Webscrapes https://coinmarketcap.com to pull all historical data and current data snapshot              #
#  Last Edit: 12/11/17                                                                                                   #
#                                                                                                                       #
#########################################################################################################################
##### Libraries, Directories, and Other #####
# Clean global environment
rm(list = ls(all = TRUE))
gc()
options(stringsAsFactors = FALSE)
work.dir <- "~/Desktop/Miscellaneous/Data Blog/Cryptocurrency/"
data.dir <- "Data"
scrp.dir <- "Webscraping Data"
date <- gsub("-", "", Sys.Date())
datetime <- paste(gsub("-", "", Sys.Date()), format(Sys.time(), "%H%M"), sep = "_")
setwd(work.dir)
library(rvest)
library(curl)

#########################################################################################################################
#                                                                                                                       #
# First order of business is to find all cryptocurrencies whose data is collected by coinmarketcap.com. This will allow #
# us to full scrape all their data. In the event we couldn't find the list in a stable place we would have had to use   #
# other tricks to find the information. As it is, a current snapshot off all cryptocurrencies can be found at the first #
# link below. I believe this includes even inactive currencies. At the moment I do not believe we will use this data.   #
# It contains data from the moment the page was loaded but we will be pulling most of this data over the life time of   #
# the currencies. For us to use this data we would have to continously pull this data every hour until we had           #
# sufficient data to analyze. That being said we will pull all of it and clean it in case we go this direction in the   #
# future.                                                                                                               #
#                                                                                                                       #
#########################################################################################################################

##### Find All Currencies #####
cmc.all <- read_html("https://coinmarketcap.com/all/views/all/")
all.currencies <- cmc.all %>%
  html_table("#currencies-all", header = TRUE, trim = TRUE) %>%
  data.frame()

##### Clean All Data #####
# Find columns wish to save
ac.columns <- colnames(all.currencies)
name.column <- grep("Name", x = ac.columns, ignore.case = TRUE, value = TRUE)
symbol.column <- grep("Symbol", x = ac.columns, ignore.case = TRUE, value = TRUE)
mrkt.cap.column <- grep("Market", x = ac.columns, ignore.case = TRUE, value = TRUE)
price.column <- grep("Price", x = ac.columns, ignore.case = TRUE, value = TRUE)
supply.column <- grep("Supply", x = ac.columns, ignore.case = TRUE, value = TRUE)
volume.column <- grep("Volume", x = ac.columns, ignore.case = TRUE, value = TRUE)
change.1h.column <- grep("X.+1h", x = ac.columns, ignore.case = TRUE, value = TRUE)
change.24h.column <- grep("X.+24h", x = ac.columns, ignore.case = TRUE, value = TRUE)
change.7d.column <- grep("X.+7d", x = ac.columns, ignore.case = TRUE, value = TRUE)
# Save columns
ac.data <- all.currencies[, c(name.column,
                              symbol.column,
                              mrkt.cap.column, 
                              price.column, 
                              supply.column, 
                              volume.column, 
                              change.1h.column, 
                              change.24h.column, 
                              change.7d.column)]
# Control names
colnames(ac.data) <- c("name", 
                       "symbol", 
                       "market.cap", 
                       "price", 
                       "circ.supply", 
                       "volume.24h", 
                       "pct.change.1h", 
                       "pct.change.24h", 
                       "pct.change.7d")
# Mining status (comes from legend of table)
premined <- grep("\\*\\*", x = ac.data$circ.supply)
notmined <- grep("\\*", x = ac.data$circ.supply)
# \\* will also appear in premined observations so we must removed the ones that appear in both
notmined <- notmined[!(notmined %in% premined)]
ac.data$mine_status <- "Mineable"
ac.data$mine_status[premined] <- "Significantly Premined"
ac.data$mine_status[notmined] <- "Not Mineable"
# Clean Formats
clean.numbers <- function(x, perc = FALSE) {
  x <- gsub("\\*", "", x)
  x <- gsub("\\$", "", x)
  x <- gsub("\\%", "", x)
  x <- gsub("\n", "", x)
  x <- gsub(" ", "", x)
  x <- gsub(",", "", x)
  x <- gsub("-", NA, x)
  x <- gsub("[A-Za-z]+", NA, x)
  x <- ifelse(x == "?", NA, x)
  x <- ifelse(x == "LowVol", "0", x)
  x <- as.numeric(x)
  if (perc) {
    x <- x/100
  }
  
  return(x)
}
ac.data$market.cap <- clean.numbers(ac.data$market.cap)
ac.data$price <- clean.numbers(ac.data$price)
ac.data$circ.supply <- clean.numbers(ac.data$circ.supply)
ac.data$volume.24h <- clean.numbers(ac.data$volume.24h)
ac.data$pct.change.1h <- clean.numbers(ac.data$pct.change.1h, perc = TRUE)
ac.data$pct.change.24h <- clean.numbers(ac.data$pct.change.24h, perc = TRUE)
ac.data$pct.change.7d <- clean.numbers(ac.data$pct.change.7d, perc = TRUE)
#########################################################################################################################
#                                                                                                                       #
# The current market snapshot includes more detailed information than the historical which we are after, but it only    #
# has information from a single moment of time. If we rearlly want to analyze it we would have to pull it from multiple #
# moments in time over a short period (say once an hour). This doesn't seem to be necessary though as most people only  #
# look at the data in the historical. We will save current market snapshot anways in case we decide to analyze it later #
#                                                                                                                       #
#########################################################################################################################
write.csv(ac.data, paste(paste(data.dir, scrp.dir, sep = "/"), "/mktcap_current_mrkt_snapshot_", datetime, ".csv", sep = ""))

#########################################################################################################################
#                                                                                                                       #
# While I originally pulled the data above as a way of finding the names of every single cryptocurrency that ended up   #
# being a waste of time. The table restricts names to 12 characters long if the full name is longer that 15 characters. #
# Fortunately we found the the .currency-name <a href> actually includes the path to the information we are after       #
# including whether the cryptocurrency is called a currency or an asset.                                                #
#                                                                                                                       #
#########################################################################################################################
##### Pull Historical Data #####
# Pull path using html href - this ensures we get currencies vs assets correctly
currency.node <- cmc.all %>% 
  html_nodes(".currency-name a")
#########################################################################################################################
#                                                                                                                       #
# Originally this code simply returned data.frame(as.list(x)) but after going away for a while then coming back to the  #
# code, I discovered it no longer worked because some of the individual xml_attrs() were missing information. So I had  #
# to write some code to handle the case I could think of. I only discovered cases where I was missing class but I wrote #
# code to handle missing href, missing both, and having more than just those. I then realized that it wasn't that some  #
# currencies were missing information, it was that the code was pulling another thing as well in the html_nodes() above #
# to handle this, I only use observations that have 2 columns.                                                          #
#                                                                                                                       #
#########################################################################################################################
currency.path <- do.call(rbind, 
                         lapply(xml_attrs(currency.node), function(x) {
                           temp <- data.frame(as.list(x))
                           if (ncol(temp) == 2) {
                             temp
                           }
                         }))
currency.path <- currency.path$href
# Pull all historical data - takes sometime
a <- Sys.time()
hist.data.final <- do.call(rbind, lapply(1:length(currency.path), FUN = function(path.iter) {
  #######################################################################################################################
  #                                                                                                                     #
  # Why curl? We use curl because simply using read_html takes too long and the connection will be cutoff. Best I can   #
  # figure, rvest is not fully designed to handle larger access requests and depending on the website you are viewing   #
  # this can lead you to being kicked off their network. curl fixes this by properly addressing the website and         #
  # identifying your computer to the server.                                                                            #
  #                                                                                                                     #
  #######################################################################################################################
  print(currency.path[path.iter])
  hist.url <- paste("https://coinmarketcap.com", currency.path[path.iter], "historical-data/?start=20130428&end=", date, sep = "")
  hist.html <- read_html(curl(hist.url))
  hist.data <- hist.html %>%
    html_table("#historical-data", header = TRUE, trim = TRUE) %>%
    data.frame()
  # Pull name and other information where desired/possible
  hist.data$name.full <- gsub("/", "", gsub("/[A-Za-z]+/", "", currency.path[path.iter]))
  hist.data$name.partial <- ac.data$name[path.iter]
  hist.data$symbol <- ac.data$symbol[path.iter]
  currency.test <- grep("currencies", currency.path[path.iter]) == 1
  assets.test <- grep("assets", currency.path[path.iter]) == 1
  hist.data$type <- ifelse(length(currency.test) == 1, "Currency",
                           ifelse(length(assets.test) == 1, "Asset",
                                  "Unknown"))
  # Clean formats
  hist.data$date.comp <- as.Date(hist.data$Date, "%b %d, %Y")
  hist.data$Open <- clean.numbers(hist.data$Open)
  hist.data$High <- clean.numbers(hist.data$High)
  hist.data$Low <- clean.numbers(hist.data$Low)
  hist.data$Close <- clean.numbers(hist.data$Close)
  hist.data$Volume <- clean.numbers(hist.data$Volume)
  hist.data$Market.Cap <- clean.numbers(hist.data$Market.Cap)
  
  hist.data
}))
#########################################################################################################################
#                                                                                                                       #
# Data information: https://coinmarketcap.com/faq/                                                                      #
#                                                                                                                       #
#########################################################################################################################
b <- Sys.time()
# Ellapsed time - copy/paste new.avg over avg value
diff <- b - a 
avg <- 24.62721 # Minutes # replace this with new.avg
n <- 7 # Add one
new.avg <- (avg*n + diff)/(n + 1)
print(diff)
print(new.avg) # copy/paste this over avg
# Save data
write.csv(hist.data.final, paste(paste(data.dir, scrp.dir, sep = "/"), "/mktcap_all_historical_data_", datetime, ".csv", sep = ""), row.names = FALSE)

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################