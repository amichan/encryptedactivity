library(ggplot2)
library(RCurl)
library(gridExtra)
library(grid)

#--------------BTC from API, web, live update------------#
myCsv <- getURL("https://www.quandl.com/api/v3/datasets/BCHAIN/NTRAN.csv")
BitCoin <- read.csv(textConnection(myCsv))
BitCoin$Date <- as.Date(BitCoin$Date, "%Y-%m-%d")
#------------BTC Local copy for development------------#
#BitCoin <- read.csv("BTCVolume.csv")
#BitCoin$Date <- as.Date(BitCoin$Date, "%m/%d/%Y")
#BitCoin <- read.csv("bitcoinDailyTransactions.csv")
#BitCoin$Date <- as.Date(BitCoin$Date, "%Y-%m-%d")
BitCoinWeekly <- read.csv("bitcoinWeeklyTransactions.csv")
BitCoinWeekly$Date <- as.Date(BitCoinWeekly$Date, "%Y-%m-%d")
#------------Torr BW from API, web, live update------------#
#myCsv2 <- getURL("https://metrics.torproject.org/stats/bandwidth.csv")
#TorrBW <- read.csv(textConnection(myCsv2))
#TorrBW$date <- as.Date(TorrBW$date, "%Y-%m-%d")
#---------Torr Local copy for development--------------#
Torr <- read.csv('relay.csv')
Torr$Date <- as.Date(Torr$Date, "%m/%d/%Y")
#-----Analytics for sentiment analysis form Google Trends API --------#
#library(gtrendsR)
#usr = "entergmailusername.com"
#psw = "enterpassword"
#gconnect(usr, psw) #login
#Example call to Google Trends, specify start and end date (<3 months gives daily information)
#fear1 <- gtrends(c("terrorist", "terror", "bomb", "gunman", "ISIS"), start_date = "2014-01-01", end_date = "2014-04-01", granularity="d")
#Stringing together data sets for historical daily set left as part of project
#Also, alternative options will be considered for sentiment analysis such as NY Times and Twitter
#-----FearIndex local copy for development-----#
#Weekly data, historical from 2014
fear <- read.csv('fearWeekly.csv')
fear$Date <- as.Date(fear$Date, "%m/%d/%Y")
#Daily data, trailing 90 days
fear90 <-read.csv('FearTrailing90.csv')
fear90$Date <- as.Date(fear90$Date, "%m/%d/%Y")

#-----Create plots 1,2,3,4-----#
p1 = ggplot( data = BitCoin, aes( Date, Value/1000 )) + 
  geom_line() + 
  xlim(as.Date(c('1/1/2014', '1/1/2017'), format="%d/%m/%Y") ) +
  ggtitle("Bitcoin Number of Transactions (Daily), Source:Quandl") + 
  labs(x="Date",y="Bitcoin Number of Transactions / Thousands") 

p2 = ggplot( data = BitCoinWeekly, aes( Date, Value/1000 )) + 
  geom_line() + 
  xlim(as.Date(c('1/1/2014', '1/1/2017'), format="%d/%m/%Y") ) +
  ggtitle("Bitcoin Number of Transactions (Weekly Average), Source:Quandl") + 
  labs(x="Date",y="Bitcoin Number of Transactions / Thousands") 

#ggplot( data = TorrBW, aes( date, bwread*8/1000000000)) + geom_line() + xlim(as.Date(c('1/1/2014', '1/1/2017'), format="%d/%m/%Y") )
#p2 = ggplot( data = BitCoin, aes( Date, Value/1000 )) + geom_line() + xlim(as.Date(c('15/4/2016', '15/7/2016'), format="%d/%m/%Y") )

p3 = ggplot( data = Torr, aes( Date, Bandwidth )) + 
  geom_line() + 
  xlim(as.Date(c('1/1/2014', '1/1/2017'), format="%d/%m/%Y") ) + 
  ggtitle("Total Relay Bandwidth in the Torr Network, Source: TorMetrics") + 
  labs(x="Date",y="Bandwidth History Gbit/s") 
#p4 = ggplot( data = Torr, aes( Date, Bandwidth )) + geom_line() + xlim(as.Date(c('15/4/2016', '15/7/2016'), format="%d/%m/%Y") )

p4 = ggplot( data = fear, aes( Date, FearIndex )) + 
  geom_line() + 
  xlim(as.Date(c('1/1/2014', '1/1/2017'), format="%d/%m/%Y") ) +
  ggtitle("Fear Indicator, Source: Google Trends") + 
  labs(x="Date",y="Interest Over Time") 

#-------Merge the Data to perform Correlation-------#
BTC_Torr=merge(BitCoin, Torr, by="Date")
BTC_Fear=merge(BitCoinWeekly, fear, by="Date")
###### calculate correlation BTC and Torr
cor.test(BTC_Torr$Value, BTC_Torr$Bandwidth)
cor.test(BTC_Fear$Value, BTC_Fear$FearIndex)

#-------Plot data sets in grid------------------------#
grid.arrange(p1, p2, p3, p4, ncol = 2)



