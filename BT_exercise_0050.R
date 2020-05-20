# This is the case of downloading data 0050.TW from yahoo
rm(list=ls())
con = gzcon(url('https://github.com/systematicinvestor/SIT/raw/master/sit.gz', 'rb'))
source(con)
close(con)

load.packages('quantmod')
tickers = spl('0050.TW')

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
head(data$`0050.TW`)
tail(data$`0050.TW`)
bt.prep(data, align='keep.all', dates='2010::2020')
sum(is.na(data$`0050.TW`$`0050.TW.Close`))
data$`0050.TW`$`0050.TW.Close`[which(is.na(data$`0050.TW`$`0050.TW.Close`) == TRUE), ]
data$`0050.TW`$`0050.TW.Close` <-  na.locf(data$`0050.TW`$`0050.TW.Close`)
#
models <- list()
# data$prices uses closing price, not adjusted prices
prices = data$prices    

# Buy & Hold    
data$weight[] = 1
models$buy.hold = bt.run(data) 
# you may turn off the timezone error message for just now
# options('xts_check_TZ'=FALSE)

# MA Cross
sma = bt.apply(data, function(x) {SMA(Cl(x), 200)})  
head(sma, 200)
head(prices, 200)
data$weight[] = NA
data$weight[] = iif(prices >= sma, 1, 0)
head(data$weight, 200)
names(data)
#
models$sma.cross = bt.run(data, do.lag = 1, trade.summary=T)  
names(models$sma.cross)
str(models$sma.cross)
models$sma.cross$trade.summary
#
bt.detail.summary(models$sma.cross)
plotbt.transition.map(models$sma.cross$weight)
plotbt.monthly.table(models$sma.cross$equity)
# Create a chart showing the strategies perfromance in 2000:2009
dates = '2010::2020'
buy.hold.equity = models$buy.hold$equity[dates] / as.double(models$buy.hold$equity[dates][1])
sma.cross.equity = models$sma.cross$equity[dates] / as.double(models$sma.cross$equity[dates][1])

chartSeries(buy.hold.equity, TA = c(addTA(sma.cross.equity, on=1, col='red')),  
            theme ='white', yrange = range(buy.hold.equity, sma.cross.equity) ) 









