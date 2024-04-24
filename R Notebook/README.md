Bitcoin Mining Stock Analysis
================
Last updated: 2024-04-23

## Preliminary Work: Install/Load Packages

To try and ensure that this R Notebook will run successfully, we’ll use
the [renv
package](https://cran.r-project.org/web/packages/renv/index.html) to
create a project-specific library of packages. This will allow us to
install the packages that we need for this project without affecting any
other projects that we may be working on. Additionally, the project
library will track the specific versions of the dependency packages so
that any updates to those packages will not break this project.

The code chunk below will first install the renv package if it is not
already installed. Then we will load the package. Next, we’ll use the
`restore()` function to install any packages listed in the renv.lock
file. Once these packages are installed, we can load them into the R
session using the `library()` commands. Below the code chunk, we’ll list
out the packages that will be used in the project demo. And if you run
into any trouble using renv, then you can use the second code chunk
below and that should be an even more reliable approach to install the
required packages.

``` r
# Install renv package if not already installed
if(!"renv" %in% installed.packages()[,"Package"]) install.packages("renv")
# Load renv package
library(renv)
# Use restore() to install any packages listed in the renv.lock file
renv::restore(clean=TRUE, lockfile="../renv.lock")
# Load in the packages
library(quantmod)
library(tidyverse)
library(tseries)
library(corrplot)
library(jsonlite)
library(stargazer)
```

- The [quantmod package](https://cran.r-project.org/package=quantmod)
  contains tools for importing and analyzing financial data.
- The [tidyverse package](https://www.tidyverse.org/) contains a suite
  of packages for data manipulation and visualization.
- The [tseries package](https://cran.r-project.org/package=tseries)
  contains additional time series analysis functions that we will
  explore.
- The [corrplot package](https://cran.r-project.org/package=corrplot)
  lets us create correlation plots.
- The [jsonlite package](https://cran.r-project.org/package=jsonlite)
  lets us more easily import JSON data.
- The [rmarkdown package](https://cran.r-project.org/package=rmarkdown)
  is used to generate this R Notebook.

Since the rmarkdown functionality is built into RStudio, this last one
is automatically loaded when you open RStudio. So no need to use the
`library()` function for it. Another observation to make about the code
chunk above is that it is labeled as `setup`, which is a special name,
which the R Notebook will recognize and automatically run prior to
running any other code chunk. This is useful for loading in packages and
setting up other global options that will be used throughout the
notebook.

Then if you wish to try and update the versions of the various R
packages in the lock file, you can use the `renv::update()` function to
update the packages in the project library. However, it is possible that
these updates could break the code in this notebook. If so, you may need
to adapt the code to work with the updated packages.

My recommendation is to first run through the code using the versions of
the packages in the lock file. Then if you want to try and update the
packages, you can do so and then run through the code again to see if it
still works. If not, you can always revert back to the lock file
versions using the `renv::restore()` function.

If you update the packages and get everything working successfully, then
you can update the lock file using the `renv::snapshot()` function. This
will update the lock file with the versions of the packages that are
currently installed in the project library. Then you can commit the
updated lock file to the repository so that others can use the updated
versions of the packages.

### Alternative Package Installation Code

If you run into any trouble using renv in the code chunk above, then you
can use the code chunk below to install the required packages for this
analysis. This method will first check if you have already installed the
packages. If any are missing, it will then install them. Then it will
load the packages into the R session. A potential flaw in this approach
compared to using renv is that it will simply install the latest
versions of the packages, which could potentially break some of the code
in this notebook if any of the updates aren’t backwards compatible.

As long as you have downloaded the entire project repository, the renv
chunk above will likely be managing the packages. Thus, the `eval=FALSE`
option is used to prevent this chunk from running unless manually
executed. So if you only downloaded this one Rmd file, this code chunk
should take care of installing the packages for you.

``` r
# Create list of packages needed for this exercise
list.of.packages = c("quantmod","tidyverse","tseries","corrplot","jsonlite","rmarkdown")
# Check if any have not yet been installed
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# If any need to be installed, install them
if(length(new.packages)) install.packages(new.packages)
# Load in the packages
library(quantmod)
library(tidyverse)
library(tseries)
library(corrplot)
library(jsonlite)
```

## Data Import and Cleaning

First, let’s use the `getSymbols()` function from the quantmod package
to import the price data for several assets relevant to our analysis. In
addition to the five mining stocks, we will also import the price data
for Bitcoin and SPY to use as factors in explaining mining stock
performance. Then we’ll import data for inflation rates and treasury
yields to compute real returns and risk premiums.

### Stock and Crypto Data

The `getSymbols()` function will automatically create an xts object for
each asset with the OHLC (open, high, low, close) price series. The
`src` argument specifies the source of the data, which in this case is
Yahoo Finance. The `from` and `to` arguments specify the date range of
the data to be imported. Although MARA stock goes back to mid-2012, the
price series for BTC only goes back to late 2014. So we will pull all
the daily stock data beginning in October 2014. For the other mining
stocks, their series will begin when they first became publicly traded.

``` r
startdate = "2014-10-01"
tickers = c("BTC-USD",
            "MARA",
            "CLSK",
            "RIOT",
            "CIFR",
            "HUT",
            "BTDR",
            "SPY")
getSymbols(tickers,
           src="yahoo",
           from=startdate,
           to=Sys.Date())
```

Next, we will convert the daily price data to monthly series using the
`to.monthly()` function from the xts package. Although there could be
some interesting analysis at the daily frequency, the monthly data will
be more useful for analyzing long-term performance and volatility.
Additionally, comparing daily data between bitcoin (which trades 365
days a year) and the stocks (which trade about 250 business days a year)
can be misleading. So the monthly data will be more comparable across
assets.

``` r
BTCdaily = `BTC-USD`
BTCmonth = to.monthly(BTCdaily, name=NULL)
MARAdaily = MARA
MARAmonth = to.monthly(MARAdaily, name=NULL)
CLSKdaily = CLSK
CLSKmonth = to.monthly(CLSKdaily, name=NULL)
RIOTdaily = RIOT
RIOTmonth = to.monthly(RIOTdaily, name=NULL)
CIFRdaily = CIFR
CIFRmonth = to.monthly(CIFRdaily, name=NULL)
HUTdaily = HUT
HUTmonth = to.monthly(HUTdaily, name=NULL)
BTDRdaily = BTDR
BTDRmonth = to.monthly(BTDRdaily, name=NULL)
SPYdaily = SPY
SPYmonth = to.monthly(SPYdaily, name=NULL)
```

Now that we have each of the assets return series saved as daily and
monthly series, let’s compute the annualized returns for each. To do
this, we can assume continuous compounding and use the log returns to
calculate the annualized returns. If you compare the daily data for BTC
and any of the stocks, you’ll notice that BTC trades 365 days a year,
while stock data typically is focused on the roughly 250 trading days
per year. To try and resolve this discrepancy, we’ll just annualize all
of the daily returns using 365 days per year.

``` r
BTCdaily$Return = diff(log(BTCdaily$`BTC-USD.Adjusted`))*365*100
BTCmonth$Return = diff(log(BTCmonth$Adjusted))*12*100
MARAdaily$Return = diff(log(MARAdaily$MARA.Adjusted))*365*100 
MARAmonth$Return = diff(log(MARAmonth$Adjusted))*12*100
CLSKdaily$Return = diff(log(CLSKdaily$CLSK.Adjusted))*365*100
CLSKmonth$Return = diff(log(CLSKmonth$Adjusted))*12*100
RIOTdaily$Return = diff(log(RIOTdaily$RIOT.Adjusted))*365*100
RIOTmonth$Return = diff(log(RIOTmonth$Adjusted))*12*100
CIFRdaily$Return = diff(log(CIFRdaily$CIFR.Adjusted))*365*100
CIFRmonth$Return = diff(log(CIFRmonth$Adjusted))*12*100
HUTdaily$Return = diff(log(HUTdaily$HUT.Adjusted))*365*100
HUTmonth$Return = diff(log(HUTmonth$Adjusted))*12*100
BTDRdaily$Return = diff(log(BTDRdaily$BTDR.Adjusted))*365*100
BTDRmonth$Return = diff(log(BTDRmonth$Adjusted))*12*100
SPYdaily$Return = diff(log(SPYdaily$SPY.Adjusted))*365*100
SPYmonth$Return = diff(log(SPYmonth$Adjusted))*12*100
```

### Economic Data

Now let’s collect some economic from
[FRED](https://fred.stlouisfed.org/) to help with the analysis. We will
import data for the Consumer Price Index (CPI) and the 10-year Treasury
yield. The CPI data will be used to compute monthly inflation rates, and
the 10-year Treasury yields will be used to represent the risk-free rate
of return. Since we’ll be imputing daily inflation rates using linear
interpolation, let’s start the import a month prior to the start of the
stock data.

``` r
fredassets = c("CPIAUCSL","DGS10")
getSymbols(fredassets,
           src="FRED",
           from=as.character(as.Date(startdate) %m-% months(1)), 
           to=Sys.Date())
```

    ## [1] "CPIAUCSL" "DGS10"

The specific CPI series we are using is the [“Consumer Price Index for
All Urban Consumers: All Items”
(CPIAUCSL).](https://fred.stlouisfed.org/series/CPIAUCSL), which is a
common measure for inflation in the U.S. As with the stock price data,
the CPI is an index for prices and tends to increase over time. This
*non-stationarity* of the time series is addressed by converting to
inflation measurements. See
[bitcoin-timeseries-project](https://github.com/tim-dombrowski/bitcoin-timeseries-project)
for more on this concept of time series stationarity.

Also, note that the CPI/inflation series is observed at a monthly
frequency, rather than the daily frequency of the other data. So in the
chunk below, we’ll first compute annualized monthly inflation rates and
then create a daily series that uses linear interpolation to fill in the
missing values. This will allow us to merge the inflation data with the
other daily data, even though the primary analysis and comparisons will
focus on the monthly data.

``` r
# Compute the annualized inflation rate in percentage units
INFmonth = diff(log(CPIAUCSL))*12*100
# Create a daily xts object with the same length as the stock data
INFdaily = xts(order.by=seq(min(index(INFmonth)), length=nrow(BTCdaily), by="day"))
# Merge to monthly inflation observations
INFdaily = merge(INFdaily, INFmonth)
# Impute missing values using linear interpolation
INFdaily = na.approx(INFdaily, na.rm = FALSE)
```

Unlike the stock and CPI variables, which are measured in prices or
index levels, the 10-year treasury yield is already an annualized rate
in percentage units. So while we can use `to.monthly()` to convert the
daily series into a monthly OHLC xts object, it’d be more appropriate to
use the monthly average yield. This is done below using some tools from
the dplyr and lubridate packages.

``` r
# Generate monthly OHLC data for the 10-year treasury yield
DGS10daily = DGS10
DGS10month = to.monthly(DGS10daily, name=NULL)
```

    ## Warning in to.period(x, "months", indexAt = indexAt, name = name, ...): missing
    ## values removed from data

``` r
# Aggregate to monthly frequency and compute means
DGS10dailydf = as.data.frame(DGS10daily)
DGS10dailydf$date = index(DGS10daily)
DGS10monthmean =  mutate(DGS10dailydf, date =floor_date(date,"month")) |> 
  group_by(date) |> summarise(DGS10 = mean(DGS10, na.rm=TRUE))
# Add the means to the monthly xts object
DGS10month$Mean = DGS10monthmean$DGS10
```

### Bitcoin Mining Data

Next, we’ll use [mempool.space](https://mempool.space/) to collect data
on Bitcoin mining difficulty and hashrate. The mining difficulty is a
measure of how difficult it is to find a new block on the Bitcoin
blockchain, while the hashrate is a measure of the total computational
power of the Bitcoin network. Both of these variables can have an impact
on the profitability of Bitcoin mining and the performance of Bitcoin
mining stocks.

#### Hashrate Data

First, specify the API base and endpoint for the hashrate (see the
[mempool.space API Documentation](https://mempool.space/docs/api/rest)
for more details). Then make the API call and read the JSON response
with the `fromJSON()` function from the jsonlite package.

``` r
# Build endpoint url for hashrates
mempoolbase = "https://mempool.space/api/v1/"
hashrateendpt = "mining/hashrate/pools/"
hashrateurl = paste(mempoolbase, hashrateendpt, sep="")
# Make API call and read JSON response
hashrateresponse = fromJSON(hashrateurl)
```

Next, we’ll extract the hashrate and difficulty data from the JSON
response and reformat the dates from Unix time to the R Date format.
Then we’ll convert the daily hashrate data to monthly data by
calculating the average hashrate for each month.

``` r
# Extract hashrate table and difficulty table
hashratedf = hashrateresponse$hashrates
difficultydf = hashrateresponse$difficulty
# Reformat dates from unix time to R date
hashratedf$date = hashratedf$timestamp |> as.POSIXct() |> as.Date()
difficultydf$date = difficultydf$time |> as.POSIXct() |> as.Date()
# Convert daily hashrate data to monthly averages
hashratemonthdf =  mutate(hashratedf, date =floor_date(date,"month")) |> 
  group_by(date) |> summarise(avgHashrate = mean(avgHashrate, na.rm=TRUE))
```

Now that we have the hashrate data at both daily and monthly
frequencies, let’s focus a bit on the units. If we plot the hashrate
data, we’ll see that the units are in hashes per second. This quickly
grows to a very large number, so it’s common to see the hashrate
expressed in terms of terahashes per second (TH/s), petahashes per
second (PH/s), or exahashes per second (EH/s). See the values in the
code chunk below for the scale of an exahash. *The warning generated by
the log hashrate chart results from a few 0 observations in the data for
the some of the earliest days in the Bitcoin network.*

``` r
# Plot the hashrate data (daily in gray, monthly in black)
ggplot() +
  geom_line(aes(x=hashratedf$date, y=hashratedf$avgHashrate/1000000000000000000), color="darkgray") +
  geom_line(aes(x=hashratemonthdf$date, y=hashratemonthdf$avgHashrate/1000000000000000000), color="black") +
  labs(title="Bitcoin Network Hashrate (1 EH = 1,000,000,000,000,000,000 hashes)",
       x="Date",
       y="Hashrate (EH/s)")
```

![](README_files/figure-gfm/hashrateplot-1.png)<!-- -->

``` r
# Log chart for the hashrate data (daily in gray, monthly in black)
ggplot() +
  geom_line(aes(x=hashratedf$date, y=hashratedf$avgHashrate), color="darkgray") +
  geom_line(aes(x=hashratemonthdf$date, y=hashratemonthdf$avgHashrate), color="black") +
  scale_y_continuous(transform='log10') +
  labs(title="Bitcoin Network Hashrate, log scale",
       x="Date",
       y="Hashrate (hashes/s)")
```

    ## Warning in scale_y_continuous(transform = "log10"): log-10 transformation
    ## introduced infinite values.

![](README_files/figure-gfm/hashrateplot-2.png)<!-- -->

Since this data looks non-stationary, let’s compute a differenced
series. As with the stock series and economic data, we’ll compute an
annualized growth rate. Then we can validate this transformation by
using the Augmented Dickey-Fuller test.

``` r
# Calculate daily hashrate growth and annualize it
hashratedf$annHashrateGrowth = c(NA, diff(log(hashratedf$avgHashrate))*365*100)
# Calculate monthly hashrate growth and annualize it
hashratemonthdf$annHashrateGrowth = c(NA, diff(log(hashratemonthdf$avgHashrate))*12*100)
```

To demonstrate non-stationarity of the hashrate series and stationarity
of the growth rates, the Augmented Dickey-Fuller test will be used. The
null hypothesis of the ADF test is that the series has a unit root,
which implies that the series is non-stationary. If the p-value of the
test is less than 0.05, then we can reject the null hypothesis and
conclude that the series is stationary. Note the removal of the first 8
observations from the daily test, as the hashrate data in the first few
days has some 0’s, which produce some Inf and -Inf growth rates.

``` r
adf.test(hashratedf$avgHashrate[-c(1:8)])
```

    ## Warning in adf.test(hashratedf$avgHashrate[-c(1:8)]): p-value greater than
    ## printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  hashratedf$avgHashrate[-c(1:8)]
    ## Dickey-Fuller = 4.0288, Lag order = 17, p-value = 0.99
    ## alternative hypothesis: stationary

``` r
adf.test(hashratedf$annHashrateGrowth[-c(1:8)])
```

    ## Warning in adf.test(hashratedf$annHashrateGrowth[-c(1:8)]): p-value smaller
    ## than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  hashratedf$annHashrateGrowth[-c(1:8)]
    ## Dickey-Fuller = -20.41, Lag order = 17, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
adf.test(hashratemonthdf$avgHashrate[-1])
```

    ## Warning in adf.test(hashratemonthdf$avgHashrate[-1]): p-value greater than
    ## printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  hashratemonthdf$avgHashrate[-1]
    ## Dickey-Fuller = 4.5893, Lag order = 5, p-value = 0.99
    ## alternative hypothesis: stationary

``` r
adf.test(hashratemonthdf$annHashrateGrowth[-1])
```

    ## Warning in adf.test(hashratemonthdf$annHashrateGrowth[-1]): p-value smaller
    ## than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  hashratemonthdf$annHashrateGrowth[-1]
    ## Dickey-Fuller = -4.1472, Lag order = 5, p-value = 0.01
    ## alternative hypothesis: stationary

After cleaning the hashrate data, let’s reformat to an xts object and
remove any observations prior to the start of the price series.

``` r
# Preview first 10 rows of hash rate data
head(hashratedf, 10)
```

    ##     timestamp avgHashrate       date annHashrateGrowth
    ## 1  1231006505    9273.324 2009-01-03                NA
    ## 2  1231027200       0.000 2009-01-04              -Inf
    ## 3  1231113600       0.000 2009-01-05               NaN
    ## 4  1231200000       0.000 2009-01-06               NaN
    ## 5  1231286400       0.000 2009-01-07               NaN
    ## 6  1231372800       0.000 2009-01-08               NaN
    ## 7  1231459200       0.000 2009-01-09               NaN
    ## 8  1231545600  128186.976 2009-01-10               Inf
    ## 9  1231632000 1676963.272 2009-01-11          93850.62
    ## 10 1231718400 4679507.181 2009-01-12          37456.60

``` r
# Convert data frame to xts object
hashratexts = xts(hashratedf, order.by=hashratedf$date)
# Remove observations prior to startdate
hashratexts = hashratexts[paste0(as.character(startdate), "/")]
# Convert monthly data to xts object
hashratemonthxts = xts(hashratemonthdf, order.by=hashratemonthdf$date)
# Remove observations prior to startdate
hashratemonthxts = hashratemonthxts[paste0(as.character(startdate), "/")]
```

#### Difficulty Data

In addition to the hashrate data, the response from the mempool.space
API includes the mining difficulty, which is a measure of how difficult
it is to find a new block on the Bitcoin blockchain. The difficulty is
adjusted every 2,016 blocks to ensure that the average time between
blocks is approximately 10 minutes. The difficulty is a key factor in
determining the profitability of Bitcoin mining, as it affects the
amount of computational power required to mine new blocks.

Since the difficulty adjustment frequency is measured in blocks, this
time series is much less uniform than the daily and monthly data for the
other variables. To make the data more uniform, we’ll fill the
intermediate days with the last observed value. This is known as the
last-observation-carried-forward method (`na.locf()`). Then we’ll
convert the daily difficulty data to monthly data by taking the average
difficulty for each month and doing much of the same processing as was
done for the hashrate data. See the comments in the code chunk for more
details.

``` r
# Convert mining difficulty series to xts object
difficultyxts = xts(difficultydf, order.by=difficultydf$date)
# Create a daily xts object with the same length as the hashrate data
difficultydailyxts = xts(order.by=seq(min(difficultydf$date), length=length(hashratedf$date), by="day"))
# Merge the daily difficulty data with the daily hashrate data
difficultydailyxts = merge(difficultydailyxts, difficultyxts)
```

    ## Warning in merge.xts(difficultydailyxts, difficultyxts): NAs introduced by
    ## coercion

``` r
# Impute missing values using last-observation-carried-forward method
difficultydailyxts = na.locf(difficultydailyxts)
# Fix the date column
#difficultydailyxts = subset(difficultydailyxts, select=-date) 
difficultydailydf = data.frame(difficultydailyxts)
difficultydailydf$date = index(difficultydailyxts)
# Convert daily hashrate data to monthly averages
difficultymonthdf =  mutate(difficultydailydf, date=floor_date(date,"month")) |> 
  group_by(date) |> summarise(avgDifficulty=mean(difficulty, na.rm=TRUE))
# Plot the difficulty data (daily in gray, monthly in black)
ggplot() +
  geom_line(aes(x=difficultydailydf$date, y=difficultydailydf$difficulty), color="darkgray") +
  geom_line(aes(x=difficultymonthdf$date, y=difficultymonthdf$avgDifficulty), color="black") +
  labs(title="Bitcoin Network Difficulty",
       x="Date",
       y="Difficulty")
```

![](README_files/figure-gfm/difficultycleaning-1.png)<!-- -->

``` r
# Log chart for the hashrate data (daily in gray, monthly in black)
ggplot() +
  geom_line(aes(x=difficultydailydf$date, y=difficultydailydf$difficulty), color="darkgray") +
  geom_line(aes(x=difficultymonthdf$date, y=difficultymonthdf$avgDifficulty), color="black") +
  scale_y_continuous(transform='log10') +
  labs(title="Bitcoin Network Difficulty, log scale",
       x="Date",
       y="Difficulty")
```

![](README_files/figure-gfm/difficultycleaning-2.png)<!-- -->

``` r
# Compute difficulty growth rates
difficultydailydf$annDifficultyGrowth = c(NA, diff(log(difficultydailydf$difficulty))*365*100)
difficultymonthdf$annDifficultyGrowth = c(NA, diff(log(difficultymonthdf$avgDifficulty))*12*100)
# Estimate ADF tests
adf.test(difficultydailydf$difficulty[-c(1:8)])
```

    ## Warning in adf.test(difficultydailydf$difficulty[-c(1:8)]): p-value greater
    ## than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  difficultydailydf$difficulty[-c(1:8)]
    ## Dickey-Fuller = 6.1668, Lag order = 17, p-value = 0.99
    ## alternative hypothesis: stationary

``` r
adf.test(difficultydailydf$annDifficultyGrowth[-c(1:8)])
```

    ## Warning in adf.test(difficultydailydf$annDifficultyGrowth[-c(1:8)]): p-value
    ## smaller than printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  difficultydailydf$annDifficultyGrowth[-c(1:8)]
    ## Dickey-Fuller = -13.568, Lag order = 17, p-value = 0.01
    ## alternative hypothesis: stationary

``` r
adf.test(difficultymonthdf$avgDifficulty[-1])
```

    ## Warning in adf.test(difficultymonthdf$avgDifficulty[-1]): p-value greater than
    ## printed p-value

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  difficultymonthdf$avgDifficulty[-1]
    ## Dickey-Fuller = 4.6762, Lag order = 5, p-value = 0.99
    ## alternative hypothesis: stationary

``` r
adf.test(difficultymonthdf$annDifficultyGrowth[-1])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  difficultymonthdf$annDifficultyGrowth[-1]
    ## Dickey-Fuller = -3.7692, Lag order = 5, p-value = 0.02199
    ## alternative hypothesis: stationary

``` r
# Convert monthly data to xts object
difficultymonthxts = xts(difficultymonthdf, order.by=difficultymonthdf$date)
# Add annDifficultyGrowth to daily xts object
difficultydailyxts$annDifficultyGrowth = xts(difficultydailydf$annDifficultyGrowth, order.by=index(difficultydailyxts))
# Remove observations prior to startdate
difficultydailyxts = difficultydailyxts[paste(as.character(startdate), "/", sep="")]
difficultymonthxts = difficultymonthxts[paste(as.character(startdate), "/", sep="")]
```

### Merge Final Dataset

Now that we have the annualized daily returns and annualized monthly
returns, let’s consolidate those values into a single data frame for
each frequency. This will make it easier to analyze the data and create
visualizations.

``` r
# Merge the daily returns into a single data frame
dailyreturns = merge(INFdaily$CPIAUCSL,
                     DGS10daily$DGS10,
                     BTCdaily$Return,
                     MARAdaily$Return,
                     CLSKdaily$Return,
                     RIOTdaily$Return,
                     CIFRdaily$Return,
                     HUTdaily$Return,
                     BTDRdaily$Return,
                     SPYdaily$Return,
                     hashratexts$annHashrateGrowth,
                     difficultydailyxts$annDifficultyGrowth)
colnames(dailyreturns) = c("INF","RF","BTC","MARA","CLSK","RIOT","CIFR","HUT","BTDR","SPY","Hashrate","Difficulty")
# Merge the monthly returns into a single data frame
monthlyreturns = merge(INFmonth$CPIAUCSL,
                       DGS10month$Mean,
                       BTCmonth$Return,
                       MARAmonth$Return,
                       CLSKmonth$Return,
                       RIOTmonth$Return,
                       CIFRmonth$Return,
                       HUTmonth$Return,
                       BTDRmonth$Return,
                       SPYmonth$Return,
                       hashratemonthxts$annHashrateGrowth,
                       difficultymonthxts$annDifficultyGrowth)
colnames(monthlyreturns) = c("INF","RF","BTC","MARA","CLSK","RIOT","CIFR","HUT","BTDR","SPY","Hashrate","Difficulty")
# Drop data prior to earliest inflation observation
dailyreturns = dailyreturns["2014-11-01/",]
monthlyreturns = monthlyreturns["2014-11-01/",]
# Trim any observations since the last inflation reading (typically 60-90 days)
ntrim = sum(is.na(tail(dailyreturns$INF,100)))
daily_nominal = dailyreturns[1:(nrow(dailyreturns)-ntrim),]
# Same for monthly series (typically 1-2 observations)
ntrim = sum(is.na(tail(monthlyreturns$INF)))
monthly_nominal = monthlyreturns[1:(nrow(monthlyreturns)-ntrim),]
```

## Data Analysis

### Real Returns

To compute the real returns for each asset, we will subtract the
inflation rate from the nominal returns. This will allow us to analyze
the performance of each asset after adjusting for inflation. We will
compute the real returns for both the daily and monthly return series.

``` r
infadjust = function(nominal, inf) {
  return((nominal-inf)/(1+(inf/100)))
}
# Compute real returns for daily series
daily_real = daily_nominal[,-1] # drop INF column since it is used here
daily_real$RF = infadjust(daily_nominal$RF, daily_nominal$INF)
daily_real$BTC = infadjust(daily_nominal$BTC, daily_nominal$INF)
daily_real$MARA = infadjust(daily_nominal$MARA, daily_nominal$INF)
daily_real$CLSK = infadjust(daily_nominal$CLSK, daily_nominal$INF)
daily_real$RIOT = infadjust(daily_nominal$RIOT, daily_nominal$INF)
daily_real$CIFR = infadjust(daily_nominal$CIFR, daily_nominal$INF)
daily_real$HUT = infadjust(daily_nominal$HUT, daily_nominal$INF)
daily_real$BTDR = infadjust(daily_nominal$BTDR, daily_nominal$INF)
daily_real$SPY = infadjust(daily_nominal$SPY, daily_nominal$INF)
# Compute real returns for monthly series
monthly_real = monthly_nominal[,-1] # drop INF column since it is used here
monthly_real$RF = infadjust(monthly_nominal$RF, monthly_nominal$INF)
monthly_real$BTC = infadjust(monthly_nominal$BTC, monthly_nominal$INF)
monthly_real$MARA = infadjust(monthly_nominal$MARA, monthly_nominal$INF)
monthly_real$CLSK = infadjust(monthly_nominal$CLSK, monthly_nominal$INF)
monthly_real$RIOT = infadjust(monthly_nominal$RIOT, monthly_nominal$INF)
monthly_real$CIFR = infadjust(monthly_nominal$CIFR, monthly_nominal$INF)
monthly_real$HUT = infadjust(monthly_nominal$HUT, monthly_nominal$INF)
monthly_real$BTDR = infadjust(monthly_nominal$BTDR, monthly_nominal$INF)
monthly_real$SPY = infadjust(monthly_nominal$SPY, monthly_nominal$INF)
```

### Excess Returns (Risk Premiums)

To compute the excess returns for each asset, we will subtract the
risk-free rate of return from the real returns. This will allow us to
analyze the performance of each asset after adjusting for inflation and
the risk-free rate. We will compute the excess returns for both the
daily and monthly return series.

``` r
# Compute excess returns for daily series
daily_excess = daily_real[,-1] # drop RF column since it is used here
daily_excess$BTC = daily_real$BTC - daily_real$RF
daily_excess$MARA = daily_real$MARA - daily_real$RF
daily_excess$CLSK = daily_real$CLSK - daily_real$RF
daily_excess$RIOT = daily_real$RIOT - daily_real$RF
daily_excess$CIFR = daily_real$CIFR - daily_real$RF
daily_excess$HUT = daily_real$HUT - daily_real$RF
daily_excess$BTDR = daily_real$BTDR - daily_real$RF
daily_excess$SPY = daily_real$SPY - daily_real$RF
# Compute excess returns for monthly series
monthly_excess = monthly_real[,-1] # drop RF column since it is used here
monthly_excess$BTC = monthly_real$BTC - monthly_real$RF
monthly_excess$MARA = monthly_real$MARA - monthly_real$RF
monthly_excess$CLSK = monthly_real$CLSK - monthly_real$RF
monthly_excess$RIOT = monthly_real$RIOT - monthly_real$RF
monthly_excess$CIFR = monthly_real$CIFR - monthly_real$RF
monthly_excess$HUT = monthly_real$HUT - monthly_real$RF
monthly_excess$BTDR = monthly_real$BTDR - monthly_real$RF
monthly_excess$SPY = monthly_real$SPY - monthly_real$RF
```

### Univariate Statistics

We will start by calculating the average annual returns and standard
deviations for each series. The chunk below focuses on the daily
returns. First, we’ll compute the average annualized nominal returns,
real returns, and excess returns for each asset. Then we’ll compute the
standard deviation of the annual returns for each asset. We’ll do the
same for the monthly returns in the next chunk.

``` r
# Compute the average annualized nominal returns
colMeans(daily_nominal, na.rm=TRUE) |> round(2)
```

    ##        INF         RF        BTC       MARA       CLSK       RIOT       CIFR 
    ##       2.92       2.32      55.87     -20.73     -13.35      32.81     -50.29 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##     -17.01     -21.92      17.11      80.22      82.43

``` r
# Compute the average annualized real returns
colMeans(daily_real, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##      -0.49      51.80     -24.77     -18.62      29.76     -48.70     -18.13 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##     -27.04      13.66      80.22      82.43

``` r
# Compute the average annualized excess returns
colMeans(daily_excess, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##      61.34     -27.10     -21.87      25.23     -38.15     -16.03     -19.27 
    ##        SPY   Hashrate Difficulty 
    ##      14.23      80.22      82.43

``` r
# Compute the standard deviation of the annual nominal returns
apply(daily_nominal, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        INF         RF        BTC       MARA       CLSK       RIOT       CIFR 
    ##       3.29       0.94    1355.02    3021.34    3566.73    2634.64    2577.70 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##    2644.67    2029.37     413.02    4388.08     696.80

``` r
# Compute the standard deviation of the annual real returns
apply(daily_real, 2, sd, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##       3.28    1326.55    2937.11    3485.75    2551.24    2440.19    2569.30 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##    1966.28     412.84    4388.08     696.80

``` r
# Compute the standard deviation of the annual excess returns
apply(daily_excess, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##    1441.14    2943.50    3492.87    2551.70    2422.44    2574.99    1967.43 
    ##        SPY   Hashrate Difficulty 
    ##     413.76    4388.08     696.80

Now at the monthly frequency:

``` r
# Compute the average annualized nominal returns
colMeans(monthly_nominal, na.rm=TRUE) |> round(2)
```

    ##        INF         RF        BTC       MARA       CLSK       RIOT       CIFR 
    ##       2.91       2.34      56.83     -16.05      -6.83      20.32     -18.98 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##      -2.43     -12.16      11.93      82.15      82.33

``` r
# Compute the average annualized real returns
colMeans(monthly_real, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##      -0.44      52.85     -19.14     -10.67      17.93     -19.40      -3.83 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##     -17.05       8.82      82.15      82.33

``` r
# Compute the average annualized excess returns
colMeans(monthly_excess, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##      53.29     -18.70      -9.78      18.80     -17.11      -2.71     -15.32 
    ##        SPY   Hashrate Difficulty 
    ##       9.26      82.15      82.33

``` r
# Compute the standard deviation of the annual nominal returns
apply(monthly_nominal, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        INF         RF        BTC       MARA       CLSK       RIOT       CIFR 
    ##       3.58       0.95     247.71     445.75     440.34     405.62     343.59 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##     408.37     361.27      53.74     106.17     105.01

``` r
# Compute the standard deviation of the annual real returns
apply(monthly_real, 2, sd, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##       3.52     241.76     432.22     424.48     391.72     326.37     393.75 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##     352.87      52.89     106.17     105.01

``` r
# Compute the standard deviation of the annual excess returns
apply(monthly_excess, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##     241.71     432.55     424.65     391.50     325.10     393.42     353.16 
    ##        SPY   Hashrate Difficulty 
    ##      52.94     106.17     105.01

However, an important note to make about the results above is that only
BTC, MARA, and SPY have data going back to 2014. So the other mining
stocks each have shorter time frames. Thus, we should be cautious to
draw any comparative conclusions from the stats above.

To create a more comparable basis for analysis, let’s create a subset of
the monthly series that only includes observations where all assets have
data.

``` r
# Subset down to complete.cases
monthly_nominal_final = monthly_nominal[complete.cases(monthly_nominal),]
monthly_real_final = monthly_real[complete.cases(monthly_real),]
monthly_excess_final = monthly_excess[complete.cases(monthly_excess),]
```

Now let’s re-run the univariate statistics for the final subset of
monthly data.

``` r
# Compute the average annualized nominal returns
colMeans(monthly_nominal_final, na.rm=TRUE) |> round(2)
```

    ##        INF         RF        BTC       MARA       CLSK       RIOT       CIFR 
    ##       5.17       3.21      20.20      -7.57      16.94     -37.14     -24.58 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##     -28.57     -12.16       8.15      67.23      65.15

``` r
# Compute the average annualized real returns
colMeans(monthly_real_final, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##      -1.74      16.40      -8.44      12.53     -36.78     -23.67     -28.61 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##     -17.05       3.25      67.23      65.15

``` r
# Compute the average annualized excess returns
colMeans(monthly_excess_final, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##      18.14      -6.70      14.27     -35.05     -21.93     -26.87     -15.32 
    ##        SPY   Hashrate Difficulty 
    ##       4.99      67.23      65.15

``` r
# Compute the standard deviation of the annual nominal returns
apply(monthly_nominal_final, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        INF         RF        BTC       MARA       CLSK       RIOT       CIFR 
    ##       3.74       1.04     218.83     497.36     417.00     409.07     389.63 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##     429.34     361.27      64.09      60.66      55.61

``` r
# Compute the standard deviation of the annual real returns
apply(monthly_real_final, 2, sd, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##       3.97     204.66     475.90     393.39     389.38     370.15     407.02 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##     352.87      61.58      60.66      55.61

``` r
# Compute the standard deviation of the annual excess returns
apply(monthly_excess_final, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##     203.46     475.11     392.87     388.38     368.70     406.26     353.16 
    ##        SPY   Hashrate Difficulty 
    ##      60.87      60.66      55.61

Since we have the excess returns, we can quite easily calculate Sharpe
ratios for each asset to compare risk-adjusted returns. The Sharpe ratio
is calculated as the average excess return divided by the standard
deviation of the excess returns. The higher the Sharpe ratio, the better
the risk-adjusted return.

``` r
# Compute the Sharpe ratios for the monthly excess returns
sharpe = colMeans(monthly_excess_final, na.rm=TRUE)/apply(monthly_excess_final, 2, sd, na.rm=TRUE)
sharpe |> round(4)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##     0.0892    -0.0141     0.0363    -0.0902    -0.0595    -0.0661    -0.0434 
    ##        SPY   Hashrate Difficulty 
    ##     0.0819     1.1084     1.1716

### Multivariate Statistics

Now let’s explore the relationships between the variables in our cleaned
dataset. For each frequency, we’ll generate a correlation matrix and a
correlation plot to visualize the relationships between the assets.

#### Correlations

Since the transformations from nominal to real and excess returns are
linear, the correlations remain the same. So for the daily data, we’ll
compute the correlation matrix for the nominal returns and then create a
correlation plot. *Note the use of the `use="pairwise.complete"`
argument in the `cor()` function to handle missing values in the data,
and the use of the `|>` operator to round the answers to two decimal
places.*

``` r
# Compute the correlation matrix for the daily returns
cor(daily_nominal, use="pairwise.complete") |> round(2)
```

    ##              INF    RF   BTC  MARA  CLSK  RIOT  CIFR   HUT  BTDR   SPY Hashrate
    ## INF         1.00  0.01 -0.01  0.02  0.02 -0.02 -0.06 -0.02  0.02  0.02     0.00
    ## RF          0.01  1.00 -0.02 -0.03  0.00 -0.03  0.01 -0.03 -0.02 -0.01     0.00
    ## BTC        -0.01 -0.02  1.00  0.33  0.13  0.44  0.27  0.49  0.08  0.23     0.03
    ## MARA        0.02 -0.03  0.33  1.00  0.17  0.63  0.40  0.62  0.20  0.28    -0.01
    ## CLSK        0.02  0.00  0.13  0.17  1.00  0.22  0.42  0.22  0.21  0.16     0.04
    ## RIOT       -0.02 -0.03  0.44  0.63  0.22  1.00  0.41  0.60  0.19  0.33     0.01
    ## CIFR       -0.06  0.01  0.27  0.40  0.42  0.41  1.00  0.41  0.20  0.26     0.01
    ## HUT        -0.02 -0.03  0.49  0.62  0.22  0.60  0.41  1.00  0.21  0.31     0.01
    ## BTDR        0.02 -0.02  0.08  0.20  0.21  0.19  0.20  0.21  1.00  0.06    -0.02
    ## SPY         0.02 -0.01  0.23  0.28  0.16  0.33  0.26  0.31  0.06  1.00     0.02
    ## Hashrate    0.00  0.00  0.03 -0.01  0.04  0.01  0.01  0.01 -0.02  0.02     1.00
    ## Difficulty -0.02  0.00  0.02 -0.04 -0.04 -0.05 -0.03 -0.07 -0.05 -0.05    -0.01
    ##            Difficulty
    ## INF             -0.02
    ## RF               0.00
    ## BTC              0.02
    ## MARA            -0.04
    ## CLSK            -0.04
    ## RIOT            -0.05
    ## CIFR            -0.03
    ## HUT             -0.07
    ## BTDR            -0.05
    ## SPY             -0.05
    ## Hashrate        -0.01
    ## Difficulty       1.00

``` r
# Create a correlation plot for the daily returns
corrplot(cor(daily_nominal, use="pairwise.complete"), method="color")
```

![](README_files/figure-gfm/corrsdaily-1.png)<!-- -->

Now for the monthly data, we’ll start with the full dataset using the
`pairwise.complete` option for handling the missing values.

``` r
# Compute the correlation matrix for the full monthly returns
cor(monthly_nominal, use="pairwise.complete") |> round(2)
```

    ##              INF    RF   BTC  MARA  CLSK  RIOT  CIFR   HUT  BTDR   SPY Hashrate
    ## INF         1.00  0.05 -0.05  0.05  0.05 -0.11 -0.35 -0.15  0.05  0.04    -0.14
    ## RF          0.05  1.00 -0.07 -0.13  0.01 -0.16  0.15 -0.14 -0.10 -0.04     0.06
    ## BTC        -0.05 -0.07  1.00  0.50  0.32  0.61  0.49  0.72 -0.07  0.36     0.14
    ## MARA        0.05 -0.13  0.50  1.00  0.50  0.73  0.54  0.72  0.21  0.49     0.00
    ## CLSK        0.05  0.01  0.32  0.50  1.00  0.44  0.48  0.39  0.07  0.46     0.05
    ## RIOT       -0.11 -0.16  0.61  0.73  0.44  1.00  0.55  0.68  0.01  0.50    -0.02
    ## CIFR       -0.35  0.15  0.49  0.54  0.48  0.55  1.00  0.62  0.05  0.36     0.08
    ## HUT        -0.15 -0.14  0.72  0.72  0.39  0.68  0.62  1.00  0.15  0.40     0.17
    ## BTDR        0.05 -0.10 -0.07  0.21  0.07  0.01  0.05  0.15  1.00  0.23    -0.10
    ## SPY         0.04 -0.04  0.36  0.49  0.46  0.50  0.36  0.40  0.23  1.00     0.04
    ## Hashrate   -0.14  0.06  0.14  0.00  0.05 -0.02  0.08  0.17 -0.10  0.04     1.00
    ## Difficulty -0.11  0.07  0.09 -0.01 -0.03 -0.06 -0.02  0.10  0.02 -0.02     0.85
    ##            Difficulty
    ## INF             -0.11
    ## RF               0.07
    ## BTC              0.09
    ## MARA            -0.01
    ## CLSK            -0.03
    ## RIOT            -0.06
    ## CIFR            -0.02
    ## HUT              0.10
    ## BTDR             0.02
    ## SPY             -0.02
    ## Hashrate         0.85
    ## Difficulty       1.00

``` r
# Create a correlation plot for the full monthly returns
corrplot(cor(monthly_nominal, use="pairwise.complete"), method="color")
```

![](README_files/figure-gfm/corrsmonthlyfull-1.png)<!-- -->

Then we’ll create a correlation matrix and plot for the subset with no
missing values.

``` r
# Compute the correlation matrix for the full monthly returns
cor(monthly_nominal_final) |> round(2)
```

    ##              INF    RF   BTC  MARA  CLSK  RIOT  CIFR   HUT  BTDR   SPY Hashrate
    ## INF         1.00 -0.49 -0.29 -0.22 -0.09 -0.25 -0.36 -0.25  0.05 -0.15     0.02
    ## RF         -0.49  1.00  0.21  0.04  0.19  0.11  0.23 -0.05 -0.10  0.13    -0.25
    ## BTC        -0.29  0.21  1.00  0.77  0.74  0.70  0.59  0.77 -0.07  0.56     0.39
    ## MARA       -0.22  0.04  0.77  1.00  0.75  0.86  0.62  0.87  0.21  0.65     0.31
    ## CLSK       -0.09  0.19  0.74  0.75  1.00  0.72  0.55  0.68  0.07  0.52     0.23
    ## RIOT       -0.25  0.11  0.70  0.86  0.72  1.00  0.68  0.73  0.01  0.55     0.22
    ## CIFR       -0.36  0.23  0.59  0.62  0.55  0.68  1.00  0.70  0.05  0.38     0.17
    ## HUT        -0.25 -0.05  0.77  0.87  0.68  0.73  0.70  1.00  0.15  0.52     0.44
    ## BTDR        0.05 -0.10 -0.07  0.21  0.07  0.01  0.05  0.15  1.00  0.23    -0.10
    ## SPY        -0.15  0.13  0.56  0.65  0.52  0.55  0.38  0.52  0.23  1.00     0.15
    ## Hashrate    0.02 -0.25  0.39  0.31  0.23  0.22  0.17  0.44 -0.10  0.15     1.00
    ## Difficulty  0.11 -0.19  0.23  0.10  0.09  0.07 -0.01  0.13  0.02  0.06     0.72
    ##            Difficulty
    ## INF              0.11
    ## RF              -0.19
    ## BTC              0.23
    ## MARA             0.10
    ## CLSK             0.09
    ## RIOT             0.07
    ## CIFR            -0.01
    ## HUT              0.13
    ## BTDR             0.02
    ## SPY              0.06
    ## Hashrate         0.72
    ## Difficulty       1.00

``` r
# Create a correlation plot for the full monthly returns
corrplot(cor(monthly_nominal_final), method="color")
```

![](README_files/figure-gfm/corrsmonthly-1.png)<!-- -->

### Factor Models

#### Captial Asset Pricing Model (CAPM)

Let’s start by applying CAPM to each of the mining stocks and BTC. See
the
[sharpe-ratio-project](https://github.com/tim-dombrowski/sharpe-ratio-project)
for a more focused analysis on the CAPM model. From this point on, we’ll
stick to the final monthly subset of data spanning August 2021 to
present.

``` r
# BTC CAPM Regression
CAPM_BTC = lm(BTC~SPY, data=monthly_nominal_final)
summary(CAPM_BTC)
```

    ## 
    ## Call:
    ## lm(formula = BTC ~ SPY, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -376.65 -102.90  -10.57  117.58  347.04 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   4.6403    32.8719   0.141 0.888686    
    ## SPY           1.9092     0.5168   3.694 0.000878 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 184.4 on 30 degrees of freedom
    ## Multiple R-squared:  0.3127, Adjusted R-squared:  0.2898 
    ## F-statistic: 13.65 on 1 and 30 DF,  p-value: 0.0008778

``` r
# MARA CAPM Regression
CAPM_MARA = lm(MARA~SPY, data=monthly_nominal_final)
summary(CAPM_MARA)
```

    ## 
    ## Call:
    ## lm(formula = MARA ~ SPY, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1155.09  -189.34    53.86   202.40   586.89 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -48.777     68.357  -0.714    0.481    
    ## SPY            5.057      1.075   4.705 5.35e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 383.5 on 30 degrees of freedom
    ## Multiple R-squared:  0.4246, Adjusted R-squared:  0.4055 
    ## F-statistic: 22.14 on 1 and 30 DF,  p-value: 5.347e-05

``` r
# CLSK CAPM Regression
CAPM_CLSK = lm(CLSK~SPY, data=monthly_nominal_final)
summary(CAPM_CLSK)
```

    ## 
    ## Call:
    ## lm(formula = CLSK ~ SPY, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -923.90 -166.10  -27.07  208.01  680.49 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  -10.871     64.326  -0.169  0.86694   
    ## SPY            3.413      1.011   3.375  0.00206 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 360.9 on 30 degrees of freedom
    ## Multiple R-squared:  0.2752, Adjusted R-squared:  0.251 
    ## F-statistic: 11.39 on 1 and 30 DF,  p-value: 0.002055

``` r
# RIOT CAPM Regression
CAPM_RIOT = lm(RIOT~SPY, data=monthly_nominal_final)
summary(CAPM_RIOT)
```

    ## 
    ## Call:
    ## lm(formula = RIOT ~ SPY, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -744.03 -224.75   43.87  216.37  544.78 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -65.9919    61.6661  -1.070 0.293088    
    ## SPY           3.5409     0.9694   3.652 0.000983 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 346 on 30 degrees of freedom
    ## Multiple R-squared:  0.3078, Adjusted R-squared:  0.2847 
    ## F-statistic: 13.34 on 1 and 30 DF,  p-value: 0.0009826

``` r
# CIFR CAPM Regression
CAPM_CIFR = lm(CIFR~SPY, data=monthly_nominal_final)
summary(CAPM_CIFR)
```

    ## 
    ## Call:
    ## lm(formula = CIFR ~ SPY, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -721.98 -262.89   35.83  188.47  788.32 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  -43.483     65.258  -0.666   0.5103  
    ## SPY            2.319      1.026   2.261   0.0312 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 366.1 on 30 degrees of freedom
    ## Multiple R-squared:  0.1455, Adjusted R-squared:  0.1171 
    ## F-statistic:  5.11 on 1 and 30 DF,  p-value: 0.0312

``` r
# HUT CAPM Regression
CAPM_HUT = lm(HUT~SPY, data=monthly_nominal_final)
summary(CAPM_HUT)
```

    ## 
    ## Call:
    ## lm(formula = HUT ~ SPY, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -927.51 -129.44    2.89  266.49  786.87 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  -56.855     66.534  -0.855  0.39958   
    ## SPY            3.471      1.046   3.318  0.00238 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 373.3 on 30 degrees of freedom
    ## Multiple R-squared:  0.2685, Adjusted R-squared:  0.2441 
    ## F-statistic: 11.01 on 1 and 30 DF,  p-value: 0.002382

``` r
# BTDR CAPM Regression
CAPM_BTDR = lm(BTDR~SPY, data=monthly_nominal_final)
summary(CAPM_BTDR)
```

    ## 
    ## Call:
    ## lm(formula = BTDR ~ SPY, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1079.00  -100.33     9.06   107.75   911.53 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  -22.808     63.677  -0.358    0.723
    ## SPY            1.306      1.001   1.305    0.202
    ## 
    ## Residual standard error: 357.2 on 30 degrees of freedom
    ## Multiple R-squared:  0.05369,    Adjusted R-squared:  0.02215 
    ## F-statistic: 1.702 on 1 and 30 DF,  p-value: 0.2019

Now let’s extract the residuals from those regressions and look at those
correlations.

``` r
# Extract residuals from CAPM regressions
CAPM_resids = data.frame(
  BTC = residuals(CAPM_BTC),
  MARA = residuals(CAPM_MARA),
  CLSK = residuals(CAPM_CLSK),
  RIOT = residuals(CAPM_RIOT),
  CIFR = residuals(CAPM_CIFR),
  HUT = residuals(CAPM_HUT),
  BTDR = residuals(CAPM_BTDR)
)
# Compute the correlation matrix for the residuals
cor(CAPM_resids) |> round(2)
```

    ##        BTC MARA  CLSK  RIOT  CIFR  HUT  BTDR
    ## BTC   1.00 0.64  0.63  0.56  0.50 0.67 -0.24
    ## MARA  0.64 1.00  0.64  0.79  0.54 0.83  0.08
    ## CLSK  0.63 0.64  1.00  0.61  0.45 0.57 -0.06
    ## RIOT  0.56 0.79  0.61  1.00  0.61 0.62 -0.15
    ## CIFR  0.50 0.54  0.45  0.61  1.00 0.63 -0.04
    ## HUT   0.67 0.83  0.57  0.62  0.63 1.00  0.04
    ## BTDR -0.24 0.08 -0.06 -0.15 -0.04 0.04  1.00

``` r
# Create a correlation plot for the residuals
corrplot(cor(CAPM_resids), method="color")
```

![](README_files/figure-gfm/capmresiduals-1.png)<!-- -->

#### BTC-Factor Model (BFM)

Next, we’ll use the BTC excess return as a second factor for the mining
stock regressions. This will allow us to remove the common risk factor
of BTC and analyze any remaining correlations between the mining stocks.

``` r
# BTC-Factor Model Regressions
BFM_MARA = lm(MARA~SPY+BTC, data=monthly_nominal_final)
summary(BFM_MARA)
```

    ## 
    ## Call:
    ## lm(formula = MARA ~ SPY + BTC, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -698.01 -194.05   20.83  183.88  592.13 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -54.9939    53.1902  -1.034   0.3097    
    ## SPY           2.4987     1.0083   2.478   0.0193 *  
    ## BTC           1.3398     0.2953   4.537 9.18e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 298.3 on 29 degrees of freedom
    ## Multiple R-squared:  0.6635, Adjusted R-squared:  0.6403 
    ## F-statistic: 28.59 on 2 and 29 DF,  p-value: 1.386e-07

``` r
BFM_CLSK = lm(CLSK~SPY+BTC, data=monthly_nominal_final)
summary(BFM_CLSK)
```

    ## 
    ## Call:
    ## lm(formula = CLSK ~ SPY + BTC, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -483.58 -232.46  -22.81  243.12  474.87 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -16.5832    50.8728  -0.326  0.74678    
    ## SPY           1.0624     0.9644   1.102  0.27968    
    ## BTC           1.2311     0.2825   4.359  0.00015 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 285.3 on 29 degrees of freedom
    ## Multiple R-squared:  0.5621, Adjusted R-squared:  0.5319 
    ## F-statistic: 18.61 on 2 and 29 DF,  p-value: 6.318e-06

``` r
BFM_RIOT = lm(RIOT~SPY+BTC, data=monthly_nominal_final)
summary(BFM_RIOT)
```

    ## 
    ## Call:
    ## lm(formula = RIOT ~ SPY + BTC, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -408.76 -197.76    4.07  193.23  602.67 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -70.8521    52.0511  -1.361   0.1839   
    ## SPY           1.5412     0.9867   1.562   0.1291   
    ## BTC           1.0474     0.2890   3.624   0.0011 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 291.9 on 29 degrees of freedom
    ## Multiple R-squared:  0.5236, Adjusted R-squared:  0.4907 
    ## F-statistic: 15.94 on 2 and 29 DF,  p-value: 2.142e-05

``` r
BFM_CIFR = lm(CIFR~SPY+BTC, data=monthly_nominal_final)
summary(BFM_CIFR)
```

    ## 
    ## Call:
    ## lm(formula = CIFR ~ SPY + BTC, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -714.28 -287.94   -7.47  205.13  534.60 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -48.0462    57.6754  -0.833   0.4116   
    ## SPY           0.4415     1.0933   0.404   0.6893   
    ## BTC           0.9835     0.3202   3.071   0.0046 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 323.5 on 29 degrees of freedom
    ## Multiple R-squared:  0.3552, Adjusted R-squared:  0.3108 
    ## F-statistic: 7.989 on 2 and 29 DF,  p-value: 0.001723

``` r
BFM_HUT = lm(HUT~SPY+BTC, data=monthly_nominal_final)
summary(BFM_HUT)
```

    ## 
    ## Call:
    ## lm(formula = HUT ~ SPY + BTC, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -661.45 -182.70  -17.45  200.92  437.83 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -63.1700    50.1096  -1.261    0.217    
    ## SPY           0.8729     0.9499   0.919    0.366    
    ## BTC           1.3609     0.2782   4.891 3.43e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 281 on 29 degrees of freedom
    ## Multiple R-squared:  0.5992, Adjusted R-squared:  0.5715 
    ## F-statistic: 21.68 on 2 and 29 DF,  p-value: 1.749e-06

``` r
BFM_BTDR = lm(BTDR~SPY+BTC, data=monthly_nominal_final)
summary(BFM_BTDR)
```

    ## 
    ## Call:
    ## lm(formula = BTDR ~ SPY + BTC, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -915.4 -166.9   36.4  103.5  908.2 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -20.6207    62.8387  -0.328   0.7452  
    ## SPY           2.2062     1.1912   1.852   0.0742 .
    ## BTC          -0.4715     0.3489  -1.351   0.1870  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 352.4 on 29 degrees of freedom
    ## Multiple R-squared:  0.1098, Adjusted R-squared:  0.04836 
    ## F-statistic: 1.788 on 2 and 29 DF,  p-value: 0.1853

Now let’s extract the residuals from those regressions and look at those
correlations.

``` r
# Extract residuals from BFM regressions
BFM_resids = data.frame(
  MARA = residuals(BFM_MARA),
  CLSK = residuals(BFM_CLSK),
  RIOT = residuals(BFM_RIOT),
  CIFR = residuals(BFM_CIFR),
  HUT = residuals(BFM_HUT),
  BTDR = residuals(BFM_BTDR)
)
# Compute the correlation matrix for the residuals
cor(BFM_resids) |> round(2)
```

    ##      MARA CLSK  RIOT CIFR  HUT  BTDR
    ## MARA 1.00 0.39  0.68 0.33 0.70  0.32
    ## CLSK 0.39 1.00  0.40 0.20 0.25  0.13
    ## RIOT 0.68 0.40  1.00 0.46 0.40 -0.02
    ## CIFR 0.33 0.20  0.46 1.00 0.47  0.10
    ## HUT  0.70 0.25  0.40 0.47 1.00  0.28
    ## BTDR 0.32 0.13 -0.02 0.10 0.28  1.00

``` r
# Create a correlation plot for the residuals
corrplot(cor(BFM_resids), method="color")
```

![](README_files/figure-gfm/bfmresiduals-1.png)<!-- -->

#### Hashrate-Factor Models

Now let’s add the annualized hashrate growth as a third factor to the
mining stock regressions. This will allow us to analyze the impact of
the hashrate on the mining stocks.

``` r
# Hashrate-Factor Model Regressions
HFM_MARA = lm(MARA~SPY+BTC+Hashrate, data=monthly_nominal_final)
summary(HFM_MARA)
```

    ## 
    ## Call:
    ## lm(formula = MARA ~ SPY + BTC + Hashrate, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -681.98 -198.10    4.57  164.98  639.95 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -76.6029    83.7901  -0.914 0.368405    
    ## SPY           2.5284     1.0279   2.460 0.020333 *  
    ## BTC           1.2992     0.3232   4.020 0.000399 ***
    ## Hashrate      0.3300     0.9782   0.337 0.738348    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 303 on 28 degrees of freedom
    ## Multiple R-squared:  0.6648, Adjusted R-squared:  0.6289 
    ## F-statistic: 18.51 on 3 and 28 DF,  p-value: 8.112e-07

``` r
HFM_CLSK = lm(CLSK~SPY+BTC+Hashrate, data=monthly_nominal_final)
summary(HFM_CLSK)
```

    ## 
    ## Call:
    ## lm(formula = CLSK ~ SPY + BTC + Hashrate, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -463.02 -244.57  -14.28  247.47  473.36 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   8.5584    80.0614   0.107 0.915632    
    ## SPY           1.0277     0.9821   1.046 0.304313    
    ## BTC           1.2784     0.3088   4.140 0.000289 ***
    ## Hashrate     -0.3840     0.9346  -0.411 0.684337    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 289.5 on 28 degrees of freedom
    ## Multiple R-squared:  0.5647, Adjusted R-squared:  0.518 
    ## F-statistic: 12.11 on 3 and 28 DF,  p-value: 2.93e-05

``` r
HFM_RIOT = lm(RIOT~SPY+BTC+Hashrate, data=monthly_nominal_final)
summary(HFM_RIOT)
```

    ## 
    ## Call:
    ## lm(formula = RIOT ~ SPY + BTC + Hashrate, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -407.8 -190.6   30.3  183.5  597.5 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -53.9730    82.0562  -0.658  0.51607   
    ## SPY           1.5179     1.0066   1.508  0.14276   
    ## BTC           1.0791     0.3165   3.409  0.00199 **
    ## Hashrate     -0.2578     0.9579  -0.269  0.78983   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 296.7 on 28 degrees of freedom
    ## Multiple R-squared:  0.5248, Adjusted R-squared:  0.4739 
    ## F-statistic: 10.31 on 3 and 28 DF,  p-value: 9.672e-05

``` r
HFM_CIFR = lm(CIFR~SPY+BTC+Hashrate, data=monthly_nominal_final)
summary(HFM_CIFR)
```

    ## 
    ## Call:
    ## lm(formula = CIFR ~ SPY + BTC + Hashrate, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -708.15 -284.43   -3.59  202.72  542.27 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -18.5511    90.7476  -0.204  0.83950   
    ## SPY           0.4008     1.1132   0.360  0.72153   
    ## BTC           1.0389     0.3500   2.968  0.00608 **
    ## Hashrate     -0.4504     1.0594  -0.425  0.67394   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 328.1 on 28 degrees of freedom
    ## Multiple R-squared:  0.3594, Adjusted R-squared:  0.2907 
    ## F-statistic: 5.236 on 3 and 28 DF,  p-value: 0.005382

``` r
HFM_HUT = lm(HUT~SPY+BTC+Hashrate, data=monthly_nominal_final)
summary(HFM_HUT)
```

    ## 
    ## Call:
    ## lm(formula = HUT ~ SPY + BTC + Hashrate, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -640.36 -154.41   -8.72  212.81  438.89 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -143.6787    76.5524  -1.877 0.070993 .  
    ## SPY            0.9839     0.9391   1.048 0.303741    
    ## BTC            1.2095     0.2953   4.096 0.000324 ***
    ## Hashrate       1.2295     0.8937   1.376 0.179788    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 276.8 on 28 degrees of freedom
    ## Multiple R-squared:  0.6246, Adjusted R-squared:  0.5843 
    ## F-statistic: 15.53 on 3 and 28 DF,  p-value: 3.862e-06

``` r
HFM_BTDR = lm(BTDR~SPY+BTC+Hashrate, data=monthly_nominal_final)
summary(HFM_BTDR)
```

    ## 
    ## Call:
    ## lm(formula = BTDR ~ SPY + BTC + Hashrate, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -918.76 -170.74   50.47  112.17  906.97 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  -0.1349    99.0610  -0.001   0.9989  
    ## SPY           2.1780     1.2152   1.792   0.0839 .
    ## BTC          -0.4330     0.3821  -1.133   0.2668  
    ## Hashrate     -0.3129     1.1564  -0.271   0.7887  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 358.2 on 28 degrees of freedom
    ## Multiple R-squared:  0.1121, Adjusted R-squared:  0.01694 
    ## F-statistic: 1.178 on 3 and 28 DF,  p-value: 0.3358

Since that doesn’t seem to add much explanatory power to any of those
models, let’s just skip those residuals and correlations.

#### Difficulty-Factor Models

Lastly, let’s see if the mining difficulty can be more effectively than
the hashrate growth as a third factor. Most likely not since the
difficulty has a very high correlation with the hashrate. But we can
check anyway.

``` r
# Difficulty-Factor Model Regressions
DFM_MARA = lm(MARA~SPY+BTC+Difficulty, data=monthly_nominal_final)
summary(DFM_MARA)
```

    ## 
    ## Call:
    ## lm(formula = MARA ~ SPY + BTC + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -684.58 -181.48   49.94  203.50  552.02 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -23.4732    84.3057  -0.278  0.78273    
    ## SPY           2.4566     1.0255   2.396  0.02352 *  
    ## BTC           1.3752     0.3080   4.465  0.00012 ***
    ## Difficulty   -0.4895     1.0067  -0.486  0.63056    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 302.3 on 28 degrees of freedom
    ## Multiple R-squared:  0.6663, Adjusted R-squared:  0.6305 
    ## F-statistic: 18.63 on 3 and 28 DF,  p-value: 7.64e-07

``` r
DFM_CLSK = lm(CLSK~SPY+BTC+Difficulty, data=monthly_nominal_final)
summary(DFM_CLSK)
```

    ## 
    ## Call:
    ## lm(formula = CLSK ~ SPY + BTC + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -466.87 -233.49  -30.21  232.62  480.26 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  16.7126    80.5578   0.207 0.837151    
    ## SPY           1.0180     0.9799   1.039 0.307758    
    ## BTC           1.2685     0.2943   4.310 0.000182 ***
    ## Difficulty   -0.5171     0.9619  -0.538 0.595130    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 288.9 on 28 degrees of freedom
    ## Multiple R-squared:  0.5665, Adjusted R-squared:  0.5201 
    ## F-statistic:  12.2 on 3 and 28 DF,  p-value: 2.765e-05

``` r
DFM_RIOT = lm(RIOT~SPY+BTC+Difficulty, data=monthly_nominal_final)
summary(DFM_RIOT)
```

    ## 
    ## Call:
    ## lm(formula = RIOT ~ SPY + BTC + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -413.82 -210.32   17.65  175.03  599.46 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -32.3792    82.3065  -0.393  0.69701   
    ## SPY           1.4899     1.0012   1.488  0.14789   
    ## BTC           1.0906     0.3007   3.627  0.00113 **
    ## Difficulty   -0.5975     0.9828  -0.608  0.54813   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 295.1 on 28 degrees of freedom
    ## Multiple R-squared:  0.5298, Adjusted R-squared:  0.4794 
    ## F-statistic: 10.52 on 3 and 28 DF,  p-value: 8.381e-05

``` r
DFM_CIFR = lm(CIFR~SPY+BTC+Difficulty, data=monthly_nominal_final)
summary(DFM_CIFR)
```

    ## 
    ## Call:
    ## lm(formula = CIFR ~ SPY + BTC + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -705.02 -266.81   12.17  228.91  488.73 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  21.8180    90.1795   0.242  0.81059   
    ## SPY           0.3483     1.0969   0.317  0.75323   
    ## BTC           1.0619     0.3295   3.223  0.00321 **
    ## Difficulty   -1.0850     1.0768  -1.008  0.32227   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 323.4 on 28 degrees of freedom
    ## Multiple R-squared:  0.3778, Adjusted R-squared:  0.3111 
    ## F-statistic: 5.667 on 3 and 28 DF,  p-value: 0.003653

``` r
DFM_HUT = lm(HUT~SPY+BTC+Difficulty, data=monthly_nominal_final)
summary(DFM_HUT)
```

    ## 
    ## Call:
    ## lm(formula = HUT ~ SPY + BTC + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -665.16 -194.67  -11.29  196.12  445.36 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -42.4707    79.5952  -0.534    0.598    
    ## SPY           0.8452     0.9682   0.873    0.390    
    ## BTC           1.3841     0.2908   4.759 5.35e-05 ***
    ## Difficulty   -0.3215     0.9504  -0.338    0.738    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 285.4 on 28 degrees of freedom
    ## Multiple R-squared:  0.6008, Adjusted R-squared:  0.558 
    ## F-statistic: 14.05 on 3 and 28 DF,  p-value: 8.957e-06

``` r
DFM_BTDR = lm(BTDR~SPY+BTC+Difficulty, data=monthly_nominal_final)
summary(DFM_BTDR)
```

    ## 
    ## Call:
    ## lm(formula = BTDR ~ SPY + BTC + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -910.95 -172.26   26.88   87.99  905.13 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -47.8542    99.7938  -0.480   0.6353  
    ## SPY           2.2426     1.2139   1.847   0.0753 .
    ## BTC          -0.5021     0.3646  -1.377   0.1794  
    ## Difficulty    0.4229     1.1916   0.355   0.7253  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 357.9 on 28 degrees of freedom
    ## Multiple R-squared:  0.1137, Adjusted R-squared:  0.01879 
    ## F-statistic: 1.198 on 3 and 28 DF,  p-value: 0.3287

As expected, those don’t add much explanatory power either. So let’s
skip the residuals and correlations again.

#### Four-Factor Model

Lastly, let’s see if the four-factor model (SPY, BTC, Hashrate,
Difficulty) can provide a better fit for the mining stocks.

``` r
# Four-Factor Model Regressions
FFM_MARA = lm(MARA~SPY+BTC+Hashrate+Difficulty, data=monthly_nominal_final)
summary(FFM_MARA)
```

    ## 
    ## Call:
    ## lm(formula = MARA ~ SPY + BTC + Hashrate + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -595.87 -173.38   54.07  178.96  607.13 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -48.1340    88.3755  -0.545 0.590463    
    ## SPY           2.4933     1.0281   2.425 0.022263 *  
    ## BTC           1.2832     0.3235   3.967 0.000483 ***
    ## Hashrate      1.2974     1.3687   0.948 0.351610    
    ## Difficulty   -1.4258     1.4117  -1.010 0.321455    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 302.9 on 27 degrees of freedom
    ## Multiple R-squared:  0.677,  Adjusted R-squared:  0.6292 
    ## F-statistic: 14.15 on 4 and 27 DF,  p-value: 2.397e-06

``` r
FFM_CLSK = lm(CLSK~SPY+BTC+Hashrate+Difficulty, data=monthly_nominal_final)
summary(FFM_CLSK)
```

    ## 
    ## Call:
    ## lm(formula = CLSK ~ SPY + BTC + Hashrate + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -464.91 -236.11  -28.93  234.15  479.52 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 17.94707   85.83638   0.209 0.835952    
    ## SPY          1.01614    0.99855   1.018 0.317891    
    ## BTC          1.27311    0.31417   4.052 0.000385 ***
    ## Hashrate    -0.06494    1.32940  -0.049 0.961397    
    ## Difficulty  -0.47021    1.37110  -0.343 0.734298    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 294.2 on 27 degrees of freedom
    ## Multiple R-squared:  0.5666, Adjusted R-squared:  0.5024 
    ## F-statistic: 8.823 on 4 and 27 DF,  p-value: 0.0001085

``` r
FFM_RIOT = lm(RIOT~SPY+BTC+Hashrate+Difficulty, data=monthly_nominal_final)
summary(FFM_RIOT)
```

    ## 
    ## Call:
    ## lm(formula = RIOT ~ SPY + BTC + Hashrate + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -414.26 -197.26   -7.03  173.20  606.07 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -37.8761    87.6299  -0.432  0.66901   
    ## SPY           1.4981     1.0194   1.470  0.15324   
    ## BTC           1.0701     0.3207   3.336  0.00248 **
    ## Hashrate      0.2892     1.3572   0.213  0.83287   
    ## Difficulty   -0.8062     1.3997  -0.576  0.56942   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 300.3 on 27 degrees of freedom
    ## Multiple R-squared:  0.5306, Adjusted R-squared:  0.461 
    ## F-statistic:  7.63 on 4 and 27 DF,  p-value: 0.0003005

``` r
FFM_CIFR = lm(CIFR~SPY+BTC+Hashrate+Difficulty, data=monthly_nominal_final)
summary(FFM_CIFR)
```

    ## 
    ## Call:
    ## lm(formula = CIFR ~ SPY + BTC + Hashrate + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -709.19 -280.90    2.73  240.68  496.54 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  11.1784    95.8407   0.117  0.90801   
    ## SPY           0.3641     1.1149   0.327  0.74651   
    ## BTC           1.0223     0.3508   2.914  0.00708 **
    ## Hashrate      0.5597     1.4843   0.377  0.70905   
    ## Difficulty   -1.4889     1.5309  -0.973  0.33939   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 328.5 on 27 degrees of freedom
    ## Multiple R-squared:  0.3811, Adjusted R-squared:  0.2894 
    ## F-statistic: 4.156 on 4 and 27 DF,  p-value: 0.009456

``` r
FFM_HUT = lm(HUT~SPY+BTC+Hashrate+Difficulty, data=monthly_nominal_final)
summary(FFM_HUT)
```

    ## 
    ## Call:
    ## lm(formula = HUT ~ SPY + BTC + Hashrate + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -640.18 -187.31   -2.28  143.18  470.92 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -96.3871    77.1442  -1.249 0.222227    
    ## SPY           0.9255     0.8974   1.031 0.311549    
    ## BTC           1.1831     0.2824   4.190 0.000267 ***
    ## Hashrate      2.8364     1.1948   2.374 0.024966 *  
    ## Difficulty   -2.3685     1.2323  -1.922 0.065209 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 264.4 on 27 degrees of freedom
    ## Multiple R-squared:  0.6697, Adjusted R-squared:  0.6208 
    ## F-statistic: 13.69 on 4 and 27 DF,  p-value: 3.208e-06

``` r
FFM_BTDR = lm(BTDR~SPY+BTC+Hashrate+Difficulty, data=monthly_nominal_final)
summary(FFM_BTDR)
```

    ## 
    ## Call:
    ## lm(formula = BTDR ~ SPY + BTC + Hashrate + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -914.79 -159.20   34.11  146.23  890.33 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -25.5147   105.3300  -0.242   0.8104  
    ## SPY           2.2093     1.2253   1.803   0.0826 .
    ## BTC          -0.4188     0.3855  -1.086   0.2870  
    ## Hashrate     -1.1752     1.6313  -0.720   0.4775  
    ## Difficulty    1.2711     1.6825   0.755   0.4565  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 361 on 27 degrees of freedom
    ## Multiple R-squared:  0.1305, Adjusted R-squared:  0.001635 
    ## F-statistic: 1.013 on 4 and 27 DF,  p-value: 0.4183

## Generating Summaries by Stock

To package up our results in a more digestible format, let’s create some
nicely formatted tables using the stargazer package. This package lets
us output some nice text-based tables here in the R output, but also can
generate LaTeX code for inclusion in a more formal report or
presentation. See below for examples of both applications. These will
generate the .tex files in the Figures folder. Then you can compile the
Tables.tex file to generate Tables.pdf with the formally typeset tables.

### Summary Statistics

Let’s start by generating summary statistics for the final monthly
dataset. We’ll generate summary statistics for the nominal returns, real
returns, and excess returns. These tables will include the mean,
standard deviation, minimum, 25th percentile, median, 75th percentile,
and maximum values for each asset.

``` r
# Generate summary statistics for the final nominal returns
stargazer(monthly_nominal_final, 
          summary=TRUE, 
          type='text')
```

    ## 
    ## ===================================================
    ## Statistic  N   Mean   St. Dev.    Min        Max   
    ## ---------------------------------------------------
    ## INF        32  5.174   3.736     -0.077    14.887  
    ## RF         32  3.208   1.041     1.283      4.798  
    ## BTC        32 20.199  218.832   -569.177   435.210 
    ## MARA       32 -7.569  497.362   -875.587  1,066.745
    ## CLSK       32 16.942  416.996   -774.014   877.120 
    ## RIOT       32 -37.136 409.067   -883.316   737.936 
    ## CIFR       32 -24.583 389.626   -687.697   914.568 
    ## HUT        32 -28.569 429.338   -781.097   984.047 
    ## BTDR       32 -12.165 361.266  -1,136.203  987.126 
    ## SPY        32  8.149   64.095   -116.402   105.709 
    ## Hashrate   32 67.230   60.657   -64.583    227.352 
    ## Difficulty 32 65.153   55.612   -56.532    198.738 
    ## ---------------------------------------------------

``` r
# Generate and save output to a LaTeX file
stargazer(monthly_nominal_final, 
          summary=TRUE,
          type='latex', 
          font.size="large",
          out="Figures/SummaryStats_nominal.tex",
          title="Summary Statistics for the Final Monthly Dataset. Asset nominal returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).", 
          label="SummaryStats_nominal")
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Apr 23, 2024 - 18:51:17
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Summary Statistics for the Final Monthly Dataset. Asset nominal returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{SummaryStats_nominal} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lccccc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ## Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
    ## \hline \\[-1.8ex] 
    ## INF & 32 & 5.174 & 3.736 & $-$0.077 & 14.887 \\ 
    ## RF & 32 & 3.208 & 1.041 & 1.283 & 4.798 \\ 
    ## BTC & 32 & 20.199 & 218.832 & $-$569.177 & 435.210 \\ 
    ## MARA & 32 & $-$7.569 & 497.362 & $-$875.587 & 1,066.745 \\ 
    ## CLSK & 32 & 16.942 & 416.996 & $-$774.014 & 877.120 \\ 
    ## RIOT & 32 & $-$37.136 & 409.067 & $-$883.316 & 737.936 \\ 
    ## CIFR & 32 & $-$24.583 & 389.626 & $-$687.697 & 914.568 \\ 
    ## HUT & 32 & $-$28.569 & 429.338 & $-$781.097 & 984.047 \\ 
    ## BTDR & 32 & $-$12.165 & 361.266 & $-$1,136.203 & 987.126 \\ 
    ## SPY & 32 & 8.149 & 64.095 & $-$116.402 & 105.709 \\ 
    ## Hashrate & 32 & 67.230 & 60.657 & $-$64.583 & 227.352 \\ 
    ## Difficulty & 32 & 65.153 & 55.612 & $-$56.532 & 198.738 \\ 
    ## \hline \\[-1.8ex] 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(monthly_real_final, 
          summary=TRUE, 
          type='text')
```

    ## 
    ## ===================================================
    ## Statistic  N   Mean   St. Dev.    Min        Max   
    ## ---------------------------------------------------
    ## RF         32 -1.736   3.968    -10.222     3.813  
    ## BTC        32 16.405  204.657   -508.382   408.305 
    ## MARA       32 -8.440  475.903   -851.841  1,067.647
    ## CLSK       32 12.534  393.391   -741.655   828.001 
    ## RIOT       32 -36.785 389.376   -845.710   689.131 
    ## CIFR       32 -23.668 370.147   -611.544   855.476 
    ## HUT        32 -28.606 407.024   -738.850   920.908 
    ## BTDR       32 -17.055 352.871  -1,126.466  960.393 
    ## SPY        32  3.250   61.583   -115.682   105.868 
    ## Hashrate   32 67.230   60.657   -64.583    227.352 
    ## Difficulty 32 65.153   55.612   -56.532    198.738 
    ## ---------------------------------------------------

``` r
# Generate and save output to a LaTeX file
stargazer(monthly_real_final, 
          summary=TRUE,
          type='latex', 
          font.size="large",
          out="Figures/SummaryStats_real.tex",
          title="Summary Statistics for the Final Monthly Dataset. Asset real returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).", 
          label="SummaryStats_real")
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Apr 23, 2024 - 18:51:17
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Summary Statistics for the Final Monthly Dataset. Asset real returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{SummaryStats_real} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lccccc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ## Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
    ## \hline \\[-1.8ex] 
    ## RF & 32 & $-$1.736 & 3.968 & $-$10.222 & 3.813 \\ 
    ## BTC & 32 & 16.405 & 204.657 & $-$508.382 & 408.305 \\ 
    ## MARA & 32 & $-$8.440 & 475.903 & $-$851.841 & 1,067.647 \\ 
    ## CLSK & 32 & 12.534 & 393.391 & $-$741.655 & 828.001 \\ 
    ## RIOT & 32 & $-$36.785 & 389.376 & $-$845.710 & 689.131 \\ 
    ## CIFR & 32 & $-$23.668 & 370.147 & $-$611.544 & 855.476 \\ 
    ## HUT & 32 & $-$28.606 & 407.024 & $-$738.850 & 920.908 \\ 
    ## BTDR & 32 & $-$17.055 & 352.871 & $-$1,126.466 & 960.393 \\ 
    ## SPY & 32 & 3.250 & 61.583 & $-$115.682 & 105.868 \\ 
    ## Hashrate & 32 & 67.230 & 60.657 & $-$64.583 & 227.352 \\ 
    ## Difficulty & 32 & 65.153 & 55.612 & $-$56.532 & 198.738 \\ 
    ## \hline \\[-1.8ex] 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(monthly_excess_final, 
          summary=TRUE, 
          type='text')
```

    ## 
    ## ===================================================
    ## Statistic  N   Mean   St. Dev.    Min        Max   
    ## ---------------------------------------------------
    ## BTC        32 18.141  203.464   -498.161   409.336 
    ## MARA       32 -6.704  475.113   -852.552  1,064.672
    ## CLSK       32 14.270  392.870   -739.470   829.032 
    ## RIOT       32 -35.049 388.384   -843.524   691.630 
    ## CIFR       32 -21.932 368.702   -601.323   857.974 
    ## HUT        32 -26.870 406.255   -739.560   923.406 
    ## BTDR       32 -15.319 353.164  -1,130.279  959.196 
    ## SPY        32  4.985   60.870   -114.656   102.893 
    ## Hashrate   32 67.230   60.657   -64.583    227.352 
    ## Difficulty 32 65.153   55.612   -56.532    198.738 
    ## ---------------------------------------------------

``` r
# Generate and save output to a LaTeX file
stargazer(monthly_excess_final, 
          summary=TRUE,
          type='latex', 
          font.size="large",
          out="Figures/SummaryStats_excess.tex",
          title="Summary Statistics for the Final Monthly Dataset. Asset excess returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).", 
          label="SummaryStats_excess")
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Apr 23, 2024 - 18:51:17
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Summary Statistics for the Final Monthly Dataset. Asset excess returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{SummaryStats_excess} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lccccc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ## Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
    ## \hline \\[-1.8ex] 
    ## BTC & 32 & 18.141 & 203.464 & $-$498.161 & 409.336 \\ 
    ## MARA & 32 & $-$6.704 & 475.113 & $-$852.552 & 1,064.672 \\ 
    ## CLSK & 32 & 14.270 & 392.870 & $-$739.470 & 829.032 \\ 
    ## RIOT & 32 & $-$35.049 & 388.384 & $-$843.524 & 691.630 \\ 
    ## CIFR & 32 & $-$21.932 & 368.702 & $-$601.323 & 857.974 \\ 
    ## HUT & 32 & $-$26.870 & 406.255 & $-$739.560 & 923.406 \\ 
    ## BTDR & 32 & $-$15.319 & 353.164 & $-$1,130.279 & 959.196 \\ 
    ## SPY & 32 & 4.985 & 60.870 & $-$114.656 & 102.893 \\ 
    ## Hashrate & 32 & 67.230 & 60.657 & $-$64.583 & 227.352 \\ 
    ## Difficulty & 32 & 65.153 & 55.612 & $-$56.532 & 198.738 \\ 
    ## \hline \\[-1.8ex] 
    ## \end{tabular} 
    ## \end{table}

### Model Results by Company

Now let’s generate tables for the model results for each of the mining
stocks. These tables will include the coefficients, standard errors,
t-statistics, and p-values for each factor in the model. After
aggregating the model results for each stock, we can omit residual
standard errors and F-statistics from the tables to keep them concise.

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_MARA, BFM_MARA, HFM_MARA, DFM_MARA, FFM_MARA, 
          type='text',
          omit.stat=c("ser","f"))
```

    ## 
    ## =========================================================
    ##                          Dependent variable:             
    ##              --------------------------------------------
    ##                                  MARA                    
    ##                (1)      (2)      (3)      (4)      (5)   
    ## ---------------------------------------------------------
    ## SPY          5.057*** 2.499**  2.528**  2.457**  2.493** 
    ##              (1.075)  (1.008)  (1.028)  (1.025)  (1.028) 
    ##                                                          
    ## BTC                   1.340*** 1.299*** 1.375*** 1.283***
    ##                       (0.295)  (0.323)  (0.308)  (0.323) 
    ##                                                          
    ## Hashrate                        0.330             1.297  
    ##                                (0.978)           (1.369) 
    ##                                                          
    ## Difficulty                               -0.490   -1.426 
    ##                                         (1.007)  (1.412) 
    ##                                                          
    ## Constant     -48.777  -54.994  -76.603  -23.473  -48.134 
    ##              (68.357) (53.190) (83.790) (84.306) (88.376)
    ##                                                          
    ## ---------------------------------------------------------
    ## Observations    32       32       32       32       32   
    ## R2            0.425    0.663    0.665    0.666    0.677  
    ## Adjusted R2   0.405    0.640    0.629    0.631    0.629  
    ## =========================================================
    ## Note:                         *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_MARA, BFM_MARA, HFM_MARA, DFM_MARA, FFM_MARA, 
          type='latex', 
          font.size="large",
          out="Figures/ModelResults_MARA.tex",
          title="Factor Model Results for Marathon Digital Holdings (MARA). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_MARA",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Apr 23, 2024 - 18:51:18
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Marathon Digital Holdings (MARA). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_MARA} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lccccc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{MARA} \\ 
    ## \\[-1.8ex] & (1) & (2) & (3) & (4) & (5)\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 5.057$^{***}$ & 2.499$^{**}$ & 2.528$^{**}$ & 2.457$^{**}$ & 2.493$^{**}$ \\ 
    ##   & (1.075) & (1.008) & (1.028) & (1.025) & (1.028) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & 1.340$^{***}$ & 1.299$^{***}$ & 1.375$^{***}$ & 1.283$^{***}$ \\ 
    ##   &  & (0.295) & (0.323) & (0.308) & (0.323) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & 0.330 &  & 1.297 \\ 
    ##   &  &  & (0.978) &  & (1.369) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & $-$0.490 & $-$1.426 \\ 
    ##   &  &  &  & (1.007) & (1.412) \\ 
    ##   & & & & & \\ 
    ##  Constant & $-$48.777 & $-$54.994 & $-$76.603 & $-$23.473 & $-$48.134 \\ 
    ##   & (68.357) & (53.190) & (83.790) & (84.306) & (88.376) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & 32 & 32 & 32 & 32 & 32 \\ 
    ## R$^{2}$ & 0.425 & 0.663 & 0.665 & 0.666 & 0.677 \\ 
    ## Adjusted R$^{2}$ & 0.405 & 0.640 & 0.629 & 0.631 & 0.629 \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_CLSK, BFM_CLSK, HFM_CLSK, DFM_CLSK, FFM_CLSK, 
          type='text',
          omit.stat=c("ser","f"))
```

    ## 
    ## =========================================================
    ##                          Dependent variable:             
    ##              --------------------------------------------
    ##                                  CLSK                    
    ##                (1)      (2)      (3)      (4)      (5)   
    ## ---------------------------------------------------------
    ## SPY          3.413***  1.062    1.028    1.018    1.016  
    ##              (1.011)  (0.964)  (0.982)  (0.980)  (0.999) 
    ##                                                          
    ## BTC                   1.231*** 1.278*** 1.269*** 1.273***
    ##                       (0.282)  (0.309)  (0.294)  (0.314) 
    ##                                                          
    ## Hashrate                        -0.384            -0.065 
    ##                                (0.935)           (1.329) 
    ##                                                          
    ## Difficulty                               -0.517   -0.470 
    ##                                         (0.962)  (1.371) 
    ##                                                          
    ## Constant     -10.871  -16.583   8.558    16.713   17.947 
    ##              (64.326) (50.873) (80.061) (80.558) (85.836)
    ##                                                          
    ## ---------------------------------------------------------
    ## Observations    32       32       32       32       32   
    ## R2            0.275    0.562    0.565    0.567    0.567  
    ## Adjusted R2   0.251    0.532    0.518    0.520    0.502  
    ## =========================================================
    ## Note:                         *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_CLSK, BFM_CLSK, HFM_CLSK, DFM_CLSK, FFM_CLSK, 
          type='latex', 
          font.size="large",
          out="Figures/ModelResults_CLSK.tex",
          title="Factor Model Results for Cleanspark (CLSK). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_CLSK",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Apr 23, 2024 - 18:51:19
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Cleanspark (CLSK). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_CLSK} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lccccc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{CLSK} \\ 
    ## \\[-1.8ex] & (1) & (2) & (3) & (4) & (5)\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 3.413$^{***}$ & 1.062 & 1.028 & 1.018 & 1.016 \\ 
    ##   & (1.011) & (0.964) & (0.982) & (0.980) & (0.999) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & 1.231$^{***}$ & 1.278$^{***}$ & 1.269$^{***}$ & 1.273$^{***}$ \\ 
    ##   &  & (0.282) & (0.309) & (0.294) & (0.314) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & $-$0.384 &  & $-$0.065 \\ 
    ##   &  &  & (0.935) &  & (1.329) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & $-$0.517 & $-$0.470 \\ 
    ##   &  &  &  & (0.962) & (1.371) \\ 
    ##   & & & & & \\ 
    ##  Constant & $-$10.871 & $-$16.583 & 8.558 & 16.713 & 17.947 \\ 
    ##   & (64.326) & (50.873) & (80.061) & (80.558) & (85.836) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & 32 & 32 & 32 & 32 & 32 \\ 
    ## R$^{2}$ & 0.275 & 0.562 & 0.565 & 0.567 & 0.567 \\ 
    ## Adjusted R$^{2}$ & 0.251 & 0.532 & 0.518 & 0.520 & 0.502 \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_RIOT, BFM_RIOT, HFM_RIOT, DFM_RIOT, FFM_RIOT, 
          type='text',
          omit.stat=c("ser","f"))
```

    ## 
    ## =========================================================
    ##                          Dependent variable:             
    ##              --------------------------------------------
    ##                                  RIOT                    
    ##                (1)      (2)      (3)      (4)      (5)   
    ## ---------------------------------------------------------
    ## SPY          3.541***  1.541    1.518    1.490    1.498  
    ##              (0.969)  (0.987)  (1.007)  (1.001)  (1.019) 
    ##                                                          
    ## BTC                   1.047*** 1.079*** 1.091*** 1.070***
    ##                       (0.289)  (0.317)  (0.301)  (0.321) 
    ##                                                          
    ## Hashrate                        -0.258            0.289  
    ##                                (0.958)           (1.357) 
    ##                                                          
    ## Difficulty                               -0.597   -0.806 
    ##                                         (0.983)  (1.400) 
    ##                                                          
    ## Constant     -65.992  -70.852  -53.973  -32.379  -37.876 
    ##              (61.666) (52.051) (82.056) (82.306) (87.630)
    ##                                                          
    ## ---------------------------------------------------------
    ## Observations    32       32       32       32       32   
    ## R2            0.308    0.524    0.525    0.530    0.531  
    ## Adjusted R2   0.285    0.491    0.474    0.479    0.461  
    ## =========================================================
    ## Note:                         *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_RIOT, BFM_RIOT, HFM_RIOT, DFM_RIOT, FFM_RIOT, 
          type='latex', 
          font.size="large",
          out="Figures/ModelResults_RIOT.tex",
          title="Factor Model Results for Riot Blockchain (RIOT). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_RIOT",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Apr 23, 2024 - 18:51:20
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Riot Blockchain (RIOT). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_RIOT} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lccccc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{RIOT} \\ 
    ## \\[-1.8ex] & (1) & (2) & (3) & (4) & (5)\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 3.541$^{***}$ & 1.541 & 1.518 & 1.490 & 1.498 \\ 
    ##   & (0.969) & (0.987) & (1.007) & (1.001) & (1.019) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & 1.047$^{***}$ & 1.079$^{***}$ & 1.091$^{***}$ & 1.070$^{***}$ \\ 
    ##   &  & (0.289) & (0.317) & (0.301) & (0.321) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & $-$0.258 &  & 0.289 \\ 
    ##   &  &  & (0.958) &  & (1.357) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & $-$0.597 & $-$0.806 \\ 
    ##   &  &  &  & (0.983) & (1.400) \\ 
    ##   & & & & & \\ 
    ##  Constant & $-$65.992 & $-$70.852 & $-$53.973 & $-$32.379 & $-$37.876 \\ 
    ##   & (61.666) & (52.051) & (82.056) & (82.306) & (87.630) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & 32 & 32 & 32 & 32 & 32 \\ 
    ## R$^{2}$ & 0.308 & 0.524 & 0.525 & 0.530 & 0.531 \\ 
    ## Adjusted R$^{2}$ & 0.285 & 0.491 & 0.474 & 0.479 & 0.461 \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_CIFR, BFM_CIFR, HFM_CIFR, DFM_CIFR, FFM_CIFR, 
          type='text',
          omit.stat=c("ser","f"))
```

    ## 
    ## =========================================================
    ##                          Dependent variable:             
    ##              --------------------------------------------
    ##                                  CIFR                    
    ##                (1)      (2)      (3)      (4)      (5)   
    ## ---------------------------------------------------------
    ## SPY          2.319**   0.441    0.401    0.348    0.364  
    ##              (1.026)  (1.093)  (1.113)  (1.097)  (1.115) 
    ##                                                          
    ## BTC                   0.983*** 1.039*** 1.062*** 1.022***
    ##                       (0.320)  (0.350)  (0.329)  (0.351) 
    ##                                                          
    ## Hashrate                        -0.450            0.560  
    ##                                (1.059)           (1.484) 
    ##                                                          
    ## Difficulty                               -1.085   -1.489 
    ##                                         (1.077)  (1.531) 
    ##                                                          
    ## Constant     -43.483  -48.046  -18.551   21.818   11.178 
    ##              (65.258) (57.675) (90.748) (90.179) (95.841)
    ##                                                          
    ## ---------------------------------------------------------
    ## Observations    32       32       32       32       32   
    ## R2            0.146    0.355    0.359    0.378    0.381  
    ## Adjusted R2   0.117    0.311    0.291    0.311    0.289  
    ## =========================================================
    ## Note:                         *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_CIFR, BFM_CIFR, HFM_CIFR, DFM_CIFR, FFM_CIFR, 
          type='latex', 
          font.size="large",
          out="Figures/ModelResults_CIFR.tex",
          title="Factor Model Results for Cipher Mining (CIFR). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_CIFR",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Apr 23, 2024 - 18:51:21
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Cipher Mining (CIFR). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_CIFR} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lccccc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{CIFR} \\ 
    ## \\[-1.8ex] & (1) & (2) & (3) & (4) & (5)\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 2.319$^{**}$ & 0.441 & 0.401 & 0.348 & 0.364 \\ 
    ##   & (1.026) & (1.093) & (1.113) & (1.097) & (1.115) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & 0.983$^{***}$ & 1.039$^{***}$ & 1.062$^{***}$ & 1.022$^{***}$ \\ 
    ##   &  & (0.320) & (0.350) & (0.329) & (0.351) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & $-$0.450 &  & 0.560 \\ 
    ##   &  &  & (1.059) &  & (1.484) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & $-$1.085 & $-$1.489 \\ 
    ##   &  &  &  & (1.077) & (1.531) \\ 
    ##   & & & & & \\ 
    ##  Constant & $-$43.483 & $-$48.046 & $-$18.551 & 21.818 & 11.178 \\ 
    ##   & (65.258) & (57.675) & (90.748) & (90.179) & (95.841) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & 32 & 32 & 32 & 32 & 32 \\ 
    ## R$^{2}$ & 0.146 & 0.355 & 0.359 & 0.378 & 0.381 \\ 
    ## Adjusted R$^{2}$ & 0.117 & 0.311 & 0.291 & 0.311 & 0.289 \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_HUT, BFM_HUT, HFM_HUT, DFM_HUT, FFM_HUT, 
          type='text',
          omit.stat=c("ser","f"))
```

    ## 
    ## ==========================================================
    ##                           Dependent variable:             
    ##              ---------------------------------------------
    ##                                   HUT                     
    ##                (1)      (2)       (3)      (4)      (5)   
    ## ----------------------------------------------------------
    ## SPY          3.471***  0.873     0.984    0.845    0.926  
    ##              (1.046)  (0.950)   (0.939)  (0.968)  (0.897) 
    ##                                                           
    ## BTC                   1.361*** 1.210***  1.384*** 1.183***
    ##                       (0.278)   (0.295)  (0.291)  (0.282) 
    ##                                                           
    ## Hashrate                         1.230            2.836** 
    ##                                 (0.894)           (1.195) 
    ##                                                           
    ## Difficulty                                -0.321  -2.368* 
    ##                                          (0.950)  (1.232) 
    ##                                                           
    ## Constant     -56.855  -63.170  -143.679* -42.471  -96.387 
    ##              (66.534) (50.110) (76.552)  (79.595) (77.144)
    ##                                                           
    ## ----------------------------------------------------------
    ## Observations    32       32       32        32       32   
    ## R2            0.269    0.599     0.625    0.601    0.670  
    ## Adjusted R2   0.244    0.572     0.584    0.558    0.621  
    ## ==========================================================
    ## Note:                          *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_HUT, BFM_HUT, HFM_HUT, DFM_HUT, FFM_HUT, 
          type='latex', 
          font.size="large",
          out="Figures/ModelResults_HUT.tex",
          title="Factor Model Results for Hut 8 Mining (HUT). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_HUT",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Apr 23, 2024 - 18:51:22
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Hut 8 Mining (HUT). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_HUT} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lccccc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{HUT} \\ 
    ## \\[-1.8ex] & (1) & (2) & (3) & (4) & (5)\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 3.471$^{***}$ & 0.873 & 0.984 & 0.845 & 0.926 \\ 
    ##   & (1.046) & (0.950) & (0.939) & (0.968) & (0.897) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & 1.361$^{***}$ & 1.210$^{***}$ & 1.384$^{***}$ & 1.183$^{***}$ \\ 
    ##   &  & (0.278) & (0.295) & (0.291) & (0.282) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & 1.230 &  & 2.836$^{**}$ \\ 
    ##   &  &  & (0.894) &  & (1.195) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & $-$0.321 & $-$2.368$^{*}$ \\ 
    ##   &  &  &  & (0.950) & (1.232) \\ 
    ##   & & & & & \\ 
    ##  Constant & $-$56.855 & $-$63.170 & $-$143.679$^{*}$ & $-$42.471 & $-$96.387 \\ 
    ##   & (66.534) & (50.110) & (76.552) & (79.595) & (77.144) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & 32 & 32 & 32 & 32 & 32 \\ 
    ## R$^{2}$ & 0.269 & 0.599 & 0.625 & 0.601 & 0.670 \\ 
    ## Adjusted R$^{2}$ & 0.244 & 0.572 & 0.584 & 0.558 & 0.621 \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_BTDR, BFM_BTDR, HFM_BTDR, DFM_BTDR, FFM_BTDR, 
          type='text',
          omit.stat=c("ser","f"))
```

    ## 
    ## ==========================================================
    ##                           Dependent variable:             
    ##              ---------------------------------------------
    ##                                  BTDR                     
    ##                (1)      (2)      (3)      (4)       (5)   
    ## ----------------------------------------------------------
    ## SPY           1.306    2.206*   2.178*   2.243*   2.209*  
    ##              (1.001)  (1.191)  (1.215)  (1.214)   (1.225) 
    ##                                                           
    ## BTC                    -0.471   -0.433   -0.502   -0.419  
    ##                       (0.349)  (0.382)  (0.365)   (0.386) 
    ##                                                           
    ## Hashrate                        -0.313            -1.175  
    ##                                (1.156)            (1.631) 
    ##                                                           
    ## Difficulty                               0.423     1.271  
    ##                                         (1.192)   (1.682) 
    ##                                                           
    ## Constant     -22.808  -20.621   -0.135  -47.854   -25.515 
    ##              (63.677) (62.839) (99.061) (99.794) (105.330)
    ##                                                           
    ## ----------------------------------------------------------
    ## Observations    32       32       32       32       32    
    ## R2            0.054    0.110    0.112    0.114     0.130  
    ## Adjusted R2   0.022    0.048    0.017    0.019     0.002  
    ## ==========================================================
    ## Note:                          *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_BTDR, BFM_BTDR, HFM_BTDR, DFM_BTDR, FFM_BTDR, 
          type='latex', 
          font.size="large",
          out="Figures/ModelResults_BTDR.tex",
          title="Factor Model Results for Bitdeer (BTDR). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_BTDR",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Apr 23, 2024 - 18:51:23
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Bitdeer (BTDR). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_BTDR} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lccccc} 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{BTDR} \\ 
    ## \\[-1.8ex] & (1) & (2) & (3) & (4) & (5)\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 1.306 & 2.206$^{*}$ & 2.178$^{*}$ & 2.243$^{*}$ & 2.209$^{*}$ \\ 
    ##   & (1.001) & (1.191) & (1.215) & (1.214) & (1.225) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & $-$0.471 & $-$0.433 & $-$0.502 & $-$0.419 \\ 
    ##   &  & (0.349) & (0.382) & (0.365) & (0.386) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & $-$0.313 &  & $-$1.175 \\ 
    ##   &  &  & (1.156) &  & (1.631) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & 0.423 & 1.271 \\ 
    ##   &  &  &  & (1.192) & (1.682) \\ 
    ##   & & & & & \\ 
    ##  Constant & $-$22.808 & $-$20.621 & $-$0.135 & $-$47.854 & $-$25.515 \\ 
    ##   & (63.677) & (62.839) & (99.061) & (99.794) & (105.330) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & 32 & 32 & 32 & 32 & 32 \\ 
    ## R$^{2}$ & 0.054 & 0.110 & 0.112 & 0.114 & 0.130 \\ 
    ## Adjusted R$^{2}$ & 0.022 & 0.048 & 0.017 & 0.019 & 0.002 \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}
