Bitcoin Mining Stock Analysis
================
Last updated: 2024-07-09

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
- The [stargazer package](https://cran.r-project.org/package=stargazer)
  is used to generate formally typeset tables of regression results.
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
list.of.packages = c("quantmod","tidyverse","tseries","corrplot","jsonlite","stargazer","rmarkdown")
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
library(stargazer)
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
    ## Dickey-Fuller = 1.7045, Lag order = 17, p-value = 0.99
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
    ## Dickey-Fuller = -20.544, Lag order = 17, p-value = 0.01
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
    ## Dickey-Fuller = 1.8446, Lag order = 5, p-value = 0.99
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
    ## Dickey-Fuller = -4.1923, Lag order = 5, p-value = 0.01
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
    ## Dickey-Fuller = 2.8301, Lag order = 17, p-value = 0.99
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
    ## Dickey-Fuller = -13.666, Lag order = 17, p-value = 0.01
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
    ## Dickey-Fuller = 1.9686, Lag order = 5, p-value = 0.99
    ## alternative hypothesis: stationary

``` r
adf.test(difficultymonthdf$annDifficultyGrowth[-1])
```

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  difficultymonthdf$annDifficultyGrowth[-1]
    ## Dickey-Fuller = -3.8137, Lag order = 5, p-value = 0.01976
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
    ##       2.92       2.36      54.16     -28.29     -15.62      25.21     -40.61 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##     -19.14     -31.44      16.48      80.12      82.08

``` r
# Compute the average annualized real returns
colMeans(daily_real, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##      -0.46      50.08     -32.05     -20.80      22.43     -39.71     -20.26 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##     -36.32      13.04      80.12      82.08

``` r
# Compute the average annualized excess returns
colMeans(daily_excess, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##      57.96     -34.42     -24.03      17.91     -29.67     -18.28     -29.18 
    ##        SPY   Hashrate Difficulty 
    ##      13.57      80.12      82.08

``` r
# Compute the standard deviation of the annual nominal returns
apply(daily_nominal, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        INF         RF        BTC       MARA       CLSK       RIOT       CIFR 
    ##       3.27       0.97    1353.32    3009.60    3553.03    2625.98    2590.74 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##    2634.41    2044.73     410.90    4389.18     692.03

``` r
# Compute the standard deviation of the annual real returns
apply(daily_real, 2, sd, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##       3.27    1324.64    2925.65    3471.80    2543.17    2455.85    2559.22 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##    1980.86     410.65    4389.18     692.03

``` r
# Compute the standard deviation of the annual excess returns
apply(daily_excess, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##    1439.11    2931.87    3478.70    2543.55    2439.13    2564.71    1982.09 
    ##        SPY   Hashrate Difficulty 
    ##     411.54    4389.18     692.03

Now at the monthly frequency:

``` r
# Compute the average annualized nominal returns
colMeans(monthly_nominal, na.rm=TRUE) |> round(2)
```

    ##        INF         RF        BTC       MARA       CLSK       RIOT       CIFR 
    ##       2.89       2.38      55.26     -17.30     -10.38      17.11     -27.25 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##      -6.25     -16.23      11.81      80.75      81.25

``` r
# Compute the average annualized real returns
colMeans(monthly_real, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##      -0.38      51.38     -20.23     -14.05      14.81     -27.41      -7.45 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##     -20.63       8.74      80.75      81.25

``` r
# Compute the average annualized excess returns
colMeans(monthly_excess, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##      51.77     -19.85     -13.24      15.61     -25.34      -6.44     -19.15 
    ##        SPY   Hashrate Difficulty 
    ##       9.12      80.75      81.25

``` r
# Compute the standard deviation of the annual nominal returns
apply(monthly_nominal, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        INF         RF        BTC       MARA       CLSK       RIOT       CIFR 
    ##       3.56       0.99     246.74     443.97     436.54     402.26     340.08 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##     405.76     354.46      53.75     105.97     104.50

``` r
# Compute the standard deviation of the annual real returns
apply(monthly_real, 2, sd, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##       3.52     240.82     430.53     420.81     388.48     323.23     391.28 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##     346.20      52.93     105.97     104.50

``` r
# Compute the standard deviation of the annual excess returns
apply(monthly_excess, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##     240.77     430.83     420.99     388.29     322.08     390.96     346.46 
    ##        SPY   Hashrate Difficulty 
    ##      52.95     105.97     104.50

However, an important note to make about the results above is that only
BTC, MARA, and SPY have data going back to 2014. So the other mining
stocks each have shorter time frames. Thus, we should be cautious to
draw any comparative conclusions from the stats above.

### Subset to Final Datasets

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
    ##       4.98       3.28      17.06     -12.26       6.15     -43.01     -34.71 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##     -35.34     -16.23       7.96      63.36      62.54

``` r
# Compute the average annualized real returns
colMeans(monthly_real_final, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##      -1.48      13.58     -12.76       2.22     -42.55     -33.54     -35.05 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##     -20.63       3.29      63.36      62.54

``` r
# Compute the average annualized excess returns
colMeans(monthly_excess_final, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##      15.06     -11.28       3.71     -41.07     -32.06     -33.57     -19.15 
    ##        SPY   Hashrate Difficulty 
    ##       4.77      63.36      62.54

``` r
# Compute the standard deviation of the annual nominal returns
apply(monthly_nominal_final, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        INF         RF        BTC       MARA       CLSK       RIOT       CIFR 
    ##       3.73       1.06     216.18     488.88     408.06     397.85     382.84 
    ##        HUT       BTDR        SPY   Hashrate Difficulty 
    ##     422.07     354.46      63.55      62.04      55.61

``` r
# Compute the standard deviation of the annual real returns
apply(monthly_real_final, 2, sd, na.rm=TRUE) |> round(2)
```

    ##         RF        BTC       MARA       CLSK       RIOT       CIFR        HUT 
    ##       4.01     202.55     468.09     385.12     378.77     363.95     400.45 
    ##       BTDR        SPY   Hashrate Difficulty 
    ##     346.20      61.21      62.04      55.61

``` r
# Compute the standard deviation of the annual excess returns
apply(monthly_excess_final, 2, sd, na.rm=TRUE) |> round(2)
```

    ##        BTC       MARA       CLSK       RIOT       CIFR        HUT       BTDR 
    ##     201.40     467.30     384.69     377.85     362.63     399.71     346.46 
    ##        SPY   Hashrate Difficulty 
    ##      60.45      62.04      55.61

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
    ##     0.0748    -0.0241     0.0096    -0.1087    -0.0884    -0.0840    -0.0553 
    ##        SPY   Hashrate Difficulty 
    ##     0.0789     1.0213     1.1245

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
    ## INF         1.00  0.01 -0.01  0.02  0.02 -0.02 -0.05 -0.02  0.03  0.02     0.00
    ## RF          0.01  1.00 -0.03 -0.03  0.00 -0.04  0.01 -0.03 -0.02 -0.01     0.00
    ## BTC        -0.01 -0.03  1.00  0.33  0.13  0.44  0.27  0.49  0.09  0.23     0.03
    ## MARA        0.02 -0.03  0.33  1.00  0.18  0.63  0.41  0.62  0.23  0.28    -0.01
    ## CLSK        0.02  0.00  0.13  0.18  1.00  0.23  0.44  0.23  0.24  0.16     0.04
    ## RIOT       -0.02 -0.04  0.44  0.63  0.23  1.00  0.42  0.60  0.22  0.33     0.02
    ## CIFR       -0.05  0.01  0.27  0.41  0.44  0.42  1.00  0.42  0.22  0.26     0.02
    ## HUT        -0.02 -0.03  0.49  0.62  0.23  0.60  0.42  1.00  0.23  0.31     0.01
    ## BTDR        0.03 -0.02  0.09  0.23  0.24  0.22  0.22  0.23  1.00  0.06     0.00
    ## SPY         0.02 -0.01  0.23  0.28  0.16  0.33  0.26  0.31  0.06  1.00     0.02
    ## Hashrate    0.00  0.00  0.03 -0.01  0.04  0.02  0.02  0.01  0.00  0.02     1.00
    ## Difficulty -0.02  0.00  0.02 -0.05 -0.04 -0.05 -0.03 -0.07 -0.05 -0.05    -0.01
    ##            Difficulty
    ## INF             -0.02
    ## RF               0.00
    ## BTC              0.02
    ## MARA            -0.05
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
    ## INF         1.00  0.04 -0.05  0.04  0.05 -0.11 -0.33 -0.15  0.04  0.03    -0.13
    ## RF          0.04  1.00 -0.08 -0.13  0.00 -0.17  0.11 -0.15 -0.11 -0.05     0.03
    ## BTC        -0.05 -0.08  1.00  0.50  0.33  0.61  0.50  0.73 -0.03  0.37     0.14
    ## MARA        0.04 -0.13  0.50  1.00  0.50  0.73  0.55  0.73  0.23  0.50     0.00
    ## CLSK        0.05  0.00  0.33  0.50  1.00  0.44  0.49  0.39  0.09  0.46     0.06
    ## RIOT       -0.11 -0.17  0.61  0.73  0.44  1.00  0.55  0.68  0.02  0.50    -0.02
    ## CIFR       -0.33  0.11  0.50  0.55  0.49  0.55  1.00  0.63  0.07  0.38     0.08
    ## HUT        -0.15 -0.15  0.73  0.73  0.39  0.68  0.63  1.00  0.17  0.41     0.16
    ## BTDR        0.04 -0.11 -0.03  0.23  0.09  0.02  0.07  0.17  1.00  0.26    -0.11
    ## SPY         0.03 -0.05  0.37  0.50  0.46  0.50  0.38  0.41  0.26  1.00     0.04
    ## Hashrate   -0.13  0.03  0.14  0.00  0.06 -0.02  0.08  0.16 -0.11  0.04     1.00
    ## Difficulty -0.10  0.04  0.09 -0.01 -0.03 -0.06 -0.03  0.10  0.00 -0.03     0.86
    ##            Difficulty
    ## INF             -0.10
    ## RF               0.04
    ## BTC              0.09
    ## MARA            -0.01
    ## CLSK            -0.03
    ## RIOT            -0.06
    ## CIFR            -0.03
    ## HUT              0.10
    ## BTDR             0.00
    ## SPY             -0.03
    ## Hashrate         0.86
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
    ## INF         1.00 -0.51 -0.29 -0.23 -0.08 -0.24 -0.34 -0.24  0.04 -0.16     0.09
    ## RF         -0.51  1.00  0.18  0.02  0.15  0.09  0.18 -0.07 -0.11  0.12    -0.30
    ## BTC        -0.29  0.18  1.00  0.78  0.74  0.69  0.60  0.77 -0.03  0.58     0.34
    ## MARA       -0.23  0.02  0.78  1.00  0.75  0.86  0.63  0.88  0.23  0.66     0.27
    ## CLSK       -0.08  0.15  0.74  0.75  1.00  0.72  0.56  0.69  0.09  0.53     0.23
    ## RIOT       -0.24  0.09  0.69  0.86  0.72  1.00  0.68  0.73  0.02  0.55     0.22
    ## CIFR       -0.34  0.18  0.60  0.63  0.56  0.68  1.00  0.71  0.07  0.40     0.16
    ## HUT        -0.24 -0.07  0.77  0.88  0.69  0.73  0.71  1.00  0.17  0.53     0.39
    ## BTDR        0.04 -0.11 -0.03  0.23  0.09  0.02  0.07  0.17  1.00  0.26    -0.11
    ## SPY        -0.16  0.12  0.58  0.66  0.53  0.55  0.40  0.53  0.26  1.00     0.10
    ## Hashrate    0.09 -0.30  0.34  0.27  0.23  0.22  0.16  0.39 -0.11  0.10     1.00
    ## Difficulty  0.16 -0.24  0.20  0.08  0.10  0.07 -0.01  0.11  0.00  0.03     0.74
    ##            Difficulty
    ## INF              0.16
    ## RF              -0.24
    ## BTC              0.20
    ## MARA             0.08
    ## CLSK             0.10
    ## RIOT             0.07
    ## CIFR            -0.01
    ## HUT              0.11
    ## BTDR             0.00
    ## SPY              0.03
    ## Hashrate         0.74
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
    ## -368.37  -99.62  -11.06  103.55  351.51 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.4677    31.0300   0.047 0.962569    
    ## SPY           1.9587     0.4917   3.984 0.000367 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 179.5 on 32 degrees of freedom
    ## Multiple R-squared:  0.3315, Adjusted R-squared:  0.3106 
    ## F-statistic: 15.87 on 1 and 32 DF,  p-value: 0.0003667

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
    ## -1153.70  -166.75    17.21   197.19   588.74 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -52.836     64.279  -0.822    0.417    
    ## SPY            5.098      1.019   5.005 1.96e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 371.8 on 32 degrees of freedom
    ## Multiple R-squared:  0.4391, Adjusted R-squared:  0.4216 
    ## F-statistic: 25.05 on 1 and 32 DF,  p-value: 1.958e-05

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
    ## -912.55 -170.30  -56.96  192.39  691.18 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -20.8041    60.8653  -0.342  0.73473   
    ## SPY           3.3867     0.9644   3.512  0.00135 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 352.1 on 32 degrees of freedom
    ## Multiple R-squared:  0.2782, Adjusted R-squared:  0.2556 
    ## F-statistic: 12.33 on 1 and 32 DF,  p-value: 0.001349

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
    ## -735.25 -216.92   24.79  218.74  555.03 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -70.5809    58.1830  -1.213  0.23398    
    ## SPY           3.4635     0.9219   3.757  0.00069 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 336.6 on 32 degrees of freedom
    ## Multiple R-squared:  0.3061, Adjusted R-squared:  0.2844 
    ## F-statistic: 14.11 on 1 and 32 DF,  p-value: 0.0006898

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
    ## -715.19 -226.90    7.32  158.91  793.93 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -53.6687    61.7363  -0.869   0.3911  
    ## SPY           2.3817     0.9782   2.435   0.0207 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 357.1 on 32 degrees of freedom
    ## Multiple R-squared:  0.1563, Adjusted R-squared:  0.1299 
    ## F-statistic: 5.928 on 1 and 32 DF,  p-value: 0.02066

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
    ## -925.19 -149.21    3.42  264.32  788.65 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -63.4897    62.7139  -1.012  0.31896   
    ## SPY           3.5373     0.9937   3.560  0.00118 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 362.8 on 32 degrees of freedom
    ## Multiple R-squared:  0.2837, Adjusted R-squared:  0.2613 
    ## F-statistic: 12.67 on 1 and 32 DF,  p-value: 0.001184

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
    ## -1071.08  -111.54    11.57   111.11   907.31 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) -27.5784    60.1629  -0.458    0.650
    ## SPY           1.4254     0.9533   1.495    0.145
    ## 
    ## Residual standard error: 348 on 32 degrees of freedom
    ## Multiple R-squared:  0.0653, Adjusted R-squared:  0.03609 
    ## F-statistic: 2.236 on 1 and 32 DF,  p-value: 0.1447

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
    ## BTC   1.00 0.65  0.63  0.55  0.50 0.67 -0.23
    ## MARA  0.65 1.00  0.64  0.79  0.54 0.83  0.08
    ## CLSK  0.63 0.64  1.00  0.61  0.45 0.57 -0.06
    ## RIOT  0.55 0.79  0.61  1.00  0.60 0.62 -0.15
    ## CIFR  0.50 0.54  0.45  0.60  1.00 0.64 -0.03
    ## HUT   0.67 0.83  0.57  0.62  0.64 1.00  0.04
    ## BTDR -0.23 0.08 -0.06 -0.15 -0.03 0.04  1.00

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
    ##    Min     1Q Median     3Q    Max 
    ## -697.7 -173.0   21.1  160.6  594.6 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -54.7975    49.8995  -1.098   0.2806    
    ## SPY           2.4800     0.9670   2.565   0.0154 *  
    ## BTC           1.3365     0.2843   4.702 5.04e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 288.6 on 31 degrees of freedom
    ## Multiple R-squared:  0.6726, Adjusted R-squared:  0.6514 
    ## F-statistic: 31.84 on 2 and 31 DF,  p-value: 3.05e-08

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
    ## -473.67 -217.50  -18.15  215.50  485.63 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -22.6076    48.2032  -0.469    0.642    
    ## SPY           0.9800     0.9341   1.049    0.302    
    ## BTC           1.2288     0.2746   4.475 9.62e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 278.8 on 31 degrees of freedom
    ## Multiple R-squared:  0.5614, Adjusted R-squared:  0.5331 
    ## F-statistic: 19.84 on 2 and 31 DF,  p-value: 2.828e-06

``` r
BFM_RIOT = lm(RIOT~SPY+BTC, data=monthly_nominal_final)
summary(BFM_RIOT)
```

    ## 
    ## Call:
    ## lm(formula = RIOT ~ SPY + BTC, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -418.8 -193.7    3.3  182.7  607.1 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -72.0977    49.3258  -1.462 0.153898    
    ## SPY           1.4393     0.9559   1.506 0.142270    
    ## BTC           1.0335     0.2810   3.678 0.000886 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 285.3 on 31 degrees of freedom
    ## Multiple R-squared:  0.5169, Adjusted R-squared:  0.4857 
    ## F-statistic: 16.58 on 2 and 31 DF,  p-value: 1.267e-05

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
    ## -711.10 -253.20   -5.16  192.69  537.73 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -55.1287    54.3228  -1.015  0.31804   
    ## SPY           0.4333     1.0527   0.412  0.68346   
    ## BTC           0.9948     0.3095   3.215  0.00305 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 314.2 on 31 degrees of freedom
    ## Multiple R-squared:  0.3672, Adjusted R-squared:  0.3264 
    ## F-statistic: 8.995 on 2 and 31 DF,  p-value: 0.0008307

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
    ## -659.02 -178.38  -34.44  186.78  439.94 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -65.4914    47.0233  -1.393    0.174    
    ## SPY           0.8659     0.9113   0.950    0.349    
    ## BTC           1.3639     0.2679   5.091 1.65e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 272 on 31 degrees of freedom
    ## Multiple R-squared:  0.6099, Adjusted R-squared:  0.5847 
    ## F-statistic: 24.23 on 2 and 31 DF,  p-value: 4.608e-07

``` r
BFM_BTDR = lm(BTDR~SPY+BTC, data=monthly_nominal_final)
summary(BFM_BTDR)
```

    ## 
    ## Call:
    ## lm(formula = BTDR ~ SPY + BTC, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -914.41 -173.81   40.37  105.84  906.04 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -26.9242    59.4906  -0.453   0.6540  
    ## SPY           2.2983     1.1529   1.994   0.0551 .
    ## BTC          -0.4457     0.3389  -1.315   0.1981  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 344.1 on 31 degrees of freedom
    ## Multiple R-squared:  0.1147, Adjusted R-squared:  0.05758 
    ## F-statistic: 2.008 on 2 and 31 DF,  p-value: 0.1513

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
    ## MARA 1.00 0.39  0.68 0.33 0.70  0.31
    ## CLSK 0.39 1.00  0.41 0.21 0.25  0.12
    ## RIOT 0.68 0.41  1.00 0.45 0.40 -0.03
    ## CIFR 0.33 0.21  0.45 1.00 0.47  0.10
    ## HUT  0.70 0.25  0.40 0.47 1.00  0.28
    ## BTDR 0.31 0.12 -0.03 0.10 0.28  1.00

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
    ## -683.94 -182.64   13.38  155.25  638.40 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -74.8189    74.6959  -1.002 0.324525    
    ## SPY           2.5238     0.9882   2.554 0.015968 *  
    ## BTC           1.2976     0.3075   4.220 0.000208 ***
    ## Hashrate      0.3210     0.8807   0.364 0.718075    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 292.8 on 30 degrees of freedom
    ## Multiple R-squared:  0.674,  Adjusted R-squared:  0.6414 
    ## F-statistic: 20.68 on 3 and 30 DF,  p-value: 1.862e-07

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
    ## -468.31 -225.76  -13.97  216.99  485.80 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -16.9121    72.3024  -0.234 0.816646    
    ## SPY           0.9675     0.9565   1.012 0.319862    
    ## BTC           1.2398     0.2976   4.166 0.000242 ***
    ## Hashrate     -0.0913     0.8524  -0.107 0.915416    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 283.4 on 30 degrees of freedom
    ## Multiple R-squared:  0.5616, Adjusted R-squared:  0.5178 
    ## F-statistic: 12.81 on 3 and 30 DF,  p-value: 1.459e-05

``` r
HFM_RIOT = lm(RIOT~SPY+BTC+Hashrate, data=monthly_nominal_final)
summary(HFM_RIOT)
```

    ## 
    ## Call:
    ## lm(formula = RIOT ~ SPY + BTC + Hashrate, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -418.84 -194.02    3.86  182.47  607.07 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -71.758537  74.000355  -0.970  0.33995   
    ## SPY           1.438532   0.978949   1.469  0.15212   
    ## BTC           1.034146   0.304609   3.395  0.00195 **
    ## Hashrate     -0.005437   0.872451  -0.006  0.99507   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 290 on 30 degrees of freedom
    ## Multiple R-squared:  0.5169, Adjusted R-squared:  0.4686 
    ## F-statistic:  10.7 on 3 and 30 DF,  p-value: 6.037e-05

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
    ## -705.11 -258.96   -1.71  188.37  544.57 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -37.5053    81.3795  -0.461  0.64822   
    ## SPY           0.3948     1.0766   0.367  0.71642   
    ## BTC           1.0290     0.3350   3.072  0.00449 **
    ## Hashrate     -0.2825     0.9594  -0.294  0.77044   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 318.9 on 30 degrees of freedom
    ## Multiple R-squared:  0.369,  Adjusted R-squared:  0.3059 
    ## F-statistic: 5.849 on 3 and 30 DF,  p-value: 0.002859

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
    ## -644.40 -146.47  -14.01  205.47  414.98 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -134.8211    68.4144  -1.971 0.058052 .  
    ## SPY            1.0175     0.9051   1.124 0.269813    
    ## BTC            1.2291     0.2816   4.364 0.000139 ***
    ## Hashrate       1.1114     0.8066   1.378 0.178424    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 268.1 on 30 degrees of freedom
    ## Multiple R-squared:  0.6331, Adjusted R-squared:  0.5964 
    ## F-statistic: 17.26 on 3 and 30 DF,  p-value: 1.066e-06

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
    ## -917.54 -176.34   48.06  111.88  906.75 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  -3.7174    89.0639  -0.042   0.9670  
    ## SPY           2.2476     1.1782   1.908   0.0661 .
    ## BTC          -0.4006     0.3666  -1.093   0.2832  
    ## Hashrate     -0.3720     1.0500  -0.354   0.7256  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 349.1 on 30 degrees of freedom
    ## Multiple R-squared:  0.1184, Adjusted R-squared:  0.03022 
    ## F-statistic: 1.343 on 3 and 30 DF,  p-value: 0.2791

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
    ## -684.20 -157.00   37.54  187.19  554.34 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -28.0000    77.3219  -0.362   0.7198    
    ## SPY           2.4299     0.9857   2.465   0.0196 *  
    ## BTC           1.3672     0.2957   4.624 6.72e-05 ***
    ## Difficulty   -0.4305     0.9400  -0.458   0.6503    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 292.4 on 30 degrees of freedom
    ## Multiple R-squared:  0.6748, Adjusted R-squared:  0.6423 
    ## F-statistic: 20.75 on 3 and 30 DF,  p-value: 1.793e-07

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
    ## -463.04 -231.46  -14.83  209.95  489.88 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -4.1546    74.8218  -0.056 0.956087    
    ## SPY           0.9455     0.9538   0.991 0.329488    
    ## BTC           1.2499     0.2861   4.369 0.000137 ***
    ## Difficulty   -0.2965     0.9096  -0.326 0.746754    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 282.9 on 30 degrees of freedom
    ## Multiple R-squared:  0.563,  Adjusted R-squared:  0.5193 
    ## F-statistic: 12.88 on 3 and 30 DF,  p-value: 1.393e-05

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
    ## -424.71 -203.76   12.85  164.70  592.70 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -47.7034    76.4736  -0.624  0.53748   
    ## SPY           1.3937     0.9748   1.430  0.16316   
    ## BTC           1.0614     0.2924   3.630  0.00104 **
    ## Difficulty   -0.3919     0.9297  -0.422  0.67637   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 289.2 on 30 degrees of freedom
    ## Multiple R-squared:  0.5197, Adjusted R-squared:  0.4717 
    ## F-statistic: 10.82 on 3 and 30 DF,  p-value: 5.539e-05

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
    ## -698.54 -239.70   -3.88  219.73  501.11 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)   2.4978    83.3178   0.030  0.97628   
    ## SPY           0.3256     1.0621   0.307  0.76131   
    ## BTC           1.0608     0.3186   3.330  0.00231 **
    ## Difficulty   -0.9258     1.0129  -0.914  0.36800   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 315.1 on 30 degrees of freedom
    ## Multiple R-squared:  0.3844, Adjusted R-squared:  0.3228 
    ## F-statistic: 6.243 on 3 and 30 DF,  p-value: 0.002012

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
    ## -661.36 -181.11  -29.89  186.89  447.05 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -48.9802    73.0108  -0.671    0.507    
    ## SPY           0.8351     0.9307   0.897    0.377    
    ## BTC           1.3828     0.2792   4.953 2.66e-05 ***
    ## Difficulty   -0.2653     0.8876  -0.299    0.767    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 276.1 on 30 degrees of freedom
    ## Multiple R-squared:  0.611,  Adjusted R-squared:  0.5721 
    ## F-statistic: 15.71 on 3 and 30 DF,  p-value: 2.519e-06

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
    ## -911.57 -180.11   27.39   96.91  901.42 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -46.9794    92.3790  -0.509   0.6148  
    ## SPY           2.3358     1.1776   1.984   0.0565 .
    ## BTC          -0.4687     0.3532  -1.327   0.1946  
    ## Difficulty    0.3222     1.1230   0.287   0.7762  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 349.3 on 30 degrees of freedom
    ## Multiple R-squared:  0.1171, Adjusted R-squared:  0.02883 
    ## F-statistic: 1.327 on 3 and 30 DF,  p-value: 0.2842

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
    ## -597.79 -158.42   38.17  166.75  605.24 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -46.3013    79.3912  -0.583 0.564264    
    ## SPY           2.4896     0.9871   2.522 0.017412 *  
    ## BTC           1.2821     0.3073   4.172 0.000251 ***
    ## Hashrate      1.2874     1.2746   1.010 0.320822    
    ## Difficulty   -1.4267     1.3623  -1.047 0.303609    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 292.3 on 29 degrees of freedom
    ## Multiple R-squared:  0.6859, Adjusted R-squared:  0.6426 
    ## F-statistic: 15.83 on 4 and 29 DF,  p-value: 5.583e-07

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
    ## -470.17 -221.22  -15.92  202.85  492.00 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -7.4265    78.1185  -0.095 0.924915    
    ## SPY           0.9561     0.9713   0.984 0.333062    
    ## BTC           1.2347     0.3024   4.083 0.000319 ***
    ## Hashrate      0.2302     1.2542   0.184 0.855667    
    ## Difficulty   -0.4745     1.3404  -0.354 0.725876    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 287.6 on 29 degrees of freedom
    ## Multiple R-squared:  0.5635, Adjusted R-squared:  0.5033 
    ## F-statistic: 9.359 on 4 and 29 DF,  p-value: 5.524e-05

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
    ## -425.24 -184.44  -33.73  163.69  598.70 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -55.4723    79.6389  -0.697   0.4916   
    ## SPY           1.4190     0.9902   1.433   0.1625   
    ## BTC           1.0253     0.3083   3.326   0.0024 **
    ## Hashrate      0.5465     1.2786   0.427   0.6722   
    ## Difficulty   -0.8148     1.3665  -0.596   0.5556   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 293.2 on 29 degrees of freedom
    ## Multiple R-squared:  0.5227, Adjusted R-squared:  0.4569 
    ## F-statistic: 7.941 on 4 and 29 DF,  p-value: 0.0001886

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
    ## -706.27 -254.46   10.58  224.29  508.51 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  -7.7981    86.6351  -0.090  0.92890   
    ## SPY           0.3592     1.0772   0.333  0.74121   
    ## BTC           1.0129     0.3354   3.020  0.00523 **
    ## Hashrate      0.7243     1.3909   0.521  0.60652   
    ## Difficulty   -1.4862     1.4865  -1.000  0.32569   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 318.9 on 29 degrees of freedom
    ## Multiple R-squared:  0.3901, Adjusted R-squared:  0.3059 
    ## F-statistic: 4.637 on 4 and 29 DF,  p-value: 0.005126

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
    ## -644.18 -190.55    3.02  142.55  459.28 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -87.5519    69.5159  -1.259  0.21791    
    ## SPY           0.9608     0.8643   1.112  0.27541    
    ## BTC           1.2034     0.2691   4.472  0.00011 ***
    ## Hashrate      2.7134     1.1161   2.431  0.02146 *  
    ## Difficulty   -2.3648     1.1928  -1.983  0.05696 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 255.9 on 29 degrees of freedom
    ## Multiple R-squared:  0.6769, Adjusted R-squared:  0.6323 
    ## F-statistic: 15.19 on 4 and 29 DF,  p-value: 8.309e-07

``` r
FFM_BTDR = lm(BTDR~SPY+BTC+Hashrate+Difficulty, data=monthly_nominal_final)
summary(FFM_BTDR)
```

    ## 
    ## Call:
    ## lm(formula = BTDR ~ SPY + BTC + Hashrate + Difficulty, data = monthly_nominal_final)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -913.5 -188.7   32.3  134.4  890.0 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept) -29.3446    95.4325  -0.307   0.7607  
    ## SPY           2.2783     1.1865   1.920   0.0647 .
    ## BTC          -0.3867     0.3694  -1.047   0.3039  
    ## Hashrate     -1.2405     1.5322  -0.810   0.4247  
    ## Difficulty    1.2821     1.6375   0.783   0.4400  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 351.3 on 29 degrees of freedom
    ## Multiple R-squared:  0.1366, Adjusted R-squared:  0.01755 
    ## F-statistic: 1.147 on 4 and 29 DF,  p-value: 0.3542

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
          type='text',
          align=TRUE,
          digits=2)
```

    ## 
    ## ================================================
    ## Statistic  N   Mean  St. Dev.    Min      Max   
    ## ------------------------------------------------
    ## INF        34  4.98    3.73     -0.08    14.89  
    ## RF         34  3.28    1.06     1.28      4.80  
    ## BTC        34 17.06   216.18   -569.18   435.21 
    ## MARA       34 -12.26  488.88   -875.59  1,066.74
    ## CLSK       34  6.15   408.06   -774.01   877.12 
    ## RIOT       34 -43.01  397.85   -883.32   737.94 
    ## CIFR       34 -34.71  382.84   -687.70   914.57 
    ## HUT        34 -35.34  422.07   -781.10   984.05 
    ## BTDR       34 -16.23  354.46  -1,136.20  987.13 
    ## SPY        34  7.96   63.55    -116.40   105.71 
    ## Hashrate   34 63.36   62.04    -64.58    227.35 
    ## Difficulty 34 62.54   55.61    -56.53    198.74 
    ## ------------------------------------------------

``` r
# Generate and save output to a LaTeX file
stargazer(monthly_nominal_final, 
          summary=TRUE,
          type='latex',
          align=TRUE,
          digits=2, 
          font.size="large",
          out="Figures/SummaryStats_nominal.tex",
          title="Summary Statistics for the Final Monthly Dataset. Asset nominal returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).", 
          label="SummaryStats_nominal")
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Jul 09, 2024 - 16:40:27
    ## % Requires LaTeX packages: dcolumn 
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Summary Statistics for the Final Monthly Dataset. Asset nominal returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{SummaryStats_nominal} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lD{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} } 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ## Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
    ## \hline \\[-1.8ex] 
    ## INF & 34 & 4.98 & 3.73 & -0.08 & 14.89 \\ 
    ## RF & 34 & 3.28 & 1.06 & 1.28 & 4.80 \\ 
    ## BTC & 34 & 17.06 & 216.18 & -569.18 & 435.21 \\ 
    ## MARA & 34 & -12.26 & 488.88 & -875.59 & 1,066.74 \\ 
    ## CLSK & 34 & 6.15 & 408.06 & -774.01 & 877.12 \\ 
    ## RIOT & 34 & -43.01 & 397.85 & -883.32 & 737.94 \\ 
    ## CIFR & 34 & -34.71 & 382.84 & -687.70 & 914.57 \\ 
    ## HUT & 34 & -35.34 & 422.07 & -781.10 & 984.05 \\ 
    ## BTDR & 34 & -16.23 & 354.46 & -1,136.20 & 987.13 \\ 
    ## SPY & 34 & 7.96 & 63.55 & -116.40 & 105.71 \\ 
    ## Hashrate & 34 & 63.36 & 62.04 & -64.58 & 227.35 \\ 
    ## Difficulty & 34 & 62.54 & 55.61 & -56.53 & 198.74 \\ 
    ## \hline \\[-1.8ex] 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(monthly_real_final, 
          summary=TRUE, 
          type='text',
          align=TRUE,
          digits=2)
```

    ## 
    ## ================================================
    ## Statistic  N   Mean  St. Dev.    Min      Max   
    ## ------------------------------------------------
    ## RF         34 -1.48    4.01    -10.22     4.41  
    ## BTC        34 13.58   202.55   -508.38   408.31 
    ## MARA       34 -12.76  468.09   -851.84  1,067.65
    ## CLSK       34  2.22   385.12   -741.65   828.00 
    ## RIOT       34 -42.55  378.77   -845.71   689.13 
    ## CIFR       34 -33.54  363.95   -611.54   855.48 
    ## HUT        34 -35.05  400.45   -738.85   920.91 
    ## BTDR       34 -20.63  346.20  -1,126.47  960.39 
    ## SPY        34  3.29   61.21    -115.68   105.87 
    ## Hashrate   34 63.36   62.04    -64.58    227.35 
    ## Difficulty 34 62.54   55.61    -56.53    198.74 
    ## ------------------------------------------------

``` r
# Generate and save output to a LaTeX file
stargazer(monthly_real_final, 
          summary=TRUE,
          type='latex',
          align=TRUE,
          digits=2, 
          font.size="large",
          out="Figures/SummaryStats_real.tex",
          title="Summary Statistics for the Final Monthly Dataset. Asset real returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).", 
          label="SummaryStats_real")
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Jul 09, 2024 - 16:40:28
    ## % Requires LaTeX packages: dcolumn 
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Summary Statistics for the Final Monthly Dataset. Asset real returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{SummaryStats_real} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lD{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} } 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ## Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
    ## \hline \\[-1.8ex] 
    ## RF & 34 & -1.48 & 4.01 & -10.22 & 4.41 \\ 
    ## BTC & 34 & 13.58 & 202.55 & -508.38 & 408.31 \\ 
    ## MARA & 34 & -12.76 & 468.09 & -851.84 & 1,067.65 \\ 
    ## CLSK & 34 & 2.22 & 385.12 & -741.65 & 828.00 \\ 
    ## RIOT & 34 & -42.55 & 378.77 & -845.71 & 689.13 \\ 
    ## CIFR & 34 & -33.54 & 363.95 & -611.54 & 855.48 \\ 
    ## HUT & 34 & -35.05 & 400.45 & -738.85 & 920.91 \\ 
    ## BTDR & 34 & -20.63 & 346.20 & -1,126.47 & 960.39 \\ 
    ## SPY & 34 & 3.29 & 61.21 & -115.68 & 105.87 \\ 
    ## Hashrate & 34 & 63.36 & 62.04 & -64.58 & 227.35 \\ 
    ## Difficulty & 34 & 62.54 & 55.61 & -56.53 & 198.74 \\ 
    ## \hline \\[-1.8ex] 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(monthly_excess_final, 
          summary=TRUE, 
          type='text',
          align=TRUE,
          digits=2)
```

    ## 
    ## ================================================
    ## Statistic  N   Mean  St. Dev.    Min      Max   
    ## ------------------------------------------------
    ## BTC        34 15.06   201.40   -498.16   409.34 
    ## MARA       34 -11.28  467.30   -852.55  1,064.67
    ## CLSK       34  3.71   384.69   -739.47   829.03 
    ## RIOT       34 -41.07  377.85   -843.52   691.63 
    ## CIFR       34 -32.06  362.63   -601.32   857.97 
    ## HUT        34 -33.57  399.71   -739.56   923.41 
    ## BTDR       34 -19.15  346.46  -1,130.28  959.20 
    ## SPY        34  4.77   60.45    -114.66   102.89 
    ## Hashrate   34 63.36   62.04    -64.58    227.35 
    ## Difficulty 34 62.54   55.61    -56.53    198.74 
    ## ------------------------------------------------

``` r
# Generate and save output to a LaTeX file
stargazer(monthly_excess_final, 
          summary=TRUE,
          type='latex',
          align=TRUE,
          digits=2, 
          font.size="large",
          out="Figures/SummaryStats_excess.tex",
          title="Summary Statistics for the Final Monthly Dataset. Asset excess returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).", 
          label="SummaryStats_excess")
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Jul 09, 2024 - 16:40:28
    ## % Requires LaTeX packages: dcolumn 
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Summary Statistics for the Final Monthly Dataset. Asset excess returns and growth rates are all annualized and measured in percentage units. Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{SummaryStats_excess} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lD{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} } 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ## Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
    ## \hline \\[-1.8ex] 
    ## BTC & 34 & 15.06 & 201.40 & -498.16 & 409.34 \\ 
    ## MARA & 34 & -11.28 & 467.30 & -852.55 & 1,064.67 \\ 
    ## CLSK & 34 & 3.71 & 384.69 & -739.47 & 829.03 \\ 
    ## RIOT & 34 & -41.07 & 377.85 & -843.52 & 691.63 \\ 
    ## CIFR & 34 & -32.06 & 362.63 & -601.32 & 857.97 \\ 
    ## HUT & 34 & -33.57 & 399.71 & -739.56 & 923.41 \\ 
    ## BTDR & 34 & -19.15 & 346.46 & -1,130.28 & 959.20 \\ 
    ## SPY & 34 & 4.77 & 60.45 & -114.66 & 102.89 \\ 
    ## Hashrate & 34 & 63.36 & 62.04 & -64.58 & 227.35 \\ 
    ## Difficulty & 34 & 62.54 & 55.61 & -56.53 & 198.74 \\ 
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
          align=TRUE,
          digits=2,
          omit.stat=c("ser","f"))
```

    ## 
    ## ====================================================
    ##                        Dependent variable:          
    ##              ---------------------------------------
    ##                               MARA                  
    ##                (1)     (2)     (3)     (4)     (5)  
    ## ----------------------------------------------------
    ## SPY          5.10*** 2.48**  2.52**  2.43**  2.49** 
    ##              (1.02)  (0.97)  (0.99)  (0.99)  (0.99) 
    ##                                                     
    ## BTC                  1.34*** 1.30*** 1.37*** 1.28***
    ##                      (0.28)  (0.31)  (0.30)  (0.31) 
    ##                                                     
    ## Hashrate                      0.32            1.29  
    ##                              (0.88)          (1.27) 
    ##                                                     
    ## Difficulty                            -0.43   -1.43 
    ##                                      (0.94)  (1.36) 
    ##                                                     
    ## Constant     -52.84  -54.80  -74.82  -28.00  -46.30 
    ##              (64.28) (49.90) (74.70) (77.32) (79.39)
    ##                                                     
    ## ----------------------------------------------------
    ## Observations   34      34      34      34      34   
    ## R2            0.44    0.67    0.67    0.67    0.69  
    ## Adjusted R2   0.42    0.65    0.64    0.64    0.64  
    ## ====================================================
    ## Note:                    *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_MARA, BFM_MARA, HFM_MARA, DFM_MARA, FFM_MARA, 
          type='latex',
          align=TRUE,
          digits=2, 
          font.size="large",
          out="Figures/ModelResults_MARA.tex",
          title="Factor Model Results for Marathon Digital Holdings (MARA). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_MARA",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Jul 09, 2024 - 16:40:29
    ## % Requires LaTeX packages: dcolumn 
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Marathon Digital Holdings (MARA). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_MARA} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lD{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} } 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{MARA} \\ 
    ## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)}\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 5.10^{***} & 2.48^{**} & 2.52^{**} & 2.43^{**} & 2.49^{**} \\ 
    ##   & (1.02) & (0.97) & (0.99) & (0.99) & (0.99) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & 1.34^{***} & 1.30^{***} & 1.37^{***} & 1.28^{***} \\ 
    ##   &  & (0.28) & (0.31) & (0.30) & (0.31) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & 0.32 &  & 1.29 \\ 
    ##   &  &  & (0.88) &  & (1.27) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & -0.43 & -1.43 \\ 
    ##   &  &  &  & (0.94) & (1.36) \\ 
    ##   & & & & & \\ 
    ##  Constant & -52.84 & -54.80 & -74.82 & -28.00 & -46.30 \\ 
    ##   & (64.28) & (49.90) & (74.70) & (77.32) & (79.39) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} \\ 
    ## R$^{2}$ & \multicolumn{1}{c}{0.44} & \multicolumn{1}{c}{0.67} & \multicolumn{1}{c}{0.67} & \multicolumn{1}{c}{0.67} & \multicolumn{1}{c}{0.69} \\ 
    ## Adjusted R$^{2}$ & \multicolumn{1}{c}{0.42} & \multicolumn{1}{c}{0.65} & \multicolumn{1}{c}{0.64} & \multicolumn{1}{c}{0.64} & \multicolumn{1}{c}{0.64} \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_CLSK, BFM_CLSK, HFM_CLSK, DFM_CLSK, FFM_CLSK, 
          type='text',
          align=TRUE,
          digits=2,
          omit.stat=c("ser","f"))
```

    ## 
    ## ====================================================
    ##                        Dependent variable:          
    ##              ---------------------------------------
    ##                               CLSK                  
    ##                (1)     (2)     (3)     (4)     (5)  
    ## ----------------------------------------------------
    ## SPY          3.39***  0.98    0.97    0.95    0.96  
    ##              (0.96)  (0.93)  (0.96)  (0.95)  (0.97) 
    ##                                                     
    ## BTC                  1.23*** 1.24*** 1.25*** 1.23***
    ##                      (0.27)  (0.30)  (0.29)  (0.30) 
    ##                                                     
    ## Hashrate                      -0.09           0.23  
    ##                              (0.85)          (1.25) 
    ##                                                     
    ## Difficulty                            -0.30   -0.47 
    ##                                      (0.91)  (1.34) 
    ##                                                     
    ## Constant     -20.80  -22.61  -16.91   -4.15   -7.43 
    ##              (60.87) (48.20) (72.30) (74.82) (78.12)
    ##                                                     
    ## ----------------------------------------------------
    ## Observations   34      34      34      34      34   
    ## R2            0.28    0.56    0.56    0.56    0.56  
    ## Adjusted R2   0.26    0.53    0.52    0.52    0.50  
    ## ====================================================
    ## Note:                    *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_CLSK, BFM_CLSK, HFM_CLSK, DFM_CLSK, FFM_CLSK, 
          type='latex',
          align=TRUE,
          digits=2, 
          font.size="large",
          out="Figures/ModelResults_CLSK.tex",
          title="Factor Model Results for Cleanspark (CLSK). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_CLSK",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Jul 09, 2024 - 16:40:30
    ## % Requires LaTeX packages: dcolumn 
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Cleanspark (CLSK). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_CLSK} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lD{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} } 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{CLSK} \\ 
    ## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)}\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 3.39^{***} & 0.98 & 0.97 & 0.95 & 0.96 \\ 
    ##   & (0.96) & (0.93) & (0.96) & (0.95) & (0.97) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & 1.23^{***} & 1.24^{***} & 1.25^{***} & 1.23^{***} \\ 
    ##   &  & (0.27) & (0.30) & (0.29) & (0.30) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & -0.09 &  & 0.23 \\ 
    ##   &  &  & (0.85) &  & (1.25) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & -0.30 & -0.47 \\ 
    ##   &  &  &  & (0.91) & (1.34) \\ 
    ##   & & & & & \\ 
    ##  Constant & -20.80 & -22.61 & -16.91 & -4.15 & -7.43 \\ 
    ##   & (60.87) & (48.20) & (72.30) & (74.82) & (78.12) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} \\ 
    ## R$^{2}$ & \multicolumn{1}{c}{0.28} & \multicolumn{1}{c}{0.56} & \multicolumn{1}{c}{0.56} & \multicolumn{1}{c}{0.56} & \multicolumn{1}{c}{0.56} \\ 
    ## Adjusted R$^{2}$ & \multicolumn{1}{c}{0.26} & \multicolumn{1}{c}{0.53} & \multicolumn{1}{c}{0.52} & \multicolumn{1}{c}{0.52} & \multicolumn{1}{c}{0.50} \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_RIOT, BFM_RIOT, HFM_RIOT, DFM_RIOT, FFM_RIOT, 
          type='text',
          align=TRUE,
          digits=2,
          omit.stat=c("ser","f"))
```

    ## 
    ## ====================================================
    ##                        Dependent variable:          
    ##              ---------------------------------------
    ##                               RIOT                  
    ##                (1)     (2)     (3)     (4)     (5)  
    ## ----------------------------------------------------
    ## SPY          3.46***  1.44    1.44    1.39    1.42  
    ##              (0.92)  (0.96)  (0.98)  (0.97)  (0.99) 
    ##                                                     
    ## BTC                  1.03*** 1.03*** 1.06*** 1.03***
    ##                      (0.28)  (0.30)  (0.29)  (0.31) 
    ##                                                     
    ## Hashrate                      -0.01           0.55  
    ##                              (0.87)          (1.28) 
    ##                                                     
    ## Difficulty                            -0.39   -0.81 
    ##                                      (0.93)  (1.37) 
    ##                                                     
    ## Constant     -70.58  -72.10  -71.76  -47.70  -55.47 
    ##              (58.18) (49.33) (74.00) (76.47) (79.64)
    ##                                                     
    ## ----------------------------------------------------
    ## Observations   34      34      34      34      34   
    ## R2            0.31    0.52    0.52    0.52    0.52  
    ## Adjusted R2   0.28    0.49    0.47    0.47    0.46  
    ## ====================================================
    ## Note:                    *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_RIOT, BFM_RIOT, HFM_RIOT, DFM_RIOT, FFM_RIOT, 
          type='latex',
          align=TRUE,
          digits=2, 
          font.size="large",
          out="Figures/ModelResults_RIOT.tex",
          title="Factor Model Results for Riot Blockchain (RIOT). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_RIOT",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Jul 09, 2024 - 16:40:31
    ## % Requires LaTeX packages: dcolumn 
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Riot Blockchain (RIOT). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_RIOT} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lD{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} } 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{RIOT} \\ 
    ## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)}\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 3.46^{***} & 1.44 & 1.44 & 1.39 & 1.42 \\ 
    ##   & (0.92) & (0.96) & (0.98) & (0.97) & (0.99) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & 1.03^{***} & 1.03^{***} & 1.06^{***} & 1.03^{***} \\ 
    ##   &  & (0.28) & (0.30) & (0.29) & (0.31) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & -0.01 &  & 0.55 \\ 
    ##   &  &  & (0.87) &  & (1.28) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & -0.39 & -0.81 \\ 
    ##   &  &  &  & (0.93) & (1.37) \\ 
    ##   & & & & & \\ 
    ##  Constant & -70.58 & -72.10 & -71.76 & -47.70 & -55.47 \\ 
    ##   & (58.18) & (49.33) & (74.00) & (76.47) & (79.64) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} \\ 
    ## R$^{2}$ & \multicolumn{1}{c}{0.31} & \multicolumn{1}{c}{0.52} & \multicolumn{1}{c}{0.52} & \multicolumn{1}{c}{0.52} & \multicolumn{1}{c}{0.52} \\ 
    ## Adjusted R$^{2}$ & \multicolumn{1}{c}{0.28} & \multicolumn{1}{c}{0.49} & \multicolumn{1}{c}{0.47} & \multicolumn{1}{c}{0.47} & \multicolumn{1}{c}{0.46} \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_CIFR, BFM_CIFR, HFM_CIFR, DFM_CIFR, FFM_CIFR, 
          type='text',
          align=TRUE,
          digits=2,
          omit.stat=c("ser","f"))
```

    ## 
    ## ====================================================
    ##                        Dependent variable:          
    ##              ---------------------------------------
    ##                               CIFR                  
    ##                (1)     (2)     (3)     (4)     (5)  
    ## ----------------------------------------------------
    ## SPY          2.38**   0.43    0.39    0.33    0.36  
    ##              (0.98)  (1.05)  (1.08)  (1.06)  (1.08) 
    ##                                                     
    ## BTC                  0.99*** 1.03*** 1.06*** 1.01***
    ##                      (0.31)  (0.33)  (0.32)  (0.34) 
    ##                                                     
    ## Hashrate                      -0.28           0.72  
    ##                              (0.96)          (1.39) 
    ##                                                     
    ## Difficulty                            -0.93   -1.49 
    ##                                      (1.01)  (1.49) 
    ##                                                     
    ## Constant     -53.67  -55.13  -37.51   2.50    -7.80 
    ##              (61.74) (54.32) (81.38) (83.32) (86.64)
    ##                                                     
    ## ----------------------------------------------------
    ## Observations   34      34      34      34      34   
    ## R2            0.16    0.37    0.37    0.38    0.39  
    ## Adjusted R2   0.13    0.33    0.31    0.32    0.31  
    ## ====================================================
    ## Note:                    *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_CIFR, BFM_CIFR, HFM_CIFR, DFM_CIFR, FFM_CIFR, 
          type='latex',
          align=TRUE,
          digits=2, 
          font.size="large",
          out="Figures/ModelResults_CIFR.tex",
          title="Factor Model Results for Cipher Mining (CIFR). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_CIFR",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Jul 09, 2024 - 16:40:32
    ## % Requires LaTeX packages: dcolumn 
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Cipher Mining (CIFR). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_CIFR} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lD{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} } 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{CIFR} \\ 
    ## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)}\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 2.38^{**} & 0.43 & 0.39 & 0.33 & 0.36 \\ 
    ##   & (0.98) & (1.05) & (1.08) & (1.06) & (1.08) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & 0.99^{***} & 1.03^{***} & 1.06^{***} & 1.01^{***} \\ 
    ##   &  & (0.31) & (0.33) & (0.32) & (0.34) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & -0.28 &  & 0.72 \\ 
    ##   &  &  & (0.96) &  & (1.39) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & -0.93 & -1.49 \\ 
    ##   &  &  &  & (1.01) & (1.49) \\ 
    ##   & & & & & \\ 
    ##  Constant & -53.67 & -55.13 & -37.51 & 2.50 & -7.80 \\ 
    ##   & (61.74) & (54.32) & (81.38) & (83.32) & (86.64) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} \\ 
    ## R$^{2}$ & \multicolumn{1}{c}{0.16} & \multicolumn{1}{c}{0.37} & \multicolumn{1}{c}{0.37} & \multicolumn{1}{c}{0.38} & \multicolumn{1}{c}{0.39} \\ 
    ## Adjusted R$^{2}$ & \multicolumn{1}{c}{0.13} & \multicolumn{1}{c}{0.33} & \multicolumn{1}{c}{0.31} & \multicolumn{1}{c}{0.32} & \multicolumn{1}{c}{0.31} \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_HUT, BFM_HUT, HFM_HUT, DFM_HUT, FFM_HUT, 
          type='text',
          align=TRUE,
          digits=2,
          omit.stat=c("ser","f"))
```

    ## 
    ## =====================================================
    ##                        Dependent variable:           
    ##              ----------------------------------------
    ##                                HUT                   
    ##                (1)     (2)     (3)      (4)     (5)  
    ## -----------------------------------------------------
    ## SPY          3.54***  0.87     1.02    0.84    0.96  
    ##              (0.99)  (0.91)   (0.91)  (0.93)  (0.86) 
    ##                                                      
    ## BTC                  1.36*** 1.23***  1.38*** 1.20***
    ##                      (0.27)   (0.28)  (0.28)  (0.27) 
    ##                                                      
    ## Hashrate                       1.11           2.71** 
    ##                               (0.81)          (1.12) 
    ##                                                      
    ## Difficulty                             -0.27  -2.36* 
    ##                                       (0.89)  (1.19) 
    ##                                                      
    ## Constant     -63.49  -65.49  -134.82* -48.98  -87.55 
    ##              (62.71) (47.02) (68.41)  (73.01) (69.52)
    ##                                                      
    ## -----------------------------------------------------
    ## Observations   34      34       34      34      34   
    ## R2            0.28    0.61     0.63    0.61    0.68  
    ## Adjusted R2   0.26    0.58     0.60    0.57    0.63  
    ## =====================================================
    ## Note:                     *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_HUT, BFM_HUT, HFM_HUT, DFM_HUT, FFM_HUT, 
          type='latex',
          align=TRUE,
          digits=2, 
          font.size="large",
          out="Figures/ModelResults_HUT.tex",
          title="Factor Model Results for Hut 8 Mining (HUT). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_HUT",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Jul 09, 2024 - 16:40:33
    ## % Requires LaTeX packages: dcolumn 
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Hut 8 Mining (HUT). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_HUT} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lD{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} } 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{HUT} \\ 
    ## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)}\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 3.54^{***} & 0.87 & 1.02 & 0.84 & 0.96 \\ 
    ##   & (0.99) & (0.91) & (0.91) & (0.93) & (0.86) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & 1.36^{***} & 1.23^{***} & 1.38^{***} & 1.20^{***} \\ 
    ##   &  & (0.27) & (0.28) & (0.28) & (0.27) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & 1.11 &  & 2.71^{**} \\ 
    ##   &  &  & (0.81) &  & (1.12) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & -0.27 & -2.36^{*} \\ 
    ##   &  &  &  & (0.89) & (1.19) \\ 
    ##   & & & & & \\ 
    ##  Constant & -63.49 & -65.49 & -134.82^{*} & -48.98 & -87.55 \\ 
    ##   & (62.71) & (47.02) & (68.41) & (73.01) & (69.52) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} \\ 
    ## R$^{2}$ & \multicolumn{1}{c}{0.28} & \multicolumn{1}{c}{0.61} & \multicolumn{1}{c}{0.63} & \multicolumn{1}{c}{0.61} & \multicolumn{1}{c}{0.68} \\ 
    ## Adjusted R$^{2}$ & \multicolumn{1}{c}{0.26} & \multicolumn{1}{c}{0.58} & \multicolumn{1}{c}{0.60} & \multicolumn{1}{c}{0.57} & \multicolumn{1}{c}{0.63} \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}

``` r
# Generate summary statistics for the final real returns
stargazer(CAPM_BTDR, BFM_BTDR, HFM_BTDR, DFM_BTDR, FFM_BTDR, 
          type='text',
          align=TRUE,
          digits=2,
          omit.stat=c("ser","f"))
```

    ## 
    ## ====================================================
    ##                        Dependent variable:          
    ##              ---------------------------------------
    ##                               BTDR                  
    ##                (1)     (2)     (3)     (4)     (5)  
    ## ----------------------------------------------------
    ## SPY           1.43    2.30*   2.25*   2.34*   2.28* 
    ##              (0.95)  (1.15)  (1.18)  (1.18)  (1.19) 
    ##                                                     
    ## BTC                   -0.45   -0.40   -0.47   -0.39 
    ##                      (0.34)  (0.37)  (0.35)  (0.37) 
    ##                                                     
    ## Hashrate                      -0.37           -1.24 
    ##                              (1.05)          (1.53) 
    ##                                                     
    ## Difficulty                            0.32    1.28  
    ##                                      (1.12)  (1.64) 
    ##                                                     
    ## Constant     -27.58  -26.92   -3.72  -46.98  -29.34 
    ##              (60.16) (59.49) (89.06) (92.38) (95.43)
    ##                                                     
    ## ----------------------------------------------------
    ## Observations   34      34      34      34      34   
    ## R2            0.07    0.11    0.12    0.12    0.14  
    ## Adjusted R2   0.04    0.06    0.03    0.03    0.02  
    ## ====================================================
    ## Note:                    *p<0.1; **p<0.05; ***p<0.01

``` r
# Generate and save output to a LaTeX file
stargazer(CAPM_BTDR, BFM_BTDR, HFM_BTDR, DFM_BTDR, FFM_BTDR, 
          type='latex',
          align=TRUE,
          digits=2, 
          font.size="large",
          out="Figures/ModelResults_BTDR.tex",
          title="Factor Model Results for Bitdeer (BTDR). Table generated with the stargazer R package (Hlavac, 2022).", 
          label="ModelResults_BTDR",
          omit.stat=c("ser","f"))
```

    ## 
    ## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
    ## % Date and time: Tue, Jul 09, 2024 - 16:40:34
    ## % Requires LaTeX packages: dcolumn 
    ## \begin{table}[!htbp] \centering 
    ##   \caption{Factor Model Results for Bitdeer (BTDR). Table generated with the stargazer R package (Hlavac, 2022).} 
    ##   \label{ModelResults_BTDR} 
    ## \large 
    ## \begin{tabular}{@{\extracolsep{5pt}}lD{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} D{.}{.}{-2} } 
    ## \\[-1.8ex]\hline 
    ## \hline \\[-1.8ex] 
    ##  & \multicolumn{5}{c}{\textit{Dependent variable:}} \\ 
    ## \cline{2-6} 
    ## \\[-1.8ex] & \multicolumn{5}{c}{BTDR} \\ 
    ## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} & \multicolumn{1}{c}{(4)} & \multicolumn{1}{c}{(5)}\\ 
    ## \hline \\[-1.8ex] 
    ##  SPY & 1.43 & 2.30^{*} & 2.25^{*} & 2.34^{*} & 2.28^{*} \\ 
    ##   & (0.95) & (1.15) & (1.18) & (1.18) & (1.19) \\ 
    ##   & & & & & \\ 
    ##  BTC &  & -0.45 & -0.40 & -0.47 & -0.39 \\ 
    ##   &  & (0.34) & (0.37) & (0.35) & (0.37) \\ 
    ##   & & & & & \\ 
    ##  Hashrate &  &  & -0.37 &  & -1.24 \\ 
    ##   &  &  & (1.05) &  & (1.53) \\ 
    ##   & & & & & \\ 
    ##  Difficulty &  &  &  & 0.32 & 1.28 \\ 
    ##   &  &  &  & (1.12) & (1.64) \\ 
    ##   & & & & & \\ 
    ##  Constant & -27.58 & -26.92 & -3.72 & -46.98 & -29.34 \\ 
    ##   & (60.16) & (59.49) & (89.06) & (92.38) & (95.43) \\ 
    ##   & & & & & \\ 
    ## \hline \\[-1.8ex] 
    ## Observations & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} & \multicolumn{1}{c}{34} \\ 
    ## R$^{2}$ & \multicolumn{1}{c}{0.07} & \multicolumn{1}{c}{0.11} & \multicolumn{1}{c}{0.12} & \multicolumn{1}{c}{0.12} & \multicolumn{1}{c}{0.14} \\ 
    ## Adjusted R$^{2}$ & \multicolumn{1}{c}{0.04} & \multicolumn{1}{c}{0.06} & \multicolumn{1}{c}{0.03} & \multicolumn{1}{c}{0.03} & \multicolumn{1}{c}{0.02} \\ 
    ## \hline 
    ## \hline \\[-1.8ex] 
    ## \textit{Note:}  & \multicolumn{5}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
    ## \end{tabular} 
    ## \end{table}
