# Bitcoin Mining Stocks Project

### Overview

This project aims to analyze the performance of publicly traded bitcoin mining companies and their relationship to the price of bitcoin. The project will focus on the following companies: 

* [Marathon Digital Holdings (MARA)](https://www.mara.com/),
* [Cleanspark (CLSK)](https://www.cleanspark.com/),
* [Riot Blockchain (RIOT)](https://www.riotplatforms.com/),
* [Cipher Mining (CIFR)](https://www.ciphermining.com/),
* [Hut 8 Mining (HUT)](https://hut8.com/), and
* [Bitdeer Technologies Group (BTDR)](https://www.bitdeer.com/).

In addition to the relationship between these stocks and the price of bitcoin (BTC), we will also incorporate other mining-related factors that may influence the stock prices of these companies. These include the mining hashrate, difficulty, and trading volume of BTC.

### Other Bitcoin-Related Projects

For some other R Notebook project demos related to bitcoin, you can check out the following repositories:

* [bitcoin-timeseries-project](https://github.com/tim-dombrowski/bitcoin-timeseries-project) - This project focuses exploring the time series properties of bitcoin price data, including autocorrelation, stationarity, and seasonality.
* [bitcoin-factoranalysis-project](https://github.com/tim-dombrowski/bitcoin-factoranalysis-project) - This project focuses on applying traditional asset pricing models to bitcoin (e.g. CAPM, Fama-French, etc) and exploring some other potential factors related to the Bitcoin blockchain, such as hashrate or trading volume.

### Repository Structure

The data work for this project demo is contained in the R Notebook directory of this repository. On GitHub, the webpage within that folder should display the README.md file, which contains the compiled output of the R Notebook. If you wish to explore the source code locally, then you can open the [miningstocks.Rmd](https://github.com/tim-dombrowski/bitcoin-miningstocks-project/blob/main/R%20Notebook/miningstocks.Rmd) file in RStudio and execute the code chunks to replicate the data work. Note the `output: html_notebook` line in the header of that file, which indicates that the R Markdown document is an R Notebook. 

After running the code chunks in RStudio and making any desired changes, you can then create a copy that will generate a copy that will appear on GitHub. To do this, save a copy of the R Notebook and name it README.Rmd. Then, change the header line for the output type to `output: github_document`. This will switch the file from being an R Notebook to an R Markdown file that will compile into a generic [Markdown](https://www.markdownguide.org/) file (.md). This format (along with the README name) will automatically be recognized by GitHub and displayed in-browser. This will also replace the Preview button with an option to Knit the Markdown file. This knitting process will re-run all the code chunks and generate a new README.md file inside of the R Notebook folder, which will display on GitHub.
