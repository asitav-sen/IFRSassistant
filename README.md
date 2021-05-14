---
title: "IFRSassistant"
author: "Asitav"
date: "15/05/2021"
output: 
  html_document: 
    keep_md: yes
---



## Intro

IFRSassistant, as the name suggests, aims to provide convenience in part of the IFRS reporting process, to the finance and accounting consultants and SMEs in the finance industry. Specifically, it helps with the estimation of loss on assets over specific periods in time. It involves certain complexities, which are as follows.

- Requirement of estimation of probability of default over different points in time
- Inclusion of external factors like macroeconomic
- Simulation of scenarios and calculation of weighted sum

The shiny app does this.


## How does it work

The application is loaded with sample data. However, you can upload your data. The application takes two data sets. One for transactions and other for data about the collaterals.

![Upload Modal](/Users/apple/Desktop/umod.png)
There are certain information compulsory in the transaction data.

- An `id` column that is the unique identifier of the asset
- A status column indicating `good or bad` as 0 and 1
- Date of origin, maturity and report generation
- Asset type and customer type classification
- Bureau or internal credit score
- Default flag
- Asset balance

And in the collateral data id and value of the collateral are required.

The data can (and should) contain multiple transactions and reports (on different report dates) per asset. However, latest transaction/report of all the assets should be on the same date i.e. if there are 100 assets, each of their last report date should be same.

![Formatting](/Users/apple/Desktop/formatting.png)

Then the columns need to be identified and confirmed.

Once done, the data is displayed. And some basic information about the portfolio is also plotted.

![Data](/Users/apple/Desktop/ndata.png)


![Basic info](/Users/apple/Desktop/basic.png)

IFRS 9 requires that the external parameters are considered during calculation. Here, we use `GDP` and `price` as the external parameters. This information is downloaded from IMF after the `fetch` button is clicked. The data is joined with the uploaded transaction data and displayed.

![Macroeconomic](/Users/apple/Desktop/macro.png)

Once this data is downloaded, the future parameters are forecasted. The results are not displayed in the app. However, they are available in the pdf report (screenshot below).

![Forecast](/Users/apple/Desktop/forecast.png)

After this, the model selection and fitting can be initiated using the relevant button. This takes some time. Then the outcome (distribution of the probabilities of default) is shown by year.

![Formatting](/Users/apple/Desktop/pod.png)
Next, some parameters are required for the user for present value calculations and Monte Carlo simulations. The present value of the exposure by year is calculated and displayed.

![sim setting and eod](/Users/apple/Desktop/eod.png)


The simulated loss calculation by year is plotted (can be selected for display). THe graphs can be clicked and dragged to display probability occurrence of loss within the selected range.

![Simulation result](/Users/apple/Desktop/sim.png)

A overall weighted sum of expected credit loss by year is also plotted.

![ecl](/Users/apple/Desktop/ecl.png)

Finally, after entering the name, a report can be generated and downloaded.


![report generation](/Users/apple/Desktop/rep.png)




![pdf](/Users/apple/Desktop/pdf.png)

