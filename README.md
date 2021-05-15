
![logo3small](https://user-images.githubusercontent.com/66162817/118362717-49154600-b5ae-11eb-99b3-0e7a0bced8fe.png)

## Intro

IFRSassistant, as the name suggests, aims to provide convenience in part of the IFRS reporting process, to the finance and accounting consultants and SMEs in the finance industry. Specifically, it helps with the estimation of loss on assets over specific periods in time. It involves certain complexities, which are as follows.

- Requirement of estimation of probability of default over different points in time
- Inclusion of external factors like macroeconomic
- Simulation of scenarios and calculation of weighted sum

The shiny app does this and provides estimated credit loss by year for up to 6 years (current, 1, 2,3,4 and 5 years).


## How does it work

The application is loaded with sample data. However, you can upload your data. The application takes two data sets. One for transactions and other for data about the collaterals.

![Upload Modal](https://user-images.githubusercontent.com/66162817/118330640-6b16b600-b525-11eb-90b4-d723bd35da56.png)

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

![formatting](https://user-images.githubusercontent.com/66162817/118330632-6a7e1f80-b525-11eb-9df9-844dabdd089b.png)

Then the columns need to be identified and confirmed.

Once done, the data is displayed. And some basic information about the portfolio is also plotted.

![data](https://user-images.githubusercontent.com/66162817/118330622-69e58900-b525-11eb-9b05-ece0ffbb8ee4.png)


![basic](https://user-images.githubusercontent.com/66162817/118330612-69e58900-b525-11eb-8f9a-5d1c2c1fd080.png)

IFRS 9 requires that the external parameters are considered during calculation. Here, we use `GDP` and `price` as the external parameters. This information is downloaded from IMF after the `fetch` button is clicked. The data is joined with the uploaded transaction data and displayed.

![macro](https://user-images.githubusercontent.com/66162817/118330604-694cf280-b525-11eb-8993-06535159f4c3.png)

Once this data is downloaded, the future parameters are forecasted. The results are not displayed in the app. However, they are available in the pdf report (screenshot below).

![forecast](https://user-images.githubusercontent.com/66162817/118330531-6520d500-b525-11eb-88ce-bba5f0172eea.png)

After this, the model selection and fitting can be initiated using the relevant button. This takes some time. Then the outcome (distribution of the probabilities of default) is shown by year.

![pod](https://user-images.githubusercontent.com/66162817/118330593-68b45c00-b525-11eb-897b-5d3ff8faa22d.png)

Next, some parameters are required for the user for present value calculations and Monte Carlo simulations. The present value of the exposure by year is calculated and displayed.

![eod](https://user-images.githubusercontent.com/66162817/118330582-681bc580-b525-11eb-8e10-3db6ce1d93cc.png)


The simulated loss calculation by year is plotted (can be selected for display). THe graphs can be clicked and dragged to display probability occurrence of loss within the selected range.

![sim](https://user-images.githubusercontent.com/66162817/118330481-6225e480-b525-11eb-817b-a9d6ca143caa.png)

A overall weighted sum of expected credit loss by year is also plotted.

![ecl](https://user-images.githubusercontent.com/66162817/118330573-67832f00-b525-11eb-8638-00aaaa166cc8.png)

Finally, after entering the name, a report can be generated and downloaded.


![rep](https://user-images.githubusercontent.com/66162817/118330566-66ea9880-b525-11eb-943d-5f89b09cd3ed.png)


![pdf](https://user-images.githubusercontent.com/66162817/118333004-90f18a00-b528-11eb-92a1-281f3ad780d3.png)



## Future work

Future plans of adding features anf functionalities will be noted in the [issues section](https://github.com/asitav-sen/IFRSassistant/issues) because the list is too big :)


## Packages used

1. [shiny](https://shiny.rstudio.com)
2. [dplyr](https://dplyr.tidyverse.org)
3. [ggplot2](https://ggplot2.tidyverse.org)
4. [lubridate](https://lubridate.tidyverse.org)
5. [survival](https://cran.r-project.org/web/packages/survival/index.html)
6. [pec](https://cran.r-project.org/web/packages/pec/index.html)
7. [DT](https://rstudio.github.io/DT/)
8. [shinycsssloader](https://github.com/daattali/shinycssloaders)
9. [imfr](https://cran.r-project.org/web/packages/imfr/index.html)
10. [patchwork](https://cran.r-project.org/web/packages/patchwork/index.html)
11. [tidyr](https://tidyr.tidyverse.org)
12. [shinythemes](https://rstudio.github.io/shinythemes/)
13. [triangle](https://cran.r-project.org/web/packages/triangle/index.html)
14. [Diagrammer](https://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html)
15. [Forecast](https://cran.r-project.org/web/packages/forecast/index.html)




