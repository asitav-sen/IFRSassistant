# Estimate exposure at risk and create report (IFRS)
# Application developed by Asitav Sen

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(survival)
library(pec)
library(DT)
library(shinycssloaders)
library(imfr)
library(patchwork)
library(tidyr)
library(shinythemes)
library(triangle)
library(DiagrammeR)


options(shiny.reactlog = TRUE)
# Some functions used

# Function to get mode
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Function to return length of unique

lenun <- function(x) {
    length(unique(x))
}


# Module for a graph

# Module to show some stats
basicstatUI <- function(id) {
    ns <- NS(id)
    fluidRow(
        h3("No of Assets"),
        br(),
        p(
            "This section shows some basic information about the portfolio."
        ),
        plotOutput(ns("statplot")),
        verbatimTextOutput(ns("balancetext"))
    )
}

basicstatServer <- function(id, dt) {
    moduleServer(id,
                 function(input, output, session) {
                     df <- reactive({
                         dt %>%
                             ungroup() %>%
                             arrange(id, report_date) %>%
                             group_by(id) %>%
                             slice_max(report_date, n = 1) %>%
                             mutate(yr = lubridate::year(origination_date))
                     })
                     
                     
                     output$statplot <- renderPlot({
                         req(!is.null(df))
                         withProgress(message = "Plotting some graphs",
                                      detail = "Won't take long",
                                      value = 0,
                                      {
                                          setProgress(value = 1, message = "1 of 3..")
                                          
                                          p1 <-
                                              df() %>%
                                              group_by(yr, loan_status) %>%
                                              summarise(no_of_loans = n()) %>%
                                              ungroup() %>%
                                              ggplot(aes(
                                                  x = yr,
                                                  y = no_of_loans,
                                                  label = no_of_loans,
                                                  fill = factor(
                                                      loan_status,
                                                      levels = c("0", "1"),
                                                      labels = c("Good", "Bad")
                                                  )
                                              )) +
                                              geom_col(position = "dodge") +
                                              geom_text(aes(y = no_of_loans + 20), position = position_dodge(width = 1)) +
                                              labs(title = "No. of assets by year",
                                                   x = "Year",
                                                   y = "#") +
                                              theme_bw() +
                                              theme(legend.position = "none")
                                          setProgress(value = 2, message = "2 of 3..")
                                          
                                          p2 <-
                                              df() %>%
                                              group_by(asset_type, loan_status) %>%
                                              summarise(no_of_loans = n()) %>%
                                              ungroup() %>%
                                              ggplot(aes(
                                                  x = asset_type,
                                                  y = no_of_loans,
                                                  label = no_of_loans,
                                                  fill = factor(
                                                      loan_status,
                                                      levels = c("0", "1"),
                                                      labels = c("Good", "Bad")
                                                  )
                                              )) +
                                              geom_col(position = "dodge") +
                                              geom_text(aes(y = no_of_loans + 20), position = position_dodge(width = 1)) +
                                              labs(title = "No. of assets by asset type",
                                                   x = "Asset Type",
                                                   y = "#") +
                                              theme_bw() +
                                              theme(legend.position = "none")
                                          
                                          setProgress(value = 3, message = "3 of 3..")
                                          
                                          p3 <-
                                              df() %>%
                                              group_by(customer_type, loan_status) %>%
                                              summarise(no_of_loans = n()) %>%
                                              ungroup() %>%
                                              ggplot(
                                                  aes(
                                                      x = customer_type,
                                                      y = no_of_loans,
                                                      label = no_of_loans,
                                                      fill = factor(
                                                          loan_status,
                                                          levels = c("0", "1"),
                                                          labels = c("Good", "Bad")
                                                      )
                                                  )
                                              ) +
                                              geom_col(position = "dodge") +
                                              geom_text(aes(y = no_of_loans + 20), position = position_dodge(width = 1)) +
                                              labs(title = "No. of assets by customer type",
                                                   x = "Customer Type",
                                                   y = "#") +
                                              theme_bw() +
                                              theme(legend.position = "bottom",
                                                    legend.title = element_blank())
                                          
                                          setProgress(value = 4, message = "Patching..")
                                          
                                          p4 <- p1 / (p2 | p3)
                                          
                                          setProgress(value = 5, message = "Done..")
                                          
                                      })
                         
                         
                         p4
                         
                     })
                     
                     output$balancetext <- renderText({
                         paste0(
                             "Total assets (no.): ",
                             length(unique(df()$id)) ,
                             "\n",
                             "Total balance outstanding: ",
                             round(sum(df()$balance) / 1000000, 2),
                             " M"
                             
                         )
                     })
                     
                     
                 })
    
}


# Adding initial data


countries <- read.csv("./data/countries.csv")


# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("cosmo"),
    title = "IFRSassistant",
    tabPanel(
        "Home",
        fluidRow(column(
            width = 11,
            fluidRow(
                column(
                    width = 8,
                    tags$div(
                        tags$blockquote(
                            "IFRSassistant is being built to provide convenience to Finance and Accounting consultants and SMEs in finance industry."
                        ),
                        p(
                            "IFRS reporting, especially estimated loss calculation is a fairly complicated process involving Monte Carlo simulation, forecasting, survival modelling (or other predictive algorithm) and financial mathematics. 
                            Naturally, it is time consuming and has its share of hassles. This app attempts to avoid all the hassles of setting up environment and tools to perform the multistep analysis. 
                            This app will calculate the provisioning requirements in a few clicks and one can download the report with a single click!"
                        ),
                        p("The basic steps are mentioned in the diagram on the left hand side. The app is not in its most evolved form yet. 
                          There is a huge list of features and functions that I personally want to include and implement in future."),
                        h6("How to use?"),
                        p("Using the tool is very simple. You can upload two data sets. One that shows a certain number of transactions of each asset, along with some dates, parameters and event outcome.
                        And the other that contains value of collateral or estimated value of sales of the asset/hypothecated asset. Then proceed further with the clicks and in between select some parameters. 
                          For e.g. the discount rate to be applied, the most probable, maximum possible and minimum possible depreciation of value of the collateral. 
                          Once the simulation is done, you can download the report in pdf format."),
                        "If you are a R coder, you are welcome to contribute and help improve. Please visit the ",
                        tags$a(href = "https://github.com/asitav-sen/IFRSassistant", "github page"),
                        " or ",
                        tags$a(href = "www.asitavsen.com", "contact me."),
                        "Please use this ",
                        tags$a(href = "https://github.com/asitav-sen/IFRSassistant/issues", "link"),
                        " to report issues and/or request new features/functions.", "For general discussions, please use this",tags$a(href = "https://github.com/asitav-sen/IFRSassistant/discussions", "link"),
                        tags$br()
                    )
                ),
                column(
                    width = 4,
                    mermaid("
                graph TB
                A[Data Upload]-->B[Downloading macroeconomic data]
                B[Downloading macroeconomic data from IMF]-->C[Forecasting macroeconomic parameters]
                C[Forecasting macroeconomic parameters]-->D[Fit Survival Model]
                D[Fit Survival Model]-->E[Monte Carlo Simulation]
                E[Monte Carlo Simulation]-->F[Report]
                        ")
                )
            ),
        )),
        tags$hr(),
        fluidRow(
            column(
                width = 6,
                fluidRow(
                    h3("Data"),
                    br(),
                    p("This section shows the data (uploaded or inbuilt sample).")
                ),
                br(),
                withSpinner(dataTableOutput("up_data"),type = 7,
                            color = "black"),
                br(),
                actionButton("uploadnew", "Upload New Data", class="btn-light")
            ),
            column(width = 6,
                   br(),
                   basicstatUI("nofloans"))
            
        ),
        br(),
        br(),
        fluidRow(
            column(
                width = 6,
                h3("With Macroeconomy data"),
                p(
                    "In this section, we add macroeconomic data. Please select the country of the asset and click on fetch button. If download fails, click again or try after sometime."
                ),
                column(
                    width = 6,
                    selectInput(
                        "country",
                        "Country",
                        selected = "United States",
                        choices = countries$Country
                    )
                ),
                column(
                    width = 6,
                    br(),
                    actionButton("fetchimf", "Fetch IMF data", class = "glow")
                ),
                withSpinner(
                    dataTableOutput("fulldata"),
                    type = 7,
                    color = "black"
                )
            ),
            column(width = 6,
                   uiOutput("ac_button"),
                   uiOutput("expcalcu"))
        ),
        br(),
        br(),
        fluidRow(column(width = 11,
                        uiOutput("scenario_opts"))),
        br(),
        fluidRow(column(width = 11,
                        uiOutput("credit_loss"))),
        fluidRow(column(width = 11,
                        uiOutput("dlmanager")))
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Data
    collateral.dt<-reactiveVal()
    transaction.dt<-reactiveVal()
    
    collateral.dt(read.csv("./data/collateral.csv") )
 
    
    transaction.dt(read.csv("./data/transactions.csv") )

    
    new.data <- reactive({
        ndt<- transaction.dt()
            withProgress(message = "Trying to read",
                         detail = "Hope the handwriting is legible!",
                         value = 0,
                         {
                             setProgress(value = 1, message = "Patience test 1 of 2")
                             ndt <-
                                 ndt %>%
                                 # Change format of columns mentioned as date
                                 mutate(
                                     origination_date = ymd(origination_date),
                                     maturity_date = ymd(maturity_date),
                                     report_date = ymd(report_date)
                                 ) %>%
                                 # Add age of loan, loan tenure in months, which could be important parameters
                                 mutate(age_of_asset_months = round(as.numeric(
                                     report_date - origination_date
                                 ) / 30)) %>%
                                 mutate(loan_tenure_months = round(as.numeric(
                                     maturity_date - origination_date
                                 ) / 30)) %>%
                                 group_by(id) %>%
                                 # Arranging to avoid mistakes in lag
                                 arrange(report_date) %>%
                                 # Add lag of bureau score and total number of defaults. Lag added for delta creation
                                 mutate(
                                     cum_default = cumsum(default_flag),
                                     bureau_score_lag = ifelse(
                                         is.na(lag(bureau_score_orig, 1)),
                                         bureau_score_orig,
                                         lag(bureau_score_orig, 1)
                                     )
                                 ) %>%
                                 # Adding delta of bureau score
                                 mutate(bureau_score_delta = bureau_score_lag -
                                            bureau_score_orig) %>%
                                 # Adding quarter info for matching later with macroeconomic data
                                 mutate(qtr = paste0(year(report_date), "-Q", quarter(report_date))) %>%
                                 # Removing dummy
                                 dplyr::select(-bureau_score_orig)
                             setProgress(value = 2, message = "Patience test 1 of 2")
                         })
            ndt
        
        
    })
    
    

    # Show uploaded data
    output$up_data <- renderDataTable({
        datatable(new.data(),
                  options = list(
                      pageLength = 5,
                      scrollX = TRUE,
                      filter = list(position = 'top', clear = FALSE),
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  ))
    })
    
    maxdate <- reactive(max(new.data()$report_date))
    # Show some stats
    
    basicstatServer("nofloans", dt = new.data())
    
    
    
    # IMF Data
    
    imf.data <- reactive({
        input$fetchimf
        req(!is.null(new.data()), input$fetchimf)
        # Defining database and other parameters for query of macroeconomic data
        databaseID <- "IFS"
        startdate = min(new.data()$report_date)
        enddate = max(new.data()$report_date)
        country = countries[countries$Country == input$country,]$Alpha.2.code
        withProgress(message = "Extracting data from IMF",
                     detail = "Hope their server is up!",
                     value = 0,
                     {
                         setProgress(value = 1, message = "Trying to reach..")
                         imf.data <- imf_data(
                             databaseID,
                             c("NGDP_R_K_IX",
                               "PCPI_IX"),
                             country = "US",
                             start = startdate,
                             end = enddate,
                             freq = "Q",
                             return_raw = FALSE,
                             print_url = FALSE,
                             times = 3
                         )
                         setProgress(value = 2, message = "Done")
                     })
        
        imf.data
    })
    
    # Adding macroeconomic data. Currently GDP and prices data are extracted
    dataset_with_eco <- reactive({
        req(!is.null(imf.data()))
        
        withProgress(message = "Trying Hard",
                     detail = "Hang on",
                     value = 0,
                     {
                         setProgress(value = 1, message = "working..")
                         
                         # New dataset by joining GDP and prices data
                         dataset_with_eco <-
                             new.data() %>%
                             left_join(imf.data(), by = c("qtr" = "year_quarter")) %>%
                             rename(gdp = NGDP_R_K_IX, prices = PCPI_IX) %>%
                             select(-iso2c) %>%
                             group_by(id) %>%
                             mutate(gdp_lag = lag(gdp, 1),
                                    prices_lag = lag(prices, 1)) %>%
                             dplyr::select(-qtr) %>%
                             dplyr::select(-c(gdp, prices))
                         
                         # removing rows with no macroeconomic data
                         dataset_eco <-
                             dataset_with_eco[!is.na(dataset_with_eco$gdp_lag) &
                                                  !is.na(dataset_with_eco$prices_lag), ]
                         setProgress(value = 2, message = "working..")
                     })
        
        return(dataset_eco)
    })
    
    # GDP Forecast
    
    gdp.forecast <- reactive({
        imf.data <- imf.data()
        imf.data$year_quarter <-
            zoo::as.yearqtr(imf.data$year_quarter, format = "%Y-Q%q")
        ga <- imf.data[, c(2, 3)]
        minqg <- min(imf.data$year_quarter)
        gats <- ts(ga$NGDP_R_K_IX,
                   start = minqg,
                   frequency = 4)
        
        withProgress(message = "Forecasting GDP",
                     detail = "Hang on",
                     value = 0,
                     {
                         setProgress(value = 1, message = "working..")
                         fit.gdp <-
                             forecast::auto.arima(gats, seasonal = FALSE)
                         gdp.forecast <-
                             fit.gdp %>% forecast::forecast(h = 60)
                         setProgress(value = 2, message = "Done")
                     })
        gdp.forecast
    })
    
    # Price forecast
    pats.forecast <- reactive({
        imf.data <- imf.data()
        imf.data$year_quarter <-
            zoo::as.yearqtr(imf.data$year_quarter, format = "%Y-Q%q")
        gp <- imf.data[, c(2, 4)]
        minqg <- min(imf.data$year_quarter)
        pats <- ts(gp$PCPI_IX, start = minqg, frequency = 4)
        withProgress(message = "Forecasting price",
                     detail = "Hang on",
                     value = 0,
                     {
                         setProgress(value = 1, message = "working..")
                         fit.pats <-
                             forecast::auto.arima(pats, seasonal = FALSE)
                         pats.forecast <-
                             fit.pats %>% forecast::forecast(h = 60)
                         setProgress(value = 2, message = "Done")
                     })
        
        pats.forecast
    })
    
    # Show full data
    
    output$fulldata <- renderDataTable({
        validate(need(!is.null(dataset_with_eco()),
                      message = "Data Not ready yet"))
        a <- dataset_with_eco()
        datatable(a,
                  options = list(
                      pageLength = 5,
                      scrollX = TRUE,
                      filter = list(position = 'top', clear = FALSE),
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  ))
    })
    
    # Probability of default
    
    output$expcalcu <- renderUI({
        req(!is.null(dataset_with_eco()), input$fetchimf)
        fluidRow(
            h3("Probability of asset going bad"),
            p(
                "In this section you can see the distribution of probabilities of the assets going bad by year, upto 5 years from the last reporting date. Please click the button to proceed."
            ),
            actionButton(
                "start_model_selection",
                "Initiate Model Selection",
                class = "glow"
            ),
            withSpinner(
                plotOutput("probability_default"),
                type = 7,
                color = "black"
            )
        )
    })
    
    
    
    # Model Selection
    selected_model <- reactive({
        req(input$start_model_selection,!is.null(dataset_with_eco()))
        withProgress(message = "Testing",
                     detail = "your patience!",
                     value = 0,
                     {
                         setProgress(value = 1, message = "Formatting Data")
                         
                         df <- dataset_with_eco() %>%
                             dplyr::select(!where(is.Date))
                         df <- df %>%
                             select(-names(which(apply(
                                 df, 2, lenun
                             ) == 1)))
                         
                         df$asset_type <- as.factor(df$asset_type)
                         #df$supplier <- as.factor(df$supplier)
                         df$customer_type <-
                             as.factor(df$customer_type)
                         
                         setProgress(value = 2, message = "Creating initial formula")
                         
                         # some cleaning
                         variables <-
                             colnames(df)[!colnames(df) %in% c("id", "age_of_asset_months", "loan_status")]
                         form <-
                             as.formula(paste0(
                                 "Surv(age_of_asset_months, loan_status) ~",
                                 paste(variables, collapse = "+")
                             ))
                         surv.res <- coxph(form, data = df, id = id)
                         res <-
                             as.data.frame(summary(surv.res)$coefficients)
                         selvars <-
                             res %>%
                             filter(is.finite(`Pr(>|z|)`)) %>%
                             filter(is.finite(z)) %>%
                             rownames()
                         for (j in 1:length(variables)) {
                             selvars[grep(pattern = variables[j], selvars)] <- variables[j]
                         }
                         selvars <- unique(selvars)
                         
                         form <-
                             as.formula(paste0(
                                 "Surv(age_of_asset_months, loan_status) ~",
                                 paste(selvars, collapse = "+")
                             ))
                         setProgress(value = 3, message = "Are you still not annoyed?")
                         
                         scores <- rep(NA, 5)
                         variables <- selvars
                         a <- length(variables)
                         f <- vector(mode = "list", length = a)
                         scores = vector(length = a)
                         setProgress(value = 4, message = "No way! Are you still waiting?")
                         for (i in 1:a) {
                             v <- variables[i:a]
                             #n<-paste0("form",i)
                             f[[i]] <-
                                 coxph(
                                     as.formula(
                                         paste0(
                                             "Surv(age_of_asset_months, loan_status) ~",
                                             paste(v, collapse = "+")
                                         )
                                     ),
                                     data = df,
                                     id = id,
                                     x = T,
                                     y = T
                                 )
                             perror <-
                                 pec(
                                     object = f[[i]],
                                     formula = form,
                                     splitMethod = "cvK5",
                                     data = df
                                 )
                             scores[i] <-
                                 1 - ibs(perror)["coxph",] / ibs(perror)["Reference",]
                         }
                         setProgress(value = 5, message = "Ok. I give up. You win!")
                         final.model <-
                             f[[which(scores == max(scores, na.rm = T))]]
                         setProgress(value = 6, message = "Ha ha! Not so fast!")
                     })
        
        final.model
    })
    
    # Preparing tables with predictions
    predicted_table <- reactive({
        input$start_model_selection
        req(!is.null(selected_model()))
        
        final.model <- selected_model()
        withProgress(message = "Still working",
                     detail = "Yes, I'm slow :(",
                     value = 0,
                     {
                         setProgress(value = 1, message = "Formatting")
                         df <- dataset_with_eco() %>%
                             dplyr::select(!where(is.Date))
                         df$asset_type <- as.factor(df$asset_type)
                         df$supplier <- as.factor(df$supplier)
                         df$customer_type <-
                             as.factor(df$customer_type)
                         maxdate <- maxdate()
                         gdp.forecast <-
                             as.data.frame(gdp.forecast()) %>%
                             mutate(qtr = rownames(.)) %>%
                             mutate(stringr::str_replace(qtr, " ", "-"))
                         pats.forecast <-
                             as.data.frame(pats.forecast()) %>%
                             mutate(qtr = rownames(.)) %>%
                             mutate(stringr::str_replace(qtr, " ", "-"))
                         setProgress(value = 2, message = "But need to run some calculations")
                         z <-
                             df %>%
                             group_by(id) %>%
                             slice_max(age_of_asset_months, n = 1) %>%
                             ungroup() %>%
                             mutate(emi = balance / (loan_tenure_months -
                                                         age_of_asset_months)) %>%
                             mutate(balance.original = balance) %>%
                             mutate(age.original = age_of_asset_months) %>%
                             mutate(risk_current = 1 - exp(
                                 -predict(
                                     final.model,
                                     .,
                                     type = "expected",
                                     collapse = id
                                 )
                             )) %>%
                             mutate(balance = balance - emi * 12) %>%
                             mutate(age_of_asset_months = age_of_asset_months +
                                        12) %>%
                             mutate(
                                 gdp_lag = gdp.forecast[gdp.forecast$qtr == zoo::as.yearqtr(maxdate + months(11)),]$`Point Forecast`,
                                 prices_lag = pats.forecast[pats.forecast$qtr ==
                                                                zoo::as.yearqtr(maxdate + months(11)),]$`Point Forecast`
                             ) %>%
                             mutate(risk_1yr = 1 - exp(
                                 -predict(
                                     final.model,
                                     .,
                                     type = "expected",
                                     collapse = id
                                 )
                             )) %>%
                             mutate(balance = balance - emi * 12) %>%
                             mutate(age_of_asset_months = age_of_asset_months +
                                        12) %>%
                             mutate(
                                 gdp_lag = gdp.forecast[gdp.forecast$qtr == zoo::as.yearqtr(maxdate + months(23)),]$`Point Forecast`,
                                 prices_lag = pats.forecast[pats.forecast$qtr ==
                                                                zoo::as.yearqtr(maxdate + months(23)),]$`Point Forecast`
                             ) %>%
                             mutate(risk_2yr = 1 - exp(
                                 -predict(
                                     final.model,
                                     .,
                                     type = "expected",
                                     collapse = id
                                 )
                             )) %>%
                             mutate(balance = balance - emi * 12) %>%
                             mutate(age_of_asset_months = age_of_asset_months +
                                        12) %>%
                             mutate(
                                 gdp_lag = gdp.forecast[gdp.forecast$qtr == zoo::as.yearqtr(maxdate + months(35)),]$`Point Forecast`,
                                 prices_lag = pats.forecast[pats.forecast$qtr ==
                                                                zoo::as.yearqtr(maxdate + months(35)),]$`Point Forecast`
                             ) %>%
                             mutate(risk_3yr = 1 - exp(
                                 -predict(
                                     final.model,
                                     .,
                                     type = "expected",
                                     collapse = id
                                 )
                             )) %>%
                             mutate(balance = balance - emi * 12) %>%
                             mutate(age_of_asset_months = age_of_asset_months +
                                        12) %>%
                             mutate(
                                 gdp_lag = gdp.forecast[gdp.forecast$qtr == zoo::as.yearqtr(maxdate + months(47)),]$`Point Forecast`,
                                 prices_lag = pats.forecast[pats.forecast$qtr ==
                                                                zoo::as.yearqtr(maxdate + months(47)),]$`Point Forecast`
                             ) %>%
                             mutate(risk_4yr = 1 - exp(
                                 -predict(
                                     final.model,
                                     .,
                                     type = "expected",
                                     collapse = id
                                 )
                             )) %>%
                             mutate(balance = balance - emi * 12) %>%
                             mutate(age_of_asset_months = age_of_asset_months +
                                        12) %>%
                             mutate(
                                 gdp_lag = gdp.forecast[gdp.forecast$qtr == zoo::as.yearqtr(maxdate + months(59)),]$`Point Forecast`,
                                 prices_lag = pats.forecast[pats.forecast$qtr ==
                                                                zoo::as.yearqtr(maxdate + months(59)),]$`Point Forecast`
                             ) %>%
                             mutate(risk_5yr = 1 - exp(
                                 -predict(
                                     final.model,
                                     .,
                                     type = "expected",
                                     collapse = id
                                 )
                             )) %>%
                             group_by(id) %>%
                             tidyr::pivot_longer(
                                 cols = c(
                                     "risk_current",
                                     "risk_1yr",
                                     "risk_2yr",
                                     "risk_3yr",
                                     "risk_4yr",
                                     "risk_5yr"
                                 )
                             ) %>%
                             mutate(r_n = row_number()) %>%
                             mutate(t.emi = emi + emi * 12 * (r_n - 1)) %>%
                             mutate(balance = ifelse(t.emi == max(emi), balance, balance -
                                                         t.emi)) %>%
                             mutate(balance = ifelse(balance <= 0, 0, balance)) %>%
                             filter(t.emi > 0)
                         setProgress(value = 3, message = "At last. Now wait for the plot please.")
                     })
        z
        
    })
    
    
    # Plot Probability of default
    output$probability_default <- renderPlot({
        input$start_model_selection
        req(predicted_table())
        withProgress(message = "Plotting",
                     detail = "Just a moment",
                     value = 0,
                     {
                         setProgress(value = 1, message = "Rushing")
                         pl <- predicted_table() %>%
                             group_by(name) %>%
                             #summarise(pd.me=median(value), pd.min=quantile(value,0.05), pd.max=quantile(value, 0.95))%>%
                             slice_head(n = 6) %>%
                             mutate(name = factor(
                                 name,
                                 levels = c(
                                     "risk_current",
                                     "risk_1yr",
                                     "risk_2yr",
                                     "risk_3yr",
                                     "risk_4yr",
                                     "risk_5yr"
                                 )
                             )) %>%
                             filter(!is.na(name)) %>%
                             ggplot(aes(
                                 x = name,
                                 y = value,
                                 fill = name
                             )) +
                             geom_violin() +
                             geom_boxplot(width = 0.1,
                                          color = "black",
                                          alpha = 0.2) +
                             labs(x = "",
                                  y = "Probability",
                                  title = "Probability of default") +
                             theme_bw() +
                             theme(legend.title = element_blank(),
                                   legend.position = "bottom")
                         setProgress(value = 2, message = "Done")
                     })
        pl
    })
    
    # Show scenario creation options when exposures and risks are calculated
    
    output$scenario_opts <- renderUI({
        req(!is.null(predicted_table()),
            input$start_model_selection)
        fluidRow(
            # remove class when btn clicked
            br(),
            h3("Possible Scenarios"),
            p(
                "In this section, the parameters for simulation will be selected. Please click on start simulation button to proceed"
            ),
        )
        fluidRow(
            column(
                width = 3,
                h5("Select expected change in collateral value"),
                p("-ve means reduction in value"),
                sliderInput(
                    "mode_dep",
                    "Most Probable",
                    min = -1,
                    max = 1,
                    value = -0.7,
                    step = 0.1
                ),
                sliderInput(
                    "min_dep",
                    "Minimum",
                    min = -1,
                    max = 1,
                    value = -1,
                    step = 0.1
                ),
                sliderInput(
                    "max_dep",
                    "Max",
                    min = -1,
                    max = 1,
                    value = 1,
                    step = 0.1
                )
            ),
            column(
                width = 3,
                sliderInput(
                    "discount_rate",
                    "Select Discount rate / WACC",
                    min = 0.00,
                    max = 0.2,
                    value = 0.02,
                    step = 0.01
                ),
                actionButton("update", "Start Simulation", class = "glow"),
                h3("Please do not forget to click this button.")
            ),
            column(
                width = 6,
                p(
                    "This section shows the possible exposure in case the asset goes bad."
                ),
                plotOutput("exposure_on_default")
            )
        )
        
    })
    
    # Exposure on Default
    
    output$exposure_on_default <- renderPlot({
        input$update
        req(!is.null(predicted_table()),!is.null(input$discount_rate))
        discount_rate_pa <- input$discount_rate
        
        withProgress(message = "Plotting",
                     detail = "Just a moment",
                     value = 0,
                     {
                         setProgress(value = 1, message = "Almost there")
                         pl <- predicted_table() %>%
                             mutate(pv.balance = balance / (1 + discount_rate_pa) ^
                                        (r_n - 1)) %>%
                             mutate(exposure_on_default = pv.balance * value) %>%
                             group_by(name) %>%
                             summarise(
                                 pv.balance = sum(pv.balance),
                                 exposure_on_default = sum(exposure_on_default)
                             ) %>%
                             mutate(name = factor(
                                 name,
                                 levels = c(
                                     "risk_current",
                                     "risk_1yr",
                                     "risk_2yr",
                                     "risk_3yr",
                                     "risk_4yr",
                                     "risk_5yr"
                                 )
                             )) %>%
                             pivot_longer(
                                 cols = c("pv.balance", "exposure_on_default"),
                                 names_to = "type",
                                 values_to = "amount"
                             ) %>%
                             ggplot(aes(
                                 x = name,
                                 y = amount / 1000000,
                                 fill = type,
                                 label = paste0(round(amount / 1000000), " M")
                             )) +
                             geom_col(position = "dodge") +
                             geom_text(aes(y = (amount / 1000000) + 5), position = position_dodge(width = 1)) +
                             labs(
                                 x = "",
                                 y = "Amount",
                                 title = "Exposure on default",
                                 subtitle = "Amounts discounted"
                             ) +
                             theme_bw() +
                             theme(legend.title = element_blank(),
                                   legend.position = "bottom")
                         setProgress(value = 2, message = "Done")
                     })
        
        pl
        
    })
    
    output$credit_loss <- renderUI({
        req(!is.null(predicted_table()), input$update)
        validate(
            need(input$mode_dep <= input$max_dep, message = "Most probable value should be less than or equal to Max"),
            need(input$min_dep <= input$mode_dep, message = "Min value should be less than or equal to Most probable value"),
            need(input$min_dep <= input$mode_dep, message = "Min value should be less than or equal to Max value")
        )
        fluidRow(
            # remove class when btn clicked
            h3("Simulated Credit Loss with probability"),
            p(
                "This section shows the distribution of the simlated possible losses. Please select the time to check the corresponding distribution."
            ),
            br(),
            selectInput(
                "riskperiod",
                "Select time",
                choices = c(
                    "risk_current",
                    "risk_1yr",
                    "risk_2yr",
                    "risk_3yr",
                    "risk_4yr",
                    "risk_5yr"
                ),
                selected = "risk_1yr"
            ),
            br(),
            column(
                width = 6,
                p(
                    "Please click an drag to check the probabilities between ranges of possible loss."
                ),
                plotOutput("simres",
                           brush = brushOpts(
                               id = "sim_res_sel", direction = "x"
                           )),
                verbatimTextOutput("cumprob")
            ),
            column(
                width = 6,
                p(
                    "This section shows the overall estimated credit loss by year from last reporting date, upto 5 years"
                ),
                plotOutput("exp_credit_loss")
            )
            
        )
    })
    
    # collateral<-reactive({
    #     collateral
    # })
    
    simdata <- reactive({
        input$update
        req(!is.null(predicted_table()))
        collateral<-collateral.dt()
        discount_rate_pa <- input$discount_rate
        withProgress(message = "Monte Carlo Simulation",
                     detail = "1000 simulations",
                     value = 0,
                     {
                         setProgress(value = 1, message = "preparing")
                         zz <-
                             predicted_table() %>%
                             ungroup() %>%
                             group_by(id) %>%
                             mutate(pv.balance = balance / (1 + discount_rate_pa) ^
                                        (r_n - 1)) %>%
                             left_join(collateral, by = "id") %>%
                             ungroup() %>%
                             select(name, pv.balance, value, collateral)
                         setProgress(value = 2, message = "simulating")
                         
                         sim.t <-
                             data.frame(matrix(ncol = 1000, nrow = nrow(zz)))
                         colnames(sim.t) <-
                             paste0("sim", seq(1:1000))
                         sim.col.prob <-
                             (1 + rtriangle(
                                 1000,
                                 a = input$min_dep,
                                 b = input$max_dep,
                                 c = input$mode_dep
                             ))
                         
                         for (i in 1:1000) {
                             sim.t[, i] <-
                                 round(zz$value * (zz$pv.balance - zz$collateral * sim.col.prob[i]),
                                       2)
                         }
                         
                         sim.t[sim.t < 0] <- 0
                         sim.t <- cbind(zz, sim.t)
                         
                         setProgress(value = 3, message = "Done")
                         
                     })
        
        sim.t
    })
    
    simresdata <- reactive({
        dt <- simdata() %>%
            filter(name == input$riskperiod) %>%
            ungroup() %>%
            select(matches("sim?"))
        hist.pro <- density(colSums(dt))
        hist.pro
    })
    
    output$simres <- renderPlot({
        req(!is.null(simresdata()))
        hist.pro <- simresdata()
        pro_dens <- data.frame(hist.pro$x, hist.pro$y)
        
        ggplot(pro_dens,
               aes(x = hist.pro.x,
                   y = hist.pro.y)) + geom_area(aes(y = hist.pro.y)) +
            labs(
                x = "Amount",
                y = "Chance",
                title = "Simulated expected credit loss",
                subtitle = "Amounts discounted"
            ) +
            theme_bw()
    })
    
    output$cumprob <- renderText({
        req(!is.null(input$sim_res_sel), !is.null(simresdata()))
        hist.pro <- simresdata()
        pro_dens <- data.frame(hist.pro$x, hist.pro$y)
        res <- brushedPoints(pro_dens, input$sim_res_sel)
        if (nrow(res) == 0) {
            return()
        }
        paste0(
            "Probability of the loss to be between ",
            min(res$hist.pro.x),
            " and ",
            max(res$hist.pro.x),
            " is " ,
            sum(res$hist.pro.y)
        )
    })
    
    finaldt <- reactive({
        dtc <- simdata() %>%
            filter(name == "risk_current") %>%
            ungroup() %>%
            select(matches("sim?")) %>%
            colSums()
        
        dt1 <- simdata() %>%
            filter(name == "risk_1yr") %>%
            ungroup() %>%
            select(matches("sim?")) %>%
            colSums()
        
        dt2 <- simdata() %>%
            filter(name == "risk_2yr") %>%
            ungroup() %>%
            select(matches("sim?")) %>%
            colSums()
        
        dt3 <- simdata() %>%
            filter(name == "risk_3yr") %>%
            ungroup() %>%
            select(matches("sim?")) %>%
            colSums()
        
        dt4 <- simdata() %>%
            filter(name == "risk_4yr") %>%
            ungroup() %>%
            select(matches("sim?")) %>%
            colSums()
        
        dt5 <- simdata() %>%
            filter(name == "risk_5yr") %>%
            ungroup() %>%
            select(matches("sim?")) %>%
            colSums()
        
        hisc <- density(dtc)
        his1 <- density(dt1)
        his2 <- density(dt2)
        his3 <- density(dt3)
        his4 <- density(dt4)
        his5 <- density(dt5)
        
        dtc <- data.frame(x = hisc$x, y = hisc$y)
        dtc$r <- rep("risk_0yr", times = nrow(dtc))
        dt1 <- data.frame(x = his1$x, y = his1$y)
        dt1$r <- rep("risk_1yr", times = nrow(dt1))
        dt2 <- data.frame(x = his2$x, y = his2$y)
        dt2$r <- rep("risk_2yr", times = nrow(dt2))
        dt3 <- data.frame(x = his3$x, y = his3$y)
        dt3$r <- rep("risk_3yr", times = nrow(dt3))
        dt4 <- data.frame(x = his4$x, y = his4$y)
        dt4$r <- rep("risk_4yr", times = nrow(dt4))
        dt5 <- data.frame(x = his5$x, y = his5$y)
        dt5$r <- rep("risk_5yr", times = nrow(dt5))
        
        df <- rbind(dtc, dt1, dt2, dt3, dt4, dt5)
        df
        
    })
    
    output$exp_credit_loss <- renderPlot({
        finaldt() %>%
            mutate(amount = x * y) %>%
            group_by(r) %>%
            summarise(amount = sum(amount)) %>%
            ggplot(aes(
                x = r,
                y = round(amount / 1000),
                label = paste0(round(amount / 1000), " K")
            )) +
            geom_col() +
            geom_text(aes(y = round(amount / 1000) + 1)) +
            labs(
                title = "Estimated Credit Loss",
                subtitle = "Weighted sum of 1000 simulations",
                x = "",
                y = "Amount"
            ) +
            theme_bw()
    })
    
    output$dlmanager <- renderUI({
        input$update
        req(!is.null(finaldt()))
        fluidRow(
            textInput("author", "Enter Your Name"),
            downloadButton("report", "Generate report")
        )
    })
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "easyIFRSriskReport.pdf",
        content = function(file) {
            withProgress(message = "Monte Carlo Simulation",
                         detail = "1000 simulations",
                         value = 0,
                         
                         {
                             setProgress(value = 1, message = "Copying to temp directory")
                             
                             
                             tempReport <-
                                 file.path(tempdir(), "easyIFRSriskReport.Rmd")
                             file.copy("easyIFRSriskReport.Rmd", tempReport, overwrite = TRUE)
                             setProgress(value = 2, message = "Passing parameters")
                             
                             ndata <- new.data()
                             edata <- dataset_with_eco()
                             predtable <- predicted_table()
                             simudata <- simdata()
                             fidata <- finaldt()
                             gfor <- gdp.forecast()
                             pfor <- pats.forecast()
                             
                             params <- list(
                                 user = input$author,
                                 newdata = ndata,
                                 predtabledata = predtable,
                                 disrate = input$discount_rate,
                                 simdata = simudata,
                                 finaldata = fidata,
                                 gdpf = gfor,
                                 pf = pfor
                             )
                             setProgress(value = 3, message = "Knitting. Hang on.")
                             
                             rmarkdown::render(
                                 tempReport,
                                 output_file = file,
                                 params = params,
                                 envir = new.env(parent = globalenv())
                             )
                             setProgress(value = 4, message = "Done.")
                         })
        }
    )
    
    # File uploading Module
    
    observeEvent(input$uploadnew, {
        showModal(modalDialog(
            title = "Upload files",
            fluidRow(
                column(
                    width = 6,
                    p("Add Transaction file"),
                    fileInput("transaction", "Choose CSV File of transactions",
                              multiple = F,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    p("Add Collateral file"),
                    fileInput("collaterals", "Choose CSV File of collaterals",
                              multiple = F,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    checkboxInput("header", "Header", TRUE),
                    fluidRow(
                        column(
                            width = 6,
                            radioButtons("sep", "Separator",
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ",")
                        ),
                        column(
                            width = 6,
                            radioButtons("quote", "Quote",
                                         choices = c(None = "",
                                                     "Double Quote" = '"',
                                                     "Single Quote" = "'"),
                                         selected = '"')
                        )
                    )
                ),
                column(
                    width = 6,
                    fluidRow(
                        tags$div(
                            "Following columns are required in the transaction file",
                            tags$ul(
                                tags$li("A unique identifier of asset (id)"), 
                                tags$li("Date on which last report was generated. The last reporting date of all the assets must be same."), 
                                tags$li("Origination Date"),
                                tags$li("Maturity Date"),
                                tags$li("Default flag i.e. whether previously defaulted"),
                                tags$li("Bureau/internal credit score"),
                                tags$li("Current loan status (1 is bad and 0 is good)"),
                                tags$li("Asset classification"),
                                tags$li("Customer classification"),
                                tags$li("Asset balance")
                            ),
                            "The collateral file should contain present value of the collateral/possible price of sales of the asset/sales of hypothecated asset etc."
                        )
                    )
                )
            ),
            easyClose = F,
            size = "l",
            footer = tagList(
                actionButton("uploadfiles", "Proceed"),
                actionButton("closemodal1","Close")
            )
        ))
    })
    
    observeEvent(input$closemodal1,{
        removeModal()
    })
    observeEvent(input$closemodal2,{
        removeModal()
    })
    
    # Uploading temp file and collecting info about the columns
    observeEvent(input$uploadfiles,{
        
        df.tr <- reactive({
            inFile <- input$transaction
            if (is.null(inFile))
                return(NULL)
            df <- read.csv(inFile$datapath,
                                   header = input$header,
                                   sep = input$sep,
                                   quote = input$quote)
            df
        })
        
        df.c <- reactive({
            inFile <- input$collaterals
            if (is.null(inFile))
                return(NULL)
            df <- read.csv(inFile$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote)
            df
        })
        updateVarSelectInput("collateralid","Select id column", df.c(),session = getDefaultReactiveDomain())
        updateVarSelectInput("collateralvalue","Select Collateral value column", df.c(),session = getDefaultReactiveDomain())
        updateVarSelectInput("reportdate","Select report date column", df.tr(),session = getDefaultReactiveDomain())
        updateVarSelectInput("origindate","Select origin date column", df.tr(),session = getDefaultReactiveDomain())
        updateVarSelectInput("maturitydate","Select maturity date column", df.tr(),session = getDefaultReactiveDomain())
        updateVarSelectInput("assettype","Select asset classifier column", df.tr(),session = getDefaultReactiveDomain())
        updateVarSelectInput("customertype","Select customer classifier column", df.tr(),session = getDefaultReactiveDomain())
        updateVarSelectInput("otherfact","Select any other classifier column", df.tr(),session = getDefaultReactiveDomain())
        updateVarSelectInput("bureauscore","Select bureau score column", df.tr(),session = getDefaultReactiveDomain())
        updateVarSelectInput("balance","Select asset balance column", df.tr(),session = getDefaultReactiveDomain())
        updateVarSelectInput("status","Select loan status", df.tr(),session = getDefaultReactiveDomain())
        updateVarSelectInput("defaultflag","Select default flag column", df.tr(),session = getDefaultReactiveDomain())
        updateVarSelectInput("transid","Select id column", df.tr(),session = getDefaultReactiveDomain())
        
    })
    
    # Modal 2
    
    observeEvent(input$uploadfiles, {
        showModal(modalDialog(
            title = "File formatting",
            fluidRow(
                
                column(
                    width = 6,
                    p("Transactions file"),
                    varSelectInput("transid","Select id column", NULL),
                    varSelectInput("reportdate","Select report date column", NULL),
                    varSelectInput("origindate","Select origin date column", NULL),
                    varSelectInput("maturitydate","Select maturity date column", NULL),
                    varSelectInput("assettype","Select asset classifier column", NULL),
                    varSelectInput("customertype","Select customer classifier column", NULL),
                    varSelectInput("otherfact","Select any other classifier column", NULL, multiple = T)
                ),
                column(
                    width = 6,
                    p("Transactions file"),
                    varSelectInput("bureauscore","Select bureau score column", NULL),
                    varSelectInput("balance","Select asset balance column", NULL),
                    varSelectInput("status","Select loan status column", NULL),
                    varSelectInput("defaultflag","Select default flag column", NULL),
                    radioButtons("dateformat","Select date format",choices = c("dmy","ymd"), selected = "ymd", inline = T),
                    p("Collateral File"),
                    varSelectInput("collateralid","Select id column", NULL),
                    varSelectInput("collateralvalue","Select Collateral value column", NULL)
                )
            ),
            easyClose = F,
            size = "l",
            footer = tagList(
                actionButton("confirmupload", "Confirm"),
                actionButton("closemodal2","Close")
            )
        ))
    })
    
    

    observeEvent(input$confirmupload,{
        transaction.dt(
            {
                df <- read.csv(input$transaction$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
                head(df)
                
                df<-
                    df %>% 
                    rename(report_date=input$reportdate,
                           origination_date=input$origindate,
                           maturity_date=input$maturitydate,
                           asset_type=input$assettype,
                           customer_type=input$customertype,
                           bureau_score_orig=input$bureauscore,
                           balance=input$balance,
                           loan_status=input$status,
                           default_flag=input$defaultflag,
                           id=input$transid)
                
                if(input$dateformat=="ymd"){
                    df$report_date<-ymd(df$report_date)
                    df$origination_date<-ymd(df$origination_date)
                    df$maturity_date<-ymd(df$maturity_date)
                } else {
                    df$report_date<-dmy(df$report_date)
                    df$origination_date<-dmy(df$origination_date)
                    df$maturity_date<-dmy(df$maturity_date)
                }
                
                df$loan_status<-as.integer(df$loan_status)
                
                # Covert to factors
                df$asset_type<-as.factor(df$asset_type)
                df$customer_type<-as.factor(df$customer_type)
                if(!is.null(input$otherfact)){
                    for(i in 1:length(input$otherfact)){
                        df$input$otherfact[i]<-as.factor(df$input$otherfact[i])
                    } 
                }
                
                df
            }
        )
        
        collateral.dt(
            {
                df <- read.csv(input$collaterals$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
                df<-
                    df %>% 
                    rename(id=input$collateralid,
                           collateral=input$collateralvalue)
                df
        }
        )
        removeModal()
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
