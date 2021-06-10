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


options(shiny.reactlog = TRUE, appDir = getwd())

source("mod_basic.R")
source("secretary.R")
source("panel.R")
source("forplumber.R")
source("modals.R")

# Adding initial data


# Define UI for application
ui <- navbarPage(
    theme = shinytheme("cosmo"),
    title = "IFRSassistant",
    panel1
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Data
    
    # Some reactive values
    collateral.dt<-reactiveVal()
    transaction.dt<-reactiveVal()
    
    # Adding initial data to reactive values
    collateral.dt(read.csv("./data/collateral.csv") )
 
    
    transaction.dt(read.csv("./data/transactions.csv") )

    # Transforming Data
    new.data <- reactive({
        ndt<- transaction.dt()
            withProgress(message = "Trying to read",
                         detail = "Hope the handwriting is legible!",
                         value = 0,
                         {
                             setProgress(value = 1, message = "Patience test 1 of 2")
                             ndt <-
                                 ndt %>%
                                 # Change format of some columns
                                 mutate(
                                     origination_date = ymd(origination_date),
                                     maturity_date = ymd(maturity_date),
                                     report_date = ymd(report_date)
                                 ) %>%
                                 # Add age of loan, loan tenure in months, which are compulsory parameters
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
                                 # Adding delta of bureau score (ask me the reason if you do not know why)
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
        
        DT::datatable(
            new.data(),
            extensions = c("Buttons"),
            options = list(
                pageLength = 5,
                scrollX = TRUE,
                dom = 'Bfrtip',
                filter = list(position = 'top', clear = FALSE),
                buttons = list(
                    list(extend = "csv", text = "Download Visible", filename = "page",
                         exportOptions = list(
                             modifier = list(page = "current")
                         )
                    ),
                    list(extend = "csv", text = "Download All", filename = "data",
                         exportOptions = list(
                             modifier = list(page = "all")
                         )
                    )
                )
            )
        )
        
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
                         
                         imf.data <- tryCatch(
                             expr={
                                 imf_data(
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
                             },
                             error = function(e){          # Specifying error message
                                 showModal(
                                     modalDialog(
                                         "Error in IMF database. Sorry for the inconvenience. Can you please try again later?"
                                     )
                                 )
                                 message("Error with IMF database. This is usually temporary. Sorry for the inconvenience. Please try again later.")
                             },
                             finally = {                   # Specifying final message
                                 message("Error with IMF database. Please try again later.")
                             }
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
        
        DT::datatable(
            a,
            extensions = c("Buttons"),
            options = list(
                pageLength = 5,
                scrollX = TRUE,
                dom = 'Bfrtip',
                filter = list(position = 'top', clear = FALSE),
                buttons = list(
                    list(extend = "csv", text = "Download Visible", filename = "page",
                         exportOptions = list(
                             modifier = list(page = "current")
                         )
                    ),
                    list(extend = "csv", text = "Download All", filename = "data",
                         exportOptions = list(
                             modifier = list(page = "all")
                         )
                    )
                )
            )
        )
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
    
    
    
    # Model Selection. Using functions. In production these are to be converted to APIs
    selected_model <- reactive({
        req(input$start_model_selection,!is.null(dataset_with_eco()))
        model_sel(dff=dataset_with_eco())
    })
    
    # Preparing tables with predictions. Using functions. In production these are to be converted to APIs
    predicted_table <- reactive({
        input$start_model_selection
        req(!is.null(selected_model()))
        
        z<-predic_t(dff=dataset_with_eco(),
                    gdpfor=gdp.forecast(),
                    prfor=pats.forecast(),
                    maxdate=maxdate(), 
                    final.model= selected_model())
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
        req(nrow(predicted_table())>0)
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
        req(nrow(predicted_table())>0)
        req(!is.null(input$discount_rate))
        input$update
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
        sim_fin(simdata())
        
    })
    
    output$exp_credit_loss <- renderPlot({
        req(!is.null(finaldt()))
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
        showModal(
            modal1
        )
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
        showModal(
            modal2
        )
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
