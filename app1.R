options(shiny.maxRequestSize = 30 * 1024 ^ 2)
library(shiny)
library(bslib)
library(DT)
library(data.table)
library(scorecard)
library(dplyr)
library(corrplot)
library(streamlineR)
options(shiny.reactlog=TRUE)

source("helpers.R")
source("panel1.R")

fudata <- read.csv("./data/fdata.csv")

# Define UI for data upload app ----
ui <- fluidPage(# Theme
    #theme = bs_theme(version = 4, bootswatch = "litera"),
    
    titlePanel("Credit Risk"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(# Sidebar panel for inputs ----
                  sidebarPanel(
                      width = 3,
                      fluidRow(
                          textOutput("info_ini"),
                          uiOutput("column_selector")
                      )
                  ),
                  mainPanel(
                      tabsetPanel(
                          panel1,
                          tabPanel("1 year risk",
                                   
                                   "Building"),
                          tabPanel("lifetime risk",
                                   
                                   " Need to build")
                      )
                      
                      
                  )))



server <- function(input, output) {
    
    # If upload button is clicked, show module
    
    observeEvent(input$upload,{
        showModal(modalDialog(
            modal1,
            title = "Upload Data",
            size = "l",
            easyClose = FALSE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "Ok")
            )
        )
        )
    })
    
    
    # Store it in a reactive frame
    
    tempdata <- eventReactive(c(input$confirm, input$newdata),{
        #If confirm button is clicked read data and store
        if(input$confirm==T){
            ndata <- read.csv(
                input$newdata$datapath,
                header = input$header,
                sep = input$sep,
                quote = input$quote
            )
        }
    })
    
    observeEvent(c(input$confirm, tempdata()),{
        updateVarSelectInput(
            session = getDefaultReactiveDomain(),
            "event_select",
            label = "Select Event Column",
            data = tempdata(),
            selected = NULL
        )
        updateVarSelectInput(
            session = getDefaultReactiveDomain(),
            "amount_select",
            label = "Select Loan Balance Column",
            data = tempdata(),
            selected = NULL
        )
        updateVarSelectInput(
            session = getDefaultReactiveDomain(),
            "date_origin",
            label = "Select origin date column",
            data = tempdata(),
            selected = NULL
        )
        updateVarSelectInput(
            session = getDefaultReactiveDomain(),
            "date_maturity",
            label = "Select maturity date column",
            data = tempdata(),
            selected = NULL
        )
        updateVarSelectInput(
            session = getDefaultReactiveDomain(),
            "assetid",
            label = "Select asset identifier (unique) column",
            data = tempdata(),
            selected = NULL
        )
        updateVarSelectInput(
            session = getDefaultReactiveDomain(),
            "factorlist",
            label = "Select factor column",
            data = tempdata(),
            selected = NULL
        )
        updateVarSelectInput(
            session = getDefaultReactiveDomain(),
            "otherdate",
            label = "Other date columns",
            data = tempdata(),
            selected = NULL
        )
        
        
    })
    
    fdata<- reactive({
        if(input$upload==F){
            ndata<-fudata
            colnames(ndata)[colnames(ndata)=="status"]<-"default_event"
            ndata$origination_date<-lubridate::ymd(ndata$origination_date)
            ndata$maturity_date<-lubridate::ymd(ndata$maturity_date)
            ndata[is.na(ndata)]<-0
            return(ndata)
        }
        
    if(!is.null(input$event_select) | !is.null(input$amount_select) |
               !is.null(input$date_origin) | !is.null(input$date_maturity) |
                                                      !is.null(input$date_origin) | 
                                                      is.null(input$otherdate)){
                ndata<-tempdata()
                ndata[is.na(ndata)]<-0
            colnames(ndata)[colnames(ndata)==input$event_select]<-"default_event"
            colnames(ndata)[colnames(ndata)==input$amount_select]<-"loan_balance"
            colnames(ndata)[colnames(ndata)==input$date_origin]<-"origination_date"
            colnames(ndata)[colnames(ndata)==input$date_maturity]<-"maturity_date"
            colnames(ndata)[colnames(ndata)==input$assetid]<-"asset_id"
            ndata$origination_date<-lubridate::ymd(ndata$origination_date)
            ndata$maturity_date<-lubridate::ymd(ndata$maturity_date)
            
            ndata<-ifelse(!is.null(input$factorlist),
                          ndata[,input$factorlist]<-as.factor(ndata[,input$factorlist]),
                          ndata
            )
            
            if(!is.null(input$otherdate)){
                ndata[,input$otherdate]<-lubridate::ymd(ndata[,input$otherdate])  
            }
            
            return(ndata)
            }
    })
    
    # Inform if using sample of custom data
    output$info_ini <- renderText({
        if (input$upload == F) {
            "Using sample data"
        } else {
            "Using custom data"
        }
    })
    
    
    observeEvent(input$ok,{
        removeModal()
    })
    
    

    
    # FOr sidebar

    output$column_selector<- renderUI({
        fluidRow(
            column(width = 1),
            column(
                width = 11,
                br(),
                sliderInput(
                    "iv_select",
                    "Select min IV",
                    min = 0.01,
                    max = 0.99,
                    value = 0.5,
                    step = 0.01
                )
            )
        )
    })
    
    
    # Table to show in the data tab
    output$fulldata <- renderDataTable({
        fdata()
    },
    options = list(
        pageLength = 5,
        scrollX = TRUE,
        filter = list(position = 'top', clear = FALSE)
    ))
    
    # Creating dataframe for analysis
    
    dt <- reactive({
            bb<- fdata()
            rm_cols<-apply(fdata(),2,no_of_factors)
            rm_cols_list<-names(rm_cols[rm_cols>25])
            ifelse(
                identical(rm_cols_list, character(0)),
                bb<-var_filter(bb, y="default_event", iv_limit = 0.5, var_kp = "asset_id"),
                bb<-var_filter(bb, y="default_event", iv_limit = 0.5, var_kp = c("asset_id",rm_cols_list))
            )
            return(bb)
    })
    
    
    
    dt_list<-reactive({
        req(dt())
        zz = split_df(dt(), y="default_event", ratios = c(0.7, 0.3), seed = 30)
        label_list = lapply(zz, function(x) x$default_event)
        return(zz)
    })
    
    
    # woe binning ------
    
    bins<- reactive({

        req(dt())
        req(input$createbin)
        woebin(dt(), y="default_event", var_skip="asset_id")
    })
    
    output$bintable<- renderDataTable({
        validate(
            need(
                unique(fdata()$default_event) %in% c(0,1),
                message = "Please check the column selected"
            ),
            need(length(unique(fdata()$default_event))==2,
                 message = "Please check the column selected")
        )
        withProgress(message = "Generating good karma",
                     detail = "reading file", value = 0,
                     {
                         setProgress(value = 1, message = "Running 1 of 3")
                         ax<-bins()[[1]]
                         setProgress(value = 1, message = "Running 2 of 3")
                         for(i in 2:length(bins())){
                             ax<-rbind(ax,bins()[[i]])
                         }
                         setProgress(value = 1, message = "Running 3 of 3")
                     })

        ax
    },
    options = list(
        pageLength = 5,
        scrollX = TRUE,
        filter = list(position = 'top', clear = FALSE)
    )
    )
    

    
    # UI of plot button
    output$woeplot<- renderUI({
        validate(
            need(
                unique(fdata()$default_event) %in% c(0,1),
                message = "Please check the column selected"
            ),
            need(length(unique(fdata()$default_event))==2,
                 message = "Please check the column selected")
        )
        req(!is.null(bins()))
        fluidRow(
            column(
                width = 11,
                h3("Plot Bins"),
                selectizeInput("woeplotselect",
                               "Select Variable",
                               choices= names(bins()),
                               selected=NULL,
                               multiple=F),
                actionButton("plotbin","Plot"),
            )
        )
    })
    
    wplots<-reactive({
        req(!is.null(bins()))
        req(input$plotbin)
        woebin_plot(bins())
    })
    
    output$binplot<- renderPlot({
        validate(
            need(
                unique(fdata()$default_event) %in% c(0,1),
                message = "Please check the column selected"
            ),
            need(length(unique(fdata()$default_event))==2,
                 message = "Please check the column selected")
        )
        req(input$woeplotselect)
        withProgress(message = 'Plotting in progress',
                     detail = 'Sorry for being this slow...',
                     value = 0,
                     {
                         setProgress(value = 1, message = "Running 1 of 2")
                         pl<-wplots()[[input$woeplotselect]] 
                         setProgress(value = 1, message = "Running 2 of 2")
                     }
                     
                     )
        pl
        
    }
    )
    
    dt_woe_list<- reactive({
        lapply(dt_list(), function(x) woebin_ply(x, bins()))
    }) 
    
    woe_corr<- reactive({
        dt_woe_list()$train%>%
            dplyr::select(ends_with("_woe"))%>%
            cor(method = "pearson", use="complete.obs")
        
    }) 
    
    output$corplot<- renderPlot({
        validate(
            need(
                unique(fdata()$default_event) %in% c(0,1),
                message = "Please check the column selected"
            ),
            need(length(unique(fdata()$default_event))==2,
                 message = "Please check the column selected")
        )
        req(woe_corr())
        corrplot(woe_corr(), method = "number", type = "lower", diag = F)
    })
    
    selected_vars<- reactive({
        ad<-woe_corr()
        ad[upper.tri(ad)] <- 0
        diag(ad) <- 0
        ad <- ad[,!apply(ad,2,function(x) any(abs(x) > 0.7))]
        selec_var<-rownames(ad)
        selec_var
    })
    
    output$selvars<- renderText({
        validate(
            need(
                unique(fdata()$default_event) %in% c(0,1),
                message = "Please check the column selected"
            ),
            need(length(unique(fdata()$default_event))==2,
                 message = "Please check the column selected")
        )
        paste0("Selected Variabes are ", paste(selected_vars(), collapse = ", "))
    })
    
    # Model selection
    
    glm_pd<-reactive({
        req(selected_vars())
        form<-as.formula(paste0("default_event~", paste(selected_vars(), collapse = "+")))
        m1 = glm(form, family = binomial(), data = dt_woe_list()$train)
        m1
    })
    
    glm_step_pd<-reactive({
        req(glm_pd())
        m_step = step(glm_pd(), direction="both", trace = FALSE)
        m2 = eval(m_step$call)
        m2
    })    
    
    output$pdmodels<-renderPrint({
        validate(
            need(
                unique(fdata()$default_event) %in% c(0,1),
                message = "Please check the column selected"
            ),
            need(length(unique(fdata()$default_event))==2,
                 message = "Please check the column selected")
        )
        req(glm_step_pd())
        #summary(glm_pd())
        summary(glm_step_pd())
        
    })
    
    pdprob<-reactive({
        lapply(dt_woe_list(), function(x) predict(glm_step_pd(), x, type='response'))
    })
    
    
    output$pdglmroc<- renderPlot({
        validate(
            need(
                unique(fdata()$default_event) %in% c(0,1),
                message = "Please check the column selected"
            ),
            need(length(unique(fdata()$default_event))==2,
                 message = "Please check the column selected")
        )
        perf.auc(model = glm_step_pd(), dt_woe_list()$train, dt_woe_list()$test)
    })
    
    finalpd<- reactive({
        pred_list <- lapply(dt_woe_list(), function(x) predict(glm_step_pd(), x, type='response'))
        a<-dt_list()$train
        b<-dt_list()$test
        a$default_risk<-pred_list$train
        b$default_risk<-pred_list$test
        rbind(a,b)
    })
    
    output$finalpdtbl<-renderDataTable({
        finalpd()
    },
    options = list(
        pageLength = 5,
        scrollX = TRUE,
        filter = list(position = 'top', clear = FALSE)
    )
    )
    
    
    
}
# Run the app ----
shinyApp(ui, server)