getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

apptab<-tabPanel("App",
                 
                 # Show full data
                 fluidRow(
                   column(
                     width = 6,
                     fluidRow(
                       column(width=6, h3("Data")),
                       column(width =6 , br(),actionButton("uploadnew","Upload New Data"))
                     ),
                     br(),
                     br(),
                     dataTableOutput("up_data")
                   ),
                   column(
                     width = 6,
                     basicstatUI("nofloans")
                   )
                   
                 ),
                 br(),
                 br(),
                 fluidRow(
                   column(
                     width = 6,
                     h3("With Macroeconomy data"),
                     column(width = 6, selectInput("country","Country", selected = "United States", choices = countries$Country)),
                     column(width = 6, br(),actionButton("fetchimf","Fetch IMF data")),
                     withSpinner(dataTableOutput("fulldata"), type=7, color="black")
                   ),
                   column(
                     width = 6,
                     uiOutput("ac_button"),
                     uiOutput("expcalcu")
                   )
                 ),
                 br(),
                 br(),
                 fluidRow(
                   column(
                     width = 6,
                     uiOutput("scenario_opts")
                   ),
                   column(
                     width = 6,
                     uiOutput("credit_loss")
                   )
                 )
                 
)