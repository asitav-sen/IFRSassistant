panel1<-tabPanel("Data",
         fluidRow(
           column(
             width = 6,
             column(
               width = 6,
               h3("Full Data")
             ),
             column(
               width = 6,
               br(),
               actionButton("upload", "Upload new data")
             ),
             # Show data
             dataTableOutput("fulldata")
           ),
           column(
             width = 6,
             fluidRow(
               column(
                 width = 6,
                 h3("Bins table")),
               column(
                 width = 6,
                 br(),
                 actionButton("createbin","Create Bins (WoE)")   
               )
             ),
             dataTableOutput("bintable")
           )
         ),
         fluidRow(
           column(
             width = 6,
             h3("Correlation between the binned variables")
           ),
           column(
             width = 6,
             uiOutput("woeplot")
           )
         ),
         fluidRow(
           column(
             width = 6,
             plotOutput("corplot")
           ),
           column(
             width = 6,
             plotOutput("binplot")
           )
         ),
         fluidRow(
           p("GLM model selected using stepwise"),
           verbatimTextOutput("pdmodels")
         ),
         fluidRow(
           h3("ROC Curve"),
           plotOutput("pdglmroc"),
           h3("Table with"),
           DTOutput("finalpdtbl")
         )
)


modal1<-             
  fluidRow(
  column(width = 1),
  column(
    width = 11,
    fluidRow(
      h5("Please provide file properties"),
      checkboxInput("header", "My files have header", TRUE),
      fluidRow(
        column(
          width = 6,
          # Input: Select separator ----
          radioButtons(
            "sep",
            "Data separated by",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = ","
          )
        ),
        column(
          width = 6,
          # Input: Select quotes ----
          radioButtons(
            "quote",
            "Empty denoted by",
            choices = c(
              None = "",
              "Double Quote" = '"',
              "Single Quote" = "'"
            ),
            selected = '"'
          )
        )),
      fluidRow(
        column(
          width = 6,
          fileInput(
            "newdata",
            "Upload customer data CSV",
            multiple = F,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
        ),
        column(
          width = 6,
          actionButton("confirm","Confirm")
        )
      )
    )
  ),
  fluidRow(
    column(width = 1),
    column(
      width = 11,
      fluidRow(
        column(
          width = 6,
          varSelectInput(
            "event_select",
            "Select Event Column",
            NULL,
            selected = NULL,
            multiple = FALSE,
            selectize = TRUE
          ),
          varSelectInput(
            "amount_select",
            "Select Loan Balance Column",
            NULL,
            selected = NULL,
            multiple = F,
            selectize = TRUE
          ),
          varSelectInput(
            "assetid",
            "Select asset identifier (unique) column",
            NULL,
            selected = NULL,
            multiple = F,
            selectize = TRUE
          ),
          varSelectInput(
            "factorlist",
            "Select factor column",
            NULL,
            selected = NULL,
            multiple = F,
            selectize = TRUE
          )
          
        ),
        column(
          width = 6,
          varSelectInput(
            "date_origin",
            "Select origin date column",
            NULL,
            selected = NULL,
            multiple = F,
            selectize = TRUE
          ),
          varSelectInput(
            "date_maturity",
            "Select maturity data column",
            NULL,
            selected = NULL,
            multiple = F,
            selectize = TRUE
          ),
          varSelectInput(
            "otherdate",
            "Other date columns",
            NULL,
            selected = NULL,
            multiple = F,
            selectize = TRUE
          )
        )
      ),
      fluidRow(
        column(
          width = 11,
          fluidRow(
            # radioButtons(
            #   "dateformat",
            #   "Date format",
            #   choices = c("dmy","ymd")
            #   )
          )
        )
      )
    )
  )
)

