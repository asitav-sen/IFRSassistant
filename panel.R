
countries <- read.csv("./data/countries.csv")

panel1<-
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
          tags$br(),
          tags$image(height=100, width=100,src="logo3.png")
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
      h3("With Macroeconomic data"),
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