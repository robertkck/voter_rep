
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(shinythemes)
# library(shinytest)

# Todo
## Transnational list in Treaty change
## Seat loss waffle chart
## Optimize increment
## Indicate which countries conform to strict deg prop in table at least
## Comment when upper limit in fix+prop does not kick in
## Comment when parabolic becomes linear
## Fix + Prop^t
## Square root
## Bring back proportional with min 3
## Spline
## Descriptions with what special cases are included in the scenarios
## Hemicycle should be a Hemicycle
## Also show political group below as in http://www.europarl.europa.eu/meps/en/hemicycle.html
## Improve download output (Variable names, order, scenario name, file name, ...)
## Compare button: Click to keep current distribution and continue exploring (also clear)
## Map
## Fix aspect ratio

# runApp(display.mode="showcase")

scenarios_list <- c(
  "Status quo",
  "Simple Brexit scenarios",
  "Minimising inequality within the Treaty", # "Allocations within the Treaty",
  "Treaty change"
)

brexit_list <- c(
  "Drop 73 MEPs",  "Equally distribute 73 MEPs",
  "Distribute 73 seats at current proportions",
  "Distribute 73 seats to increase representativeness"
  # "Distribute 73 seats to increase representativeness (no maximum)",
  # "Allocate seats to transnational list"
)

treaty_list <- c(
  # "Cambridge Compromise (total 751)",
  "Cambridge Compromise (total 639) - minimise Gini",
  "Cambridge Compromise (total 736) - minimise malapportionment"
)

# scenarios_list <- c("Status quo",
#                     "Brexit - Drop 73 MEPs",
#                     "Brexit - Equally distribute 73 MEPs",
#                     "Brexit - Distribute 73 seats at current proportions",
#                     "Brexit - Distribute 73 seats to increase representativeness",
#                     "Brexit - Distribute 73 seats to increase representativeness (no maximum)",
#                     "Brexit - Allocate seats to transnational list",
#                     "Proportional",
#                     "Degressively proportional - Fix-prop (Cambridge Compromise)",
#                     "Degressively proportional - Fix-prop (Cambridge Compromise) - optimal total",
#                     # "Degressively proportional - Parabolic",
#                     # "Degressively proportional - Spline",
#                     "My scenario"
# )

shinyUI(fluidPage(
  # includeCSS("http://www.w3schools.com/lib/w3.css"),
  theme = "bootstrap.css",
  # theme = shinytheme("paper"),
  titlePanel(h3("Representation in the European Parliament"), windowTitle = "Representation in the European Parliament"),
  fluidRow(
    column(9,
      sidebarLayout(
        sidebarPanel(
          radioButtons("scen", "Scenarios", scenarios_list),
          conditionalPanel(condition = "input.scen == 'Treaty change'",
            tags$hr(),
            selectInput("myscenario", "Select method",
                        c("Cambridge Compromise (Base + Prop)", "Parabolic", "Limited loss") # "tbd - Square Root"
            ),
            checkboxInput("uk", "UK remains a Member", value = FALSE),
            sliderInput("m", "Minimum number per State", 1, 10, 6),
            sliderInput("M", "Maximum number per State", 75, 140, 96),
            sliderInput("H", "Total number of Seats", 600, 800, 751)# ,
            # downloadButton('downloadData', 'Download')
          ),
          conditionalPanel(condition = "input.scen == 'Simple Brexit scenarios'",
            tags$hr(),
            selectInput("brexit", "Select method", brexit_list
            )
          ),
          conditionalPanel(condition = "input.scen == 'Minimising inequality within the Treaty'",
            tags$hr(),
            selectInput("treaty", "Select method", treaty_list
            )
          ),
          tags$hr(),
          actionButton("compare", "Compare", icon = icon("thumb-tack")),
          actionButton("clear", "Clear", icon = icon("undo"))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Representation", plotlyOutput("represent"), icon = icon("institution")) , # "The chart shows the allocation of seats in the 2014 - 2019 parliamentary cycle."), # ,
            tabPanel("Shares", plotlyOutput("shares", width="100%"), icon = icon("bar-chart")),
            tabPanel("Degressive Proportionality", plotlyOutput("degprop", width="100%"), icon = icon("line-chart")), # ,
            tabPanel("Table", DT::dataTableOutput("table"), icon = icon("table")),
            tabPanel(
              "Seats",
              # radioButtons("seats", "",
              #              c("Political group",
              #                "Member State")
              # ),
              plotlyOutput("hemi", width="100%"),
              icon = icon("pie-chart")
              ) # ,
            # tabPanel("Comparison", DT::dataTableOutput("comp"))
            # tabPanel("Summary", verbatimTextOutput("summary"))
          )
          # DT::dataTableOutput("comp")
        )
      )
    ),
    column(3,
           h3("Description of the scenario"),
           textOutput("text_scen"),
           textOutput("text_method"),
           textOutput("text_gini"),
           textOutput("text_specs"),
           tags$hr(),
           textOutput("text_desc"),
           downloadButton('downloadData', 'Download Data')
    )
  )
))
