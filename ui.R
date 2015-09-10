library(shinydashboard)
library(shiny)
library(DT)

dashboardPage(skin = "black",
              


              dashboardHeader(title = ""),
              dashboardSidebar(
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                ),
                
      #           tags$head(tags$style(HTML('
      #   .main-sidebar .sidebar {
      #     background-color: #FFFFFF;
      #     color: #000000;
      #   }
      # '))),
                sidebarMenu(
#                  menuItem("", tabName = "production_and_trade"),
                  # menuItem("Trade", tabName = "economy", icon = icon("money")),
                  radioButtons("trade_production",label = "Pick one",choices = c("Production","Trade"), inline = TRUE),
                  uiOutput("indicator"),
#                  uiOutput("country"),
                 # uiOutput("group"),
                 # uiOutput("domain"),
                  #uiOutput("indOrAgg"),
                   # uiOutput("item"),
#                  uiOutput("time_frame"),
                  # uiOutput("element"),
                  uiOutput("yearYear"),
                  # sliderInput("year_range", "Select year range", min = 2000, max = 2010, value = c(2000,2010), step = 1,animate = TRUE),
                  
                  #menuItem("Nutrition", tabName = "des", icon = icon("cutlery")),

#                  

                  menuItem("Source code", icon = icon("github"),
                           href = "https://github.com/unfaostatistics/faodash")
                )


              ),
              dashboardBody(
                

                
                # tags$head(tags$style(HTML('
                #       .shiny-output-error { visibility: hidden; }
                #       .shiny-output-error:before { visibility: hidden; }
                #       '))),
                
                    tabItem(tabName = "production_and_trade",
                  #   #uiOutput("item"),
                     tags$h2(uiOutput("page_title")),
                     tags$hr(),

                     fluidRow(
                       valueBoxOutput("box_1_1",  width=6),
                       valueBoxOutput("box_1_2",  width=6)
                             ),
                  fluidRow(
                    infoBoxOutput("box_1_3",  width=6),
                    infoBoxOutput("box_1_4",  width=6)
                  ),
                    tags$h3(uiOutput("sub_title1")),
                     fluidRow(
                       box(plotOutput("box_2_1"),  width=6),
                       box(plotOutput("box_2_2"),  width=6)
                     ),
                      tags$h3(uiOutput("sub_title2")),
                     uiOutput("yearRange", width = 12),
                  fluidRow(
                    box(plotOutput("box_3_1"),  width=9),
                    box(tableOutput("box_3_2"),  width=3,title = "Top 11")
                  ),
                     
                  tags$hr()
                  #dataTableOutput("mytable")
                   )
              )
              
)
