library(shinydashboard)
library(shiny)
library(DT)

dashboardPage(#skin = "black",
              dashboardHeader(title = "FAO Data Dashboard"),
              dashboardSidebar(
                sidebarMenu(
                  uiOutput("country"),
                  menuItem("Population", tabName = "population", icon = icon("group")),
                  menuItem("Economy", tabName = "economy", icon = icon("money")),
                  menuItem("Dietary energy supply", tabName = "des", icon = icon("cutlery")),
#                   uiOutput("group"),
#                   uiOutput("domain"),
#                   uiOutput("indOrAgg"),
#                   uiOutput("item"),
#                   uiOutput("time_frame"),
#                   uiOutput("element"),
#                   uiOutput("yearRange"),
                  menuItem("Source code", icon = icon("github"),
                           href = "https://github.com/unfaostatistics/faodash")
                )


              ),
              dashboardBody(
                tabItems(

                  # First tab content
                  tabItem(tabName = "population",
                    h2("Population"),

                    fluidRow(
                      infoBoxOutput("populationBox"),
                      valueBoxOutput("populationGrowth"),
                      valueBoxOutput("lifeExpectancy")
                            ),
                    h3("Rank"),
                    fluidRow(
                      box(plotOutput("populationBox_rank"),  width=4),
                      box(plotOutput("populationGrowth_rank"),  width=4),
                      box(plotOutput("lifeExpectancy_rank"),  width=4)
                                    )
                  ),
                  # Second tab content
                  tabItem(tabName = "economy",
                  h2("Economy"),
                  fluidRow(
                    infoBoxOutput("gdpPerCapita"),
                    valueBoxOutput("valueAddedWorker"),
                    valueBoxOutput("valueAddedAgriculture")
                          ),
                  h3("Rank"),
                  fluidRow(
                    box(plotOutput("gdpPerCapita_rank"),  width=4),
                    box(plotOutput("valueAddedWorker_rank"),  width=4),
                    box(plotOutput("valueAddedAgriculture_rank"),  width=4)
                                  )

                  ),
                  # Second tab content
                  tabItem(tabName = "des",
                  h2("Dietary energy supply"),
                  fluidRow(
                    infoBoxOutput("dietaryEnergySupply"),
                    valueBoxOutput("undernourishment"),
                    valueBoxOutput("rootsAndTubers")
                          )

                  )

                )
              )
)
