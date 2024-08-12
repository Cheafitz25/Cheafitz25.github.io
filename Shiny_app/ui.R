# ui.R
library(shiny)
library(shinydashboard)
library(DT)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Track and Field App"),
    
    # Setting Tab Menu
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "about"),
        menuItem("Track and Field Insights", tabName = "insights"),
        menuItem("2024 Estimates", tabName = "estimates")
      )
    ),
    
    dashboardBody(     # About tab
      tabItems(
        tabItem(tabName = "about",
                fluidPage(
                  box(
                    title = "About",
                    textOutput("about_text")
                  )
                )
        ),
        
        tabItem(tabName = "insights",      # Insight tab
                fluidPage(
                  sidebarLayout(
                    sidebarPanel(
                      p("Select a gender and an event."),
                      selectInput("insights_sex", "Select Sex:", choices = c("Men", "Women")),
                      selectInput("insights_event", "Select Event:",
                                  choices = c("60", "200", "400", "600", "800", "1k", "Mile", "3k", "5k", "60 Hurdles",
                                              "High Jump", "Pole Vault", "Triple Jump", "Long Jump", "Shot Put", "Weight Throw", "Heptathlon", "Pentathlon")),
                      selectInput("insights_place", "Select Place:", choices = c("All Places" = "place", 1, 2, 3, 4, 5, 6, 7, 8))
                      
                    ),
                    mainPanel(
                      plotOutput("insights_trackFieldPlot"),
                      DTOutput("insights_dataTable")  # Add DataTable output
                    )
                  )
                )
        ),
        
        tabItem(tabName = "estimates",    # Estimate tab
                fluidPage(
                  sidebarLayout(
                    sidebarPanel(
                      p("Select a gender and an event."),
                      selectInput("estimates_sex", "Select Sex:", choices = c("Men", "Women")),
                      selectInput("estimates_event", "Select Event:",
                                  choices = c("60", "200", "400", "600", "800", "1k", "Mile", "3k", "5k", "60 Hurdles",
                                              "High Jump", "Pole Vault", "Triple Jump", "Long Jump", "Shot Put", "Weight Throw", "Heptathlon", "Pentathlon"))
                    ),
                    mainPanel(
                      textOutput("estimates_estimationText")  # Add Text Output for Estimation
                    )
                  )
                )
        )
      )
    )
  )
)

