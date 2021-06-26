header <- dashboardHeader(title = "F1")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem(
      "Lap Times",
      tabName = "lap_times",
      icon = icon("flag-checkered")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      "lap_times",
      h2("Track Lap Time Distributions"),
      column(
        width = 3,
        box(
          width = 12,
          title = "Circuit Filter",
          status = "primary",
          selectInput(
            "lap_time_circuit",
            "Circuit:",
            choices = default_circuit,
            selected = default_circuit,
            multiple = F
          ),
          sliderInput(
            "lap_time_season",
            "Season:",
            min = 1995,
            max = 2021,
            value = 2011,
            sep = ""
          )
        ),
        box(
          width = 12,
          status = "info",
          plotOutput("circuit_seasons_violen", height = "550px")
        )
      )
    )
  )
)

shinyUI(dashboardPage(
  header,
  sidebar,
  body,
  skin = "red"
))
