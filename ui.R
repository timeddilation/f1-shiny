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
          status = "warning",
          plotOutput("lap_time_circuit_seasons_violen", height = "550px")
        )
      ),
      column(
        width = 9,
        box(
          width = 12,
          title = "Race Lap Time Decomposition",
          status = "danger",
          column(
            width = 2,
            h3("Fastest Lap"),
            tableOutput("lap_time_circuit_race_drivers_best")
          ),
          column(
            width = 10,
            plotOutput("lap_time_circuit_race_density", height = "200px"),
            plotOutput("lap_time_circtuit_race_driver_times_violen", height = "550")
          )
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
