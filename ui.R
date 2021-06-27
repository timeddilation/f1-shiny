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

default_selected_circuit <- circuits_with_times[2]

body <- dashboardBody(
  tabItems(
    tabItem(
      "lap_times",
      h2(textOutput("lap_time_race_name")),
      column(
        width = 3,
        box(
          width = 12,
          title = "Circuit Filter",
          status = "primary",
          selectInput(
            "lap_time_circuit",
            "Circuit:",
            choices = circuits_with_times,
            selected = default_selected_circuit,
            multiple = F
          ),
          # TODO: change to race, not season. Some tracks have >1 race per season
          sliderTextInput(
            "lap_time_season",
            "Season:",
            choices = available_circuit_seasons(default_selected_circuit),
            selected = available_circuit_seasons(default_selected_circuit)[1],
            animate = T
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
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Race Lap Times",
            column(
              width = 2,
              h3("Fastest Lap"),
              tableOutput("lap_time_circuit_race_drivers_best")
            ),
            column(
              width = 10,
              plotOutput("lap_time_circuit_race_density", height = "200px"),
              hr(),
              plotOutput("lap_time_circtuit_race_driver_times_violen", height = "550")
            )
          ),
          tabPanel(
            "Driver Lap Times",
            fluidRow(
              hr(),
              column(
                width = 3,
                selectInput(
                  "lap_time_race_driver",
                  "Driver:",
                  choices = race_drivers(default_selected_circuit, available_circuit_seasons(default_selected_circuit)[1]),
                  selected = race_drivers(default_selected_circuit, available_circuit_seasons(default_selected_circuit)[1])[1],
                  multiple = F
                )
              ),
              # valueBoxOutput(
              #   "lap_time_drive_debug",
              #   width = 3
              # ),
              valueBoxOutput(
                "lap_time_driver_start_pos",
                width = 3
              ),
              valueBoxOutput(
                "lap_time_driver_finish_pos",
                width = 3
              ),
              valueBoxOutput(
                "lap_time_driver_fastest_lap_time",
                width = 3
              )
            ),
            fluidRow(
              plotlyOutput("lap_time_race_driver_times", height = "620px")
            )
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
