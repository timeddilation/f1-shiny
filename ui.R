header <- dashboardHeader(title = "F1")

sidebar <- dashboardSidebar(
  #####
  sidebarMenu(
    id = "tabs",
    menuItem(
      "Race Summary",
      tabName = "race_sum",
      icon = icon("flag-checkered")
    ),
    menuItem(
      "Lap Times",
      tabName = "lap_times",
      icon = icon("stopwatch")
    )
  )
)
#####
default_selected_circuit <- circuits_with_times[2]

body <- dashboardBody(
  tags$head(
    tags$style(HTML('.content-wrapper { overflow: auto; }')) # allows overflow for scolling
  ),
  tabItems(
    tabItem(
      "race_sum",
      h3("Race Summary"),
      column(
        width = 4,
        box(
          width = 12,
          title = "Race Selection",
          status = "primary",
          sliderInput(
            "race_sum_year",
            "Year:",
            min = lap_times_tidy[, year] |> min(),
            max = lap_times_tidy[, year] |> max(),
            value = lap_times_tidy[, year] |> min(),
            step = 1,
            sep = "",
            animate = T
          ),
          selectInput(
            "race_sum_gp",
            "Grand Prix:",
            choices = gps_by_year(
              lap_times_tidy[, year] |> min()
            )[, fq_name],
            selected = gps_by_year(
              lap_times_tidy[, year] |> min()
            )[1, fq_name],
            multiple = F
          ),
          plotlyOutput(
            "race_sum_map",
            height = "300px"
          )
        )
      ),
      column(
        width = 8,
        h3("Overtakes"),
        plotlyOutput(
          "race_sum_lap_pos"
        )
      )
    ),
    tabItem(
      "lap_times",
      #####
      h3("Track Lap Time Distributions"),
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
          sliderTextInput(
            "lap_time_race",
            "Race:",
            choices = available_circuit_races(default_selected_circuit),
            selected = available_circuit_races(default_selected_circuit)[1],
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
        h3(textOutput("lap_time_race_name")),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Race Lap Times",
            column(
              width = 10,
              plotOutput("lap_time_circuit_race_density", height = "200px"),
              hr(),
              plotOutput("lap_time_circtuit_race_driver_times_violen", height = "550")
            ),
            column(
              width = 2,
              h3("Fastest Lap"),
              tableOutput("lap_time_circuit_race_drivers_best")
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
                  choices = race_drivers(default_selected_circuit, available_circuit_races(default_selected_circuit)[1]),
                  selected = race_drivers(default_selected_circuit, available_circuit_races(default_selected_circuit)[1])[1],
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
    #####
  )
)

shinyUI(dashboardPage(
  header,
  sidebar,
  body,
  skin = "red"
))
