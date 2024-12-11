library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

# Load or simulate your dataset
# Assuming your data is stored in a data frame called `Datos_top_3`

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Colombian Epidemiological Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("chart-bar")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table"))
    ),
    selectInput("year", "Select Year", choices = c("All", unique(Datos_top_3$Año)), selected = "All"),
    selectInput("state", "Select State", choices = c("All", unique(Datos_top_3$Departamento)), selected = "All"),
    selectInput("city", "Select City", choices = c("All", unique(Datos_top_3$Municipio)), selected = "All"),
    selectInput("diagnosis", "Select Type of Diagnostic", choices = c("All", unique(Datos_top_3$Diagnostico)), selected = "All"),
    sliderInput("age_range", "Select Age Range", 
                min = min(Datos_top_3$Edades, na.rm = TRUE),
                max = max(Datos_top_3$Edades, na.rm = TRUE),
                value = range(Datos_top_3$Edades, na.rm = TRUE)),
    selectInput("gender", "Select Gender", choices = c("All", levels(Datos_top_3$Sexo)), selected = "All")
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_diagnoses"),
          valueBoxOutput("unique_diagnoses"),
          valueBoxOutput("total_patients")
        )
      ),
      tabItem(
        tabName = "visualizations",
        fluidRow(
          box(title = "Diagnoses by State", status = "primary", solidHeader = TRUE, 
              plotOutput("bar_chart", height = "300px")),
          box(title = "Trends Over Time by Diagnostic Type", status = "primary", solidHeader = TRUE, 
              plotOutput("line_chart", height = "300px"))
        ),
        fluidRow(
          box(title = "Diagnoses Heatmap", status = "primary", solidHeader = TRUE, 
              plotOutput("heatmap", height = "300px")),
          box(title = "Gender Distribution by Diagnostic Type", status = "primary", solidHeader = TRUE, 
              plotOutput("pie_chart", height = "300px"))
        )
      ),
      tabItem(
        tabName = "data_table",
        fluidRow(
          box(title = "Filtered Data Table", status = "primary", solidHeader = TRUE, 
              dataTableOutput("data_table"))
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Filtered Data
  filtered_data <- reactive({
    data <- Datos_top_3
    if (input$year != "All") data <- data %>% filter(Año == input$year)
    if (input$state != "All") data <- data %>% filter(Departamento == input$state)
    if (input$city != "All") data <- data %>% filter(Municipio == input$city)
    if (input$diagnosis != "All") data <- data %>% filter(Diagnostico == input$diagnosis)
    if (input$gender != "All") data <- data %>% filter(Sexo == input$gender)
    data <- data %>% filter(Edades >= input$age_range[1], Edades <= input$age_range[2])
    return(data)
  })
  
  # Overview Boxes
  output$total_diagnoses <- renderValueBox({
    valueBox(nrow(filtered_data()), "Total Diagnoses", icon = icon("stethoscope"))
  })
  
  output$unique_diagnoses <- renderValueBox({
    valueBox(length(unique(filtered_data()$Diagnostico)), "Unique Diagnostic Types", icon = icon("notes-medical"))
  })
  
  output$total_patients <- renderValueBox({
    valueBox(sum(filtered_data()$cantidad_p, na.rm = TRUE), "Total Patients", icon = icon("user"))
  })
  
  # Plots
  output$bar_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = Departamento, fill = Diagnostico)) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(title = "Diagnoses by State", x = "State", y = "Count", fill = "Type of Diagnostic")
  })
  
  output$line_chart <- renderPlot({
    ggplot(filtered_data(), aes(x = Año, y = cantidad_p, color = Diagnostico)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Trends Over Time by Diagnostic Type", x = "Year", y = "Count", color = "Type of Diagnostic")
  })
  
  output$heatmap <- renderPlot({
    ggplot(filtered_data(), aes(x = Año, y = Edades, fill = cantidad_p)) +
      geom_tile() +
      theme_minimal() +
      labs(title = "Heatmap of Diagnoses", x = "Year", y = "Age", fill = "Number of Diagnoses")
  })
  
  output$pie_chart <- renderPlot({
    filtered_data() %>%
      count(Sexo, Diagnostico) %>%
      ggplot(aes(x = "", y = n, fill = Sexo)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = "Gender Distribution by Diagnostic Type", fill = "Gender")
  })
  
  # Data Table
  output$data_table <- renderDataTable({
    datatable(filtered_data())
  })
}

# Run the app
shinyApp(ui, server)
