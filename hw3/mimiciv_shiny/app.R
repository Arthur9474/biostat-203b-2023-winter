#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

icu_cohort <- readRDS("icu_cohort.rds")

icu_cohort_s <- icu_cohort %>%
  select(-subject_id, -stay_id, -intime, -hadm_id, 
         -first_careunit, -last_careunit,
         -outtime, -los, -anchor_age, -anchor_year,
         -anchor_year_group, -dod, -admittime, -dischtime,
         -deathtime, -admission_type, -admission_location, 
         -admission_location, -discharge_location, -edregtime,
         -edouttime, -hospital_expire_flag)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("ICU Cohort data Summary"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "var", 
                  label = "Variable of Interest:", 
                  choices = colnames(icu_cohort_s)),
      sliderInput("bins", 
                  "Number of Bins:", 
                  min = 5, 
                  max = 100, 
                  value = 30),
      sliderInput("n",
                  "Number of rows:",
                  value = 5,
                  min = 1,
                  max = 100)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", tableOutput("summary")),
                  tabPanel("Preview", tableOutput("table"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    hist <- ggplot(data = icu_cohort_s, aes_string(x = input$var)) +
      geom_histogram(aes(fill = thirty_day_mort),
                     bins = input$bins) +
      labs(x = input$var, y = "Frequency")
    
    bar <- ggplot(data = icu_cohort_s, aes_string(x = input$var)) +
      geom_bar(aes(fill = thirty_day_mort)) +
      labs(x = input$var, y = "Frequency")
    variable <- icu_cohort_s %>% select(input$var)
    
    if (is.numeric(pull(variable, input$var)) == TRUE) {
      hist
    } else {
      bar
    }
  })
  
  output$summary <- renderTable({
    variable <- icu_cohort_s %>% select(input$var)
    if (is.numeric(pull(variable, input$var)) == TRUE) {
      summary_table <- summary(variable)
      } else {
      summary_table <- table(variable)
      }
    summary_table
  })
  
  output$table <- renderTable({
    data_table <- icu_cohort_s %>%
      select(input$var) %>%
      head(n = input$n)
  })
}

shinyApp(ui = ui, server = server)
