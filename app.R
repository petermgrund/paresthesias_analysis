## LOAD PACKAGES
library(tidyverse)
library(shiny)
library(rsconnect)

## CREATE UI
ui <- fluidPage(
  selectInput("subjectInput", "Choose a Subject:", choices = unique(data_cleaned$study_ID)),
  selectInput("visitInput", "Choose a Visit:", choices = unique(data_cleaned$visit)), # Choices set in server
  plotOutput("subjectPlot")
)

## CREATE SERVER
server <- function(input, output, session) {
  observeEvent(input$subjectInput, {
    visits <- unique(data_cleaned %>%
                       filter(study_ID == input$subjectInput) %>%
                       pull(visit))
    updateSelectInput(session, "visitInput", choices = visits)
  })
  
  output$subjectPlot <- renderPlot({
    selected_data <- data_cleaned %>%
      filter(study_ID == input$subjectInput, visit == input$visitInput) %>%
      arrange(as.numeric(order_type))
    
    ggplot(selected_data, aes(x = settings, y = mA, color = severity, shape = para_type)) +
      geom_point(size = 4, stroke = 0) +
      geom_text(aes(label = severity), size = 3, nudge_x = 0.2, nudge_y = 0.0, check_overlap = TRUE) +
      
      scale_color_gradient(low = "#D2EBE2", high = "#061F16", limits = c(0, 10)) +
      scale_y_continuous(limits = c(0, 5)) +  # Set y-axis limits
      labs(title = paste("Paresthesia thresholds for subject", input$subjectInput, "during", input$visitInput),
           x = "Settings (Ordered by type)",
           y = "mA (amplitude)",
           color = "Severity",
           shape = "Paresthesia type") +
      annotate("text", x = Inf, y = Inf, label = "Note: Gray points without values indicate missing severity data", hjust = 1.1, vjust = 1.1, size = 3) +
      theme_minimal() +
      scale_shape_manual(values = c("Transient" = 16, "Persistent" = 17))  
  })
}

# INITIALIZE
shinyApp(ui = ui, server = server)
