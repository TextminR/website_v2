library(shiny)
library(shinyjs)

IP <- "localhost"

ui <- navbarPage("TextminR",
  tabPanel("Themenrelevanz in der Literatur",
    sidebarLayout(
      sidebarPanel(
        useShinyjs(),
        "Wähle 1-4 Topics aus",
        div(
            style = "overflow-y: scroll; max-height: 300px;",
            uiOutput("topicsList")),
        div(sliderInput("year_range",
                        "Wähle eine Zeitspanne:",
                        min = 1600, 
                        max = 2025, 
                        value = c(1800, 1920),
                        step = 1,
                        sep = ""),
            checkboxInput("absolute_relative",
                          "Relative Zahlen verwenden",
                          value = FALSE))
      ),
      mainPanel(
        textOutput("selected_topics")
      )
    )
           )
                 )

server <- function(input, output){
  topics <- reactiveVal()
  
  observe({
    IP <- IP
    Pfad <- "/topiccount/lda"
    url <- sprintf("http://%s:8000%s", IP, Pfad)
    
    res <- httr::GET(url)
    
    status <- httr::status_code(res)
    
    if (status == 200) {
      num_topics <- as.numeric(httr::content(res, as = "text"))
      if (!is.na(num_topics) && num_topics > 0) {
        topics(data.frame(
          TopicName = paste0("Topic ", 0:(num_topics-1)),
          TopicNumber = 0:(num_topics-1)
        ))
      } else {
        topics(NULL)
      }
    } else {
      error_message <- paste("Fehler beim API-Aufruf! HTTP Status:", status)
      topics(error_message)
      print(error_message)
    }
  })
  
  output$topicsList <- renderUI({
    req(topics())
    checkboxGroupInput(
      inputId = "selected_topics",
      label = NULL, # Keine extra Beschriftung, da oben bereits ein Titel steht
      choices = setNames(topics()$TopicNumber, topics()$TopicName),
      selected = NULL
    )
  })
  
  observe({
    req(input$selected_topics)
    selected_count <- length(input$selected_topics)
    
    # Wenn 4 ausgewählt sind -> Rest deaktivieren
    if (selected_count >= 4) {
      shinyjs::disable(selector = sprintf("input[name='selected_topics'][value!='%s']", paste(input$selected_topics, collapse = "'][value!='")))
    } else {
      shinyjs::enable(selector = "input[name='selected_topics']")
    }
  })
  
  output$selected_topics <- renderText({
    selected <- input$selected_topics
    if (length(selected) < 1 || length(selected) > 4) {
      return("Bitte wähle zwischen 1 und 4 Topics aus.")
    }
    paste("Ausgewählte Topics:", paste(selected, collapse = ", "))
  })
  
}
shinyApp(ui = ui, server = server)

