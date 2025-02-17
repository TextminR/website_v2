library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(highcharter)
library(dplyr)

IP <- "localhost"

ui <- navbarPage("TextminR",
                 tabPanel("Themenrelevanz in der Literatur",
                          sidebarLayout(
                            sidebarPanel(
                              useShinyjs(),
                              textInput("search_word", "Gib ein Wort ein:", ""),
                              actionButton("search_btn", "Suchen"),
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
                                                value = FALSE)),
                              actionButton("plot_btn", "Plotte")
                            ),
                            mainPanel(
                              highchartOutput("plot")
                            )
                          )
                 )
)

server <- function(input, output, session){
  topics <- reactiveVal(data.frame(TopicNumber = integer()))
  selected_topics <- reactiveVal(integer())
  plot_data <- reactiveVal(NULL)
  
  observeEvent(input$search_btn, {
    req(input$search_word)
    
    url <- sprintf("http://%s:8000/topicForWord/lda?word=%s", IP, URLencode(input$search_word))
    res <- httr::GET(url)
    
    status <- httr::status_code(res)
    
    if (status == 200) {
      topic_data <- httr::content(res, as = "text", encoding = "UTF-8")
      topic_data <- fromJSON(topic_data)
      
      if (length(topic_data) > 0) {
        topic_numbers <- as.integer(names(topic_data))
        new_topics <- setdiff(topic_numbers, selected_topics())
        topics(data.frame(TopicNumber = new_topics))
      } else {
        topics(data.frame(TopicNumber = integer()))
      }
    } else {
      showNotification(paste("Fehler beim API-Aufruf! HTTP Status:", status), type = "error")
    }
  })
  
  output$topicsList <- renderUI({
    req(nrow(topics()) > 0)
    checkboxGroupInput(
      inputId = "selected_topics",
      label = NULL,
      choices = setNames(topics()$TopicNumber, paste("Topic", topics()$TopicNumber)),
      selected = NULL
    )
  })
  
  observeEvent(input$selected_topics, {
    req(input$selected_topics)
    
    selected <- selected_topics()
    new_selection <- as.integer(input$selected_topics)
    
    if (is.null(new_selection)) new_selection <- integer()
    
    updated_selection <- unique(c(selected, new_selection))
    
    if (length(updated_selection) > 4) {
      showNotification("Maximal 4 Topics erlaubt!", type = "error")
    } else {
      selected_topics(updated_selection)
    }
    shinyjs::enable(selector = "input[name='selected_topics']")
    
    if (length(updated_selection) >= 4) {
      disable_selector <- sprintf("input[name='selected_topics'][value!='%s']", 
                                  paste(updated_selection, collapse = "'][value!='"))
      shinyjs::disable(selector = disable_selector)
    }
  })
  
  observeEvent(input$plot_btn, {
    req(length(selected_topics()) > 0)
    
    startdate <- input$year_range[1]
    enddate <- input$year_range[2]
    absolute <- !input$absolute_relative
    
    query_params <- list(
      startdate = startdate,
      enddate = enddate,
      absolute = tolower(as.character(absolute))
    )
    
    for (t in selected_topics()) {
      query_params <- append(query_params, list(topics = t))
    }
    
    res <- httr::GET(
      url = sprintf("http://%s:8000/wordfrequency", IP),
      query = query_params
    )
    
    if (httr::status_code(res) == 200) {
      response_text <- httr::content(res, as = "text", encoding = "UTF-8")
      
      data <- tryCatch({
        fromJSON(response_text)
      }, error = function(e) {
        showNotification("Fehler beim Parsen der API-Daten!", type = "error")
        return(NULL)
      })
      
      req(!is.null(data))
      
      if (length(data) == 0) {
        showNotification("Keine Daten für die gewählten Topics gefunden.", type = "warning")
        return()
      }
      
      df_list <- lapply(names(data), function(topic) {
        topic_data <- data[[topic]]
        
        if (!is.list(topic_data) || length(topic_data) == 0) {
          return(NULL)
        }
        
        tibble(
          topic = as.integer(topic),
          date = as.Date(names(topic_data)),
          frequency = as.numeric(unlist(topic_data))
        )
      })
      
      df_list <- df_list[!sapply(df_list, is.null)]
      
      if (length(df_list) == 0) {
        showNotification("Keine validen Daten für die ausgewählten Topics gefunden.", type = "warning")
        return()
      }
      
      df <- bind_rows(df_list)
      
      plot_data(df)
    } else {
      showNotification("Fehler beim API-Aufruf!", type = "error")
    }
  })
  
  output$plot <- renderHighchart({
    df <- plot_data()
    req(df)
    
    if (nrow(df) == 0) {
      showNotification("Keine Daten für das Diagramm vorhanden.", type = "warning")
      return(NULL)
    }
    
    df$date <- as.POSIXct(df$date, origin = "1970-01-01", tz = "UTC")
    
    # Daten nach Topic gruppieren
    series_list <- split(df, df$topic)
    
    if (length(series_list) == 0) {
      showNotification("Fehler: Keine gültigen Daten für das Diagramm.", type = "error")
      return(NULL)
    }
    
    # Highchart-Objekt erstellen und direkt Serien hinzufügen
    hc <- highchart() %>%
      hc_chart(type = "line") %>%
      hc_chart(debug = TRUE) %>%
      hc_xAxis(
        type = "datetime",
        title = list(text = "Datum"),
        labels = list(format = "{value:%d.%m.%Y}"),
        minPadding = 0.05,
        maxPadding = 0.05
      ) %>%
      hc_yAxis(title = list(text = "Häufigkeit"), min = 0) %>%
      hc_title(text = "Topic-Häufigkeiten über die Zeit") %>%
      hc_tooltip(shared = TRUE, xDateFormat = "%d.%m.%Y") %>%
      hc_legend(enabled = TRUE)
    
    # Jede Serie direkt an das Highchart-Objekt anhängen
    for (topic_df in series_list) {
      hc <- hc %>%
        hc_add_series(
          name = paste("Topic", topic_df$topic[1]),
          type = "line",
          data = lapply(1:nrow(topic_df), function(i) {
            list(
              as.numeric(topic_df$date[i]) * 1000,
              topic_df$frequency[i]
            )
          })
        )
    }
    
    print(series_list)
    
    return(hc)
  })
  
  
}

shinyApp(ui = ui, server = server)

