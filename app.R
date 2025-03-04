library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(highcharter)
library(dplyr)
library(leaflet)

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
                              actionButton("reset_btn", "Auswahl zurücksetzen"),
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
                 ),
                 tabPanel("Geographische Daten",
                          sidebarLayout(
                            sidebarPanel(
                              useShinyjs(),
                              textInput("search_word2", "Gib ein Wort ein:", ""),
                              actionButton("search_btn2", "Suchen"),
                              "Wähle maximal 1 Topic aus",
                              div(
                                style = "overflow-y: scroll; max-height: 300px;",
                                uiOutput("topicsList2")),
                              actionButton("reset_btn2", "Auswahl zurücksetzen"),
                              div(sliderInput("year_range2",
                                              "Wähle eine Zeitspanne:",
                                              min = 1600, 
                                              max = 2025, 
                                              value = c(1800, 1920),
                                              step = 1,
                                              sep = "")),
                              actionButton("plot_btn2", "Plotte")
                            ),
                            mainPanel(
                              leafletOutput("heatmap")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  topics <- reactiveVal(data.frame(TopicNumber = integer()))
  topics2 <- reactiveVal(data.frame(TopicNumber = integer()))
  selected_topics <- reactiveVal(integer())
  selected_topics2 <- reactiveVal(integer())
  plot_data <- reactiveVal(NULL)
  plot_data2 <- reactiveVal(NULL)
  
  # API-Aufruf für erstes Tab
  observeEvent(input$search_btn, {
    req(input$search_word)
    
    url <- sprintf("http://%s:8000/topicForWord/lda?word=%s", IP, URLencode(input$search_word))
    res <- httr::GET(url)
    
    if (httr::status_code(res) == 200) {
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
  
  # API-Aufruf für zweites Tab
  observeEvent(input$search_btn2, {
    req(input$search_word2)
    
    previous_selection <- selected_topics2()
    
    url <- sprintf("http://%s:8000/topicForWord/lda?word=%s", IP, URLencode(input$search_word2))
    res <- httr::GET(url)
    
    if (httr::status_code(res) == 200) {
      topic_data <- httr::content(res, as = "text", encoding = "UTF-8")
      topic_data <- fromJSON(topic_data)
      
      if (length(topic_data) > 0) {
        topic_numbers <- as.integer(names(topic_data))
        new_topics <- setdiff(topic_numbers, previous_selection)
        topics2(data.frame(TopicNumber = new_topics))
      } else {
        topics2(data.frame(TopicNumber = integer()))
      }
    } else {
      showNotification(paste("Fehler beim API-Aufruf! HTTP Status:", status), type = "error")
    }
  })
  
  # Topic-Checkboxen für erstes Tab
  output$topicsList <- renderUI({
    req(nrow(topics()) > 0)
    checkboxGroupInput(
      inputId = "selected_topics",
      label = NULL,
      choices = setNames(topics()$TopicNumber, paste("Topic", topics()$TopicNumber)),
      selected = NULL
    )
  })
  
  # Topic-Checkboxen für zweites Tab (max. 1 Auswahl)
  output$topicsList2 <- renderUI({
    req(nrow(topics2()) > 0)
    checkboxGroupInput(
      inputId = "selected_topics2",
      label = NULL,
      choices = setNames(topics2()$TopicNumber, paste("Topic", topics2()$TopicNumber)),
      selected = NULL
    )
  })
  
  # Maximal 4 Topics im ersten Tab
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
  
  # Maximal 1 Topic im zweiten Tab
  observeEvent(input$selected_topics2, {
    req(input$selected_topics2)
    selected <- selected_topics2()
    new_selection <- as.integer(input$selected_topics2)
    
    if (is.null(new_selection)) new_selection <- integer()
    
    updated_selection <- unique(c(selected, new_selection))
    
    if (length(updated_selection) > 2) {
      showNotification("Nur 1 Topic gleichzeitig erlaubt!", type = "error")
    } else {
      selected_topics2(updated_selection)
    }
    shinyjs::enable(selector = "input[name='selected_topics2']")
    if (length(updated_selection) >= 1) {
      disable_selector <- sprintf("input[name='selected_topics2'][value!='%s']", 
                                  paste(updated_selection, collapse = "'][value!='"))
      shinyjs::disable(selector = disable_selector)
    }
  })
  
  # Reset-Button im ersten Tab
  observeEvent(input$reset_btn, {
    selected_topics(integer())
    shinyjs::enable(selector = "input[name='selected_topics']")
    updateCheckboxGroupInput(session, "selected_topics", selected = character(0))
  })
  
  # Reset-Button im zweiten Tab
  observeEvent(input$reset_btn2, {
    selected_topics2(integer())
    shinyjs::enable(selector = "input[name='selected_topics2']")
    updateCheckboxGroupInput(session, "selected_topics2", selected = character(0))
  })
  
  # Plot-Funktion für das erste Tab
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
  

  
  # Plot-Funktion für zweiten Tab
  observeEvent(input$plot_btn2, {
    req(length(selected_topics2()) > 0)
    
    startdate <- as.Date(paste0(input$year_range2[1], "-01-01"))
    enddate <- as.Date(paste0(input$year_range2[2], "-01-01"))
    print(startdate)
    
    
    res <- httr::GET(
      url = sprintf("http://%s:8000/heatmap?startdate=%s&enddate=%s&topic=%s", IP, startdate, enddate, selected_topics2()),
    )
    
    
    if (httr::status_code(res) == 200) {
      response_text <- httr::content(res, as = "text", encoding = "UTF-8")
      
      data <- tryCatch({
        fromJSON(response_text, flatten = TRUE)
      }, error = function(e) {
        showNotification("Fehler beim Parsen der API-Daten!", type = "error")
        return(NULL)
      })
      
      req(!is.null(data))
      
      if (!is.data.frame(data)) {
        data <- as.data.frame(data)
      }
      
      if (nrow(data) == 0) {
        showNotification("Keine Daten für die gewählten Topics gefunden.", type = "warning")
        return()
      }
      
      data_df <- data.frame(
        title = data$title,
        longitude = data$longitude,
        latitude = data$latitude,
        sentiment = data$sentiment,
        topics = data$topics,
        stringsAsFactors = FALSE
      )
      
      plot_data2(data_df)
    }
  })
  
  observeEvent(input$plot_btn2, {
    output$heatmap <- renderLeaflet({
      df <- plot_data2()
      
      
      df$color <- ifelse(df$sentiment > 0.5, "green",
                         ifelse(df$sentiment < 0.5, "red", "gray"))
      
      icons <- awesomeIcons(
        icon = 'info-sign',
        iconColor = 'white',
        markerColor = df$color
      )
      
      leaflet(df) %>%
        addTiles() %>%  
        addAwesomeMarkers(~longitude, ~latitude,icon = icons, popup = ~paste(title, "<br>Wert:", sentiment))%>%
        addProviderTiles(
          "OpenStreetMap",
          group = "OpenStreetMap"
        ) %>%
        addProviderTiles(
          "Esri.WorldImagery",
          group = "Esri.WorldImagery"
        ) %>%
        addLayersControl(
          baseGroups = c(
            "OpenStreetMap", "Esri.WorldImagery"
          ),
          position = "bottomleft"
        )
    })
  })
  
  
  output$plot <- renderHighchart({
    df <- plot_data()
    req(df)
    
    if (nrow(df) == 0) {
      showNotification("Keine Daten für das Diagramm vorhanden.", type = "warning")
      return(NULL)
    }
    
    df$date <- as.POSIXct(df$date, origin = "1970-01-01", tz = "UTC")
    
    series_list <- split(df, df$topic)
    
    if (length(series_list) == 0) {
      showNotification("Fehler: Keine gültigen Daten für das Diagramm.", type = "error")
      return(NULL)
    }
    
    
    topic_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
    zaehler = 0
    hc <- highchart() %>%
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
    
    adaptive_span <- function(n) {
      max(0.3, min(1, 10 / n)) # skaliert zwischen 0.3 und 1
    }
    
    for (topic_df in series_list) {
      loess_fit <- loess(frequency ~ as.numeric(date), data = topic_df, span = adaptive_span(nrow(topic_df)))
      
      smoothed_data <- data.frame(
        date = topic_df$date,
        frequency = predict(loess_fit)
      )
      
      hc <- hc %>%
        hc_add_series(
          name = paste("Topic", topic_df$topic[1]),
          type = "scatter",
          color = "rgba(100,100,100,0.5)",
          data = lapply(1:nrow(topic_df), function(i) {
            list(
              as.numeric(topic_df$date[i]) * 1000,
              topic_df$frequency[i]
            )
          })
        )
      hc <- hc %>%
        hc_add_series(
          name = paste("Smooth Curve: Topic", topic_df$topic[1]),
          type = "spline",
          data = lapply(1:nrow(smoothed_data), function(i) {
            list(
              as.numeric(smoothed_data$date[i]) * 1000,
              smoothed_data$frequency[i]
            )
          }),
          lineWidth = 3,
          marker = list(
            enabled = FALSE  # Keine Marker für die Spline-Linie
          ),
          enableMouseTracking = FALSE
        )
    }
    return(hc)
  })
}

shinyApp(ui = ui, server = server)
