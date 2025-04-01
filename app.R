library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(highcharter)
library(dplyr)
library(leaflet)
library(ggplot2)
library(shinyWidgets)

IP <- "localhost"

ui <- navbarPage("TextminR",
                 tabPanel("Startseite",
                          fluidRow(
                            column(12,div(style="text-align: center;",
                                          h2("TextminR"), 
                                          h3("Was bieten wir?"), 
                                          p("Unser Ziel ist die Entwicklung einer skalierbaren Modellversion, die sowohl literarische Werke als auch Newsartikel analysiert. Vortrainierte Modelle und gründliche Datenprüfungen ermöglichen praxisnahe Unterrichtsanwendungen ohne aufwändige Live-Berechnungen. Beispielaufgaben und Anleitungen für SchülerInnen und Lehrkräfte erleichtern den Einstieg in die Nutzung."),
                                          br(),
                                          div(style="display: flex; justify-content: center; align-items: center; width: 100%;", 
                                              switchInput("switch_button", "Aktiviere Funktion", value = FALSE, onLabel = "Literarische Daten", offLabel = "News-Media Daten")))),
                                          fluidRow(
                                            column(12, 
                                                   div(style="display: flex; justify-content: space-between; align-items: center; background-color: #f1f1f1; padding: 10px; width: 100%;",
                                                       h4("Titel der Leiste"),  
                                                       actionButton("popup_button", label = " ", icon = icon("arrow-right"), style = "background-color: transparent; border: none;")  
                                                   )
                                            )
                                          ),
                          ),
                 ),
                 tabPanel("Topic Ansicht",
                          mainPanel(
                            fluidRow(
                              column(12, 
                                div(style = "display: flex; align-items: center; gap: 10px; padding-bottom: 10px;",
                                  h3("Topic Streudiagramm", style = "margin-bottom: 0; line-height: 1.2;"),
                                  div(style = "display: flex; align-items: center;",
                                    actionButton("info_button_streudiagramm", label = NULL, icon = icon("info-circle"), class = "btn btn-info", 
                                                 style = "height: 32px; padding: 3px 12px; display: flex; align-items: center")
                                  )
                                )
                              )
                            ),
                            highchartOutput("interTopic"),
                            plotOutput("selectedTopic")
                          )
                          ),
                 tabPanel("Themenrelevanz in der Literatur",
                          sidebarLayout(
                            sidebarPanel(
                              useShinyjs(),
                              fluidRow(
                                column(12, 
                                       div(style="display: flex; gap: 15px;",
                                           h3("Themenrelevanz", style="margin-top: 0px;"), 
                                           actionButton("info_button_themenrelevanz", label = NULL, icon = icon("info-circle"), class = "btn btn-info", 
                                                        style = "height: 32px; padding: 3px 12px; display: flex; align-items: center")
                                       )
                                )
                              ),
                              textInput("search_word", "Gib Sie ein Wort ein und wählen 1-4 Topics aus:", ""),
                              actionButton("search_btn", "Suchen", 
                                           style = "margin-bottom: 10px;
                                           padding: 5px 15px; background-color: #007BFF; color: white;
                                           border: none; border-radius: 5px; cursor: pointer; display: block;
                                           width: 100%; font-size: 14px;"),
                              div(
                                style = "overflow-y: scroll; max-height: 300px;",
                                uiOutput("topicsList")),
                              actionButton("reset_btn", "Auswahl zurücksetzen", 
                                           style = "margin-bottom: 10px;
                                           padding: 5px 15px; background-color: #007BFF; color: white;
                                           border: none; border-radius: 5px; cursor: pointer; display: block;
                                           width: 100%; font-size: 14px;"),
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
                              actionButton("plot_btn", "Plotte",
                                           style = "margin-bottom: 10px;
                                           padding: 5px 15px; background-color: #007BFF; color: white;
                                           border: none; border-radius: 5px; cursor: pointer; display: block;
                                           width: 100%; font-size: 14px;")
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
                              fluidRow(
                                column(12, 
                                  div(style="display: flex; gap: 15px;",
                                    h3("Heatmap", style="margin-top: 0px;"), 
                                    actionButton("info_button_heatmap", label = NULL, icon = icon("info-circle"), class = "btn btn-info", 
                                                  style = "height: 32px; padding: 3px 12px; display: flex; align-items: center")
                                  )
                                )
                              ),
                              textInput("search_word2", "Gib ein Wort ein:", ""),
                              actionButton("search_btn2", "Suchen",
                                           style = "margin-bottom: 10px;
                                           padding: 5px 15px; background-color: #007BFF; color: white;
                                           border: none; border-radius: 5px; cursor: pointer; display: block;
                                           width: 100%; font-size: 14px;"),
                              "Wähle maximal 1 Topic aus",
                              div(
                                style = "overflow-y: scroll; max-height: 300px;",
                                uiOutput("topicsList2")),
                              actionButton("reset_btn2", "Auswahl zurücksetzen",
                                           style = "margin-bottom: 10px;
                                           padding: 5px 15px; background-color: #007BFF; color: white;
                                           border: none; border-radius: 5px; cursor: pointer; display: block;
                                           width: 100%; font-size: 14px;"),
                              div(sliderInput("year_range2",
                                              "Wähle eine Zeitspanne:",
                                              min = 1600, 
                                              max = 2025, 
                                              value = c(1800, 1920),
                                              step = 1,
                                              sep = "")),
                              actionButton("plot_btn2", "Plotte",
                                           style = "margin-bottom: 10px;
                                           padding: 5px 15px; background-color: #007BFF; color: white;
                                           border: none; border-radius: 5px; cursor: pointer; display: block;
                                           width: 100%; font-size: 14px;")
                            ),
                            mainPanel(
                              leafletOutput("heatmap")
                            )
                          )
                 ),
                 tabPanel("Specific Document",
                          sidebarLayout(
                            sidebarPanel(
                              fluidRow(
                                column(12, 
                                  div(style="display: flex; gap: 15px;",
                                   h3("Spezifisches Dokument", style="margin-top: 0px;"), 
                                   actionButton("info_button_document", label = NULL, icon = icon("info-circle"), class = "btn btn-info", 
                                                style = "height: 32px; padding: 3px 12px; display: flex; align-items: center")
                                   )
                                )
                              ),
                              div(
                                class="custom_sidebar",
                                textInput("such_string", "Suchfeld", placeholder = "Suchen Sie ein Werk")
                              ),
                              div(
                                style = "overflow-y: scroll; max-height: 400px;",
                                uiOutput("documents_list"),
                                div(
                                  style = "position: absolute; bottom: -10px; left: 10px; width: calc(100% - 20px);",
                                  actionButton(
                                    inputId = "plot_button",
                                    label = "Topics anzeigen",
                                    style = "margin-bottom: 10px;
                                    padding: 5px 15px; background-color: #007BFF; color: white;
                                    border: none; border-radius: 5px; cursor: pointer; display: block;
                                    width: 100%; font-size: 14px;"
                                  )
                                )
                              )
                            ),
                            mainPanel(
                              verbatimTextOutput("document_content")
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
  
  observeEvent(input$popup_button, {
    showModal(modalDialog(
      title= "Text Anzeige",
      p("Text 123"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Schließen")
      )
    ))
  })
  
  # API-Aufruf für erstes Tab
  observeEvent(input$search_btn, {
    req(input$search_word)
    
    url <- sprintf("http://%s:8000/topicmodels/word?model=lda&word=%s", IP, URLencode(input$search_word))
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
    
    url <- sprintf("http://%s:8000/topicmodels/word?model=lda&word=%s", IP, URLencode(input$search_word2))
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
    
    startdate <- as.Date(paste0(input$year_range[1], "-01-01"))
    enddate <- as.Date(paste0(input$year_range[2], "-01-01"))
    absolute <- !input$absolute_relative
    print(startdate)
    print(enddate)
    
    query_params <- list(
      startdate = startdate,
      enddate = enddate,
      absolute = tolower(as.character(absolute))
    )
    
    for (t in selected_topics()) {
      query_params <- append(query_params, list(topics = t))
    }
    
    res <- httr::GET(
      url = sprintf("http://%s:8000/topicmodels/frequency", IP),
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
  
  
  flatten_topics <- function(topics) {
    sapply(topics, function(x) paste(names(x), x, sep = ":", collapse = ", "))
  }
  
  # Plot-Funktion für zweiten Tab
  observeEvent(input$plot_btn2, {
    req(length(selected_topics2()) > 0)
    
    startdate <- as.Date(paste0(input$year_range2[1], "-01-01"))
    enddate <- as.Date(paste0(input$year_range2[2], "-01-01"))
    
    
    res <- httr::GET(
      url = sprintf("http://%s:8000/heatmap?startdate=%s&enddate=%s&topic=%s", IP, startdate, enddate, selected_topics2()),
    )
    
    print(res)
    
    
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
      
      print(names(data))
      print(class(data))
      print(length(data))
      
      
      safe_topics <- lapply(data$topics, function(x) {
        if (is.null(x) || length(x) == 0) return(NA) # Leere Topics mit NA füllen
        paste(names(x), x, sep = ":", collapse = ", ")
      })
      
      data_df <- data.frame(
        title = data$title,
        latitude = data$latitude,
        longitude = data$longitude,
        location = data$location,
        sentiment = data$sentiment,
        accuracy = data$topic_accuracy,
        id = data$id,
        probability_positive = data$probabilities.positive,
        probability_neutral = data$probabilities.neutral,
        probability_negative = data$probabilities.negative,
        stringsAsFactors = FALSE
      )
      print(data_df)
      
      plot_data2(data_df)
    }
  })
  
  observeEvent(input$plot_btn2, {
    output$heatmap <- renderLeaflet({
      df <- plot_data2()
      
      df$max_probability <- apply(df[, c("probability_positive", "probability_neutral", "probability_negative")], 1, max)
      
      df$max_category <- apply(df[, c("probability_positive", "probability_neutral", "probability_negative")], 1, function(row) {
        names(row)[which.max(row)]
      })
      
      print(df$max_probability)
      print(df$max_category)
      
      df$color <- ifelse(df$max_category == "probability_positive", ifelse(df$max_probability < 0.75, "#90EE90", "#006400"),
                         ifelse(df$max_category == "probability_negative", ifelse(df$max_probability < 0.75, "#ffcccb", "#8B0000"),
                                "#808080"))
      
      icons <- awesomeIcons(
        icon = 'info-sign',
        iconColor = 'white',
        markerColor = df$color
      )
      
      print(df$color)
      
      leaflet(df) %>%
        addTiles() %>%  
        addAwesomeMarkers(~longitude, ~latitude,icon = icons, popup = ~paste(title, "<br>Wert:", df$max_probability))%>%
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
            enabled = FALSE
          ),
          enableMouseTracking = FALSE
        )
    }
    return(hc)
  })
  
  df <- reactiveVal()
  document_content <- reactiveVal()
  content_id <- reactiveVal()
  
  observe({
    IP <- IP
    Pfad <- "/documents"
    url <- sprintf("http://%s:8000%s", IP, Pfad)
    
    res <- httr::GET(url)
    
    status <- httr::status_code(res)
    
    if (status == 200) {
      documents <- httr::content(res, as = "parsed")
      
      df(data.frame(
        id = names(documents),
        titel = unlist(documents, use.names = FALSE),
        stringsAsFactors = FALSE
      ))
    } else {
      error_message <- paste("Fehler beim API-Aufruf! HTTP Status:", status)
      
    }
  })
  
  filtered_documents <- reactive({
    req(df())
    if (!"titel" %in% colnames(df())) {
      stop("Die Spalte 'titel' wurde nicht gefunden.")
    }
    
    if (input$such_string != "") {
      df() %>%
        filter(grepl(input$such_string, titel, ignore.case = TRUE))
    } else {
      df()
    }
  })
  
  output$documents_list <- renderUI({
    req(filtered_documents())
    
    if (nrow(filtered_documents()) == 0) {
      tags$p("Keine Dokumente gefunden.")
    } else {
      tags$ul(
        lapply(1:nrow(filtered_documents()), function(i) {
          documents_name <- filtered_documents()$titel[i]
          document_id <- filtered_documents()$id[i]
          actionLink(
            inputId = paste0("document_", document_id), 
            label = documents_name, 
            style = "display: block; 
              margin-left: -38px;
              font-size: 16px;
              color: #000000;
              border-bottom: 1px solid #000000;
              margin-bottom: 10px;
              transition: color 0.3s ease;")
        })
      )
    }
  })
  
  observe({
    req(filtered_documents())
    
    lapply(1:nrow(filtered_documents()), function(i) {
      documents_id <- filtered_documents()$id[i]
      action_link_id <- paste0("document_", documents_id)
      
      observeEvent(input[[action_link_id]], {
        content_id(documents_id)
        
        content_id(documents_id)
        
        # Index des angeklickten Dokuments finden
        index <- which(filtered_documents()$id == documents_id)
        
        # Titel aus dem geklickten Dokument holen
        title <- if (length(index) > 0) filtered_documents()$titel[index] else "Unbekannter Titel"
        
        IP <- IP
        url <- sprintf("http://%s:8000/documents/info/%s", IP, documents_id)
        
        res <- httr::GET(url)
        
        if (httr::status_code(res) == 200) {
          data <- httr::content(res, as = "text", encoding = "UTF-8")
          
          if (nzchar(data)) {  # Stellt sicher, dass die Antwort nicht leer ist
            tryCatch({
              parsed_data <- fromJSON(data)
              
              # Author und Location ins DataFrame
              content_df <- data.frame(
                author = parsed_data$author,
                location = parsed_data$location,
                stringsAsFactors = FALSE
              )
              
              print(content_df)
              
              # Topics ins DataFrame
              topics_list <- parsed_data$topics
              
              # Named list in DataFrame umwandeln
              topics_df <- data.frame(
                topic = names(topics_list),
                probability = unlist(topics_list),
                stringsAsFactors = FALSE
              )
              
              print(topics_df)
              
              # Sentiment ins DataFrame
              sentiment_df <- data.frame(
                positiv = parsed_data$sentiment$positiv,
                neutral = parsed_data$sentiment$neutral,
                negativ = parsed_data$sentiment$negativ
              )
              
              print(sentiment_df)
              
              # Korrekte Speicherung der Ergebnisse
              document_content(list(
                title = title,
                author = content_df,
                topics = topics_df,
                sentiment = sentiment_df
              ))
              
              print(document_content())  # <- Klammern hinzugefügt, um den reaktiven Wert zu holen
              
            }, error = function(e) {
              showNotification(paste("Fehler beim Parsen der Daten:", e$message), type = "error")
            })
          } else {
            showNotification("Leere Antwort vom Server!", type = "error")
          }
        } else {
          showNotification(paste("Fehler beim Abrufen der Daten: HTTP", httr::status_code(res)), type = "error")
        }
      })
    })
  })
  
  
  output$document_content <- renderText({
  if (is.null(document_content())) {
    return("Noch kein Dokument ausgewählt oder kein Inhalt verfügbar.")
  } else {
    content_df <- document_content()$author
    title <- document_content()$title
    sentiment <- document_content()$sentiment
    
    positive_value <- as.numeric(sentiment$positiv)
    neutral_value <- as.numeric(sentiment$neutral)
    negative_value <- as.numeric(sentiment$negativ)
    
    print(positive_value)
    print(neutral_value)
    print(negative_value)
    
    if (any(is.na(c(positive_value, neutral_value, negative_value)))) {
      sentiment_output <- "Fehler: Ungültige Sentiment-Werte"
    } else {
      # Formatierte Ausgabe für Sentiment
      sentiment_output <- paste(
        "positiv:", round(positive_value, 2), 
        ", neutral:", round(neutral_value, 2), 
        ", negativ:", round(negative_value, 2)
      )
    }
    
    result <- paste(
      h2(title), 
      "\n \nAutor:", content_df$author, 
      "\nStandort:", content_df$location, 
      "\n\nSentiment:\n", sentiment_output
    )
    
    return(result)
  }
})

  
  observeEvent(input$plot_button, {
    showModal(
      modalDialog(
        title = "Topics",
        plotOutput("modal_plot"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Schließen")
      )
    )
  })
  
  output$modal_plot <- renderPlot({
    req(content_id())
    
    ggplot(document_content()$topics, aes(x = document_content()$topics$probability, y = reorder(document_content()$topics$topic, document_content()$topics$probability), fill = document_content()$topics$probability)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_gradient(low = "#03a1fc", high = "#1803fc") +
      labs(
        title = sprintf("Topics des ausgewählten Werkes"),
        x = "",
        y = ""
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "none"
      )
  })
  
  interTopicDF <- reactiveVal(NULL)
  
  output$interTopic <- renderHighchart({
    url <- sprintf("http://%s:8000/topicmodels/plot", IP)
    res <- httr::GET(url)
    
    
    content <- content(res, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(content)
    
    df <- as.data.frame(json_data)
    interTopicDF(df)
    
    highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_add_series(data = df, type = "bubble",
                    hcaes(x = x, y = y, size = size, name = topic),
                    dataLabels = list(enabled = TRUE, format = "{point.name}", showInLegend = FALSE)) %>%
      hc_xAxis(title = list(text = "X-Koordinate")) %>%
      hc_yAxis(title = list(text = "Y-Koordinate")) %>%
      hc_tooltip(pointFormat = "Topic: {point.name}") %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(series = list(
        point = list(
          events = list(
            click = JS("
              function() {
                Shiny.setInputValue('plot_click', {
                  name: this.name,
                  x: this.x,
                  y: this.y
                });
              }
            ")
          )
        )
      ))
  })
  
  observeEvent(input$info_button_streudiagramm, {
    showModal(modalDialog(
      title = "Information zum Streudiagramm",
      "Dieses Streudiagramm zeigt die verschiedenen Topics als Punkte in einem zweidimensionalen Raum. 
      Die Größe der Blasen repräsentiert die relative Bedeutung eines Topics. \n Klicken Sie auf ein Topic, um die relevantesten Wörter anzuzeigen.",
      easyClose = TRUE,
      footer = modalButton("Schließen")
    ))
  })
  
  observeEvent(input$info_button_themenrelevanz, {
    showModal(modalDialog(
      title = "Information zum Streudiagramm",
      "Dieses Streudiagramm zeigt die verschiedenen Topics als Punkte in einem zweidimensionalen Raum. 
      Die Größe der Blasen repräsentiert die relative Bedeutung eines Topics. \n Klicken Sie auf ein Topic, um die relevantesten Wörter anzuzeigen.",
      easyClose = TRUE,
      footer = modalButton("Schließen")
    ))
  })
  
  observeEvent(input$info_button_heatmap, {
    showModal(modalDialog(
      title = "Information zum Streudiagramm",
      "Dieses Streudiagramm zeigt die verschiedenen Topics als Punkte in einem zweidimensionalen Raum. 
      Die Größe der Blasen repräsentiert die relative Bedeutung eines Topics. \n Klicken Sie auf ein Topic, um die relevantesten Wörter anzuzeigen.",
      easyClose = TRUE,
      footer = modalButton("Schließen")
    ))
  })
  
  observeEvent(input$info_button_document, {
    showModal(modalDialog(
      title = "Information zum Streudiagramm",
      "Dieses Streudiagramm zeigt die verschiedenen Topics als Punkte in einem zweidimensionalen Raum. 
      Die Größe der Blasen repräsentiert die relative Bedeutung eines Topics. \n Klicken Sie auf ein Topic, um die relevantesten Wörter anzuzeigen.",
      easyClose = TRUE,
      footer = modalButton("Schließen")
    ))
  })
  
  observeEvent(input$plot_click, {
    req(interTopicDF())
    topic <- input$plot_click$name
    
    IP <- "127.0.0.1"  # oder deine gewünschte IP
    url <- sprintf("http://%s:8000/topicmodels/topics/%d?model=lda", IP, topic)
    
    res <- GET(url)
    
    if (status_code(res) == 200) {
      dependencies_data <- content(res, as = "parsed")
      dependencies_df <- data.frame(
        name = names(dependencies_data),
        wert = unlist(dependencies_data, use.names = FALSE),
        stringsAsFactors = FALSE
      )
      
      output$selectedTopic <- renderPlot({
        ggplot(dependencies_df, aes(x = wert, y = reorder(name, wert), fill = wert)) +
          geom_bar(stat = "identity", color = "black") +
          scale_fill_gradient(low = "#03a1fc", high = "#1803fc") +
          labs(
            title = sprintf("Topic %d: Relevanz der Wörter", topic),
            x = "Relevanz",
            y = "Wörter"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            legend.position = "none"
          )
      })
    } else {
      output$selectedTopic <- renderText({
        paste("Error: Failed to load dependencies for topic number:", topic)
      })
    }
  })
}

shinyApp(ui = ui, server = server)
