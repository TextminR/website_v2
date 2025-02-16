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
  
  # --- Suche Muruk ---
  observeEvent(input$search_btn, {
    req(input$search_word)
    
    url <- sprintf("http://%s:8000/topicForWord/lda?word=%s", IP, URLencode(input$search_word))
    res <- httr::GET(url)
    
    status <- httr::status_code(res)
    
    if (status == 200) {
      topic_data <- httr::content(res, as = "text", encoding = "UTF-8")
      topic_data <- fromJSON(topic_data)
      
      if (length(topic_data) > 0) {
        topic_numbers <- as.integer(names(topic_data))  # Die Keys (Topic-Nummern) extrahieren
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
    
    # Falls new_selection NULL ist, setze eine leere Liste
    if (is.null(new_selection)) new_selection <- integer()
    
    updated_selection <- unique(c(selected, new_selection))
    
    # Begrenze auf maximal 4 Topics
    if (length(updated_selection) > 4) {
      showNotification("Maximal 4 Topics erlaubt!", type = "error")
    } else {
      selected_topics(updated_selection)
    }
    shinyjs::enable(selector = "input[name='selected_topics']")
    
    # Falls 4 gewählt sind, restliche Checkboxen deaktivieren
    if (length(updated_selection) >= 4) {
      disable_selector <- sprintf("input[name='selected_topics'][value!='%s']", 
                                  paste(updated_selection, collapse = "'][value!='"))
      shinyjs::disable(selector = disable_selector)
    }
  })
  
  # --- Große Call Muruk ---
  observeEvent(input$plot_btn, {
    req(length(selected_topics()) > 0)
    
    startdate <- input$year_range[1]
    enddate <- input$year_range[2]
    absolute <- !input$absolute_relative  # Checkbox umkehren (TRUE = absolute, FALSE = relative)
    
    # Query-Parameter vorbereiten
    query_params <- list(
      startdate = startdate,
      enddate = enddate,
      absolute = tolower(as.character(absolute))
    )
    
    # Mehrfach-Topics in die Query einfügen
    for (t in selected_topics()) {
      query_params <- append(query_params, list(topics = t))
    }
    
    # API-Call mit korrekten Query-Parametern
    res <- httr::GET(
      url = sprintf("http://%s:8000/wordfrequency", IP),
      query = query_params
    )
    print(res)
    
    if (httr::status_code(res) == 200) {
      response_text <- httr::content(res, as = "text", encoding = "UTF-8")
      
      # JSON-Daten sicher parsen
      data <- tryCatch({
        fromJSON(response_text)
      }, error = function(e) {
        showNotification("Fehler beim Parsen der API-Daten!", type = "error")
        return(NULL)
      })
      
      # Falls das JSON-Parsing fehlschlägt, abbrechen
      req(!is.null(data))
      
      # Falls die API eine leere Liste zurückgibt
      if (length(data) == 0) {
        showNotification("Keine Daten für die gewählten Topics gefunden.", type = "warning")
        return()
      }
      
      # Umwandlung der API-Daten in DataFrame für Highcharter
      df_list <- lapply(names(data), function(topic) {
        topic_data <- data[[topic]]
        
        # Fehlerprüfung: Falls topic_data nicht die erwartete Struktur hat, abbrechen
        if (!is.list(topic_data) || length(topic_data) == 0) {
          return(NULL)
        }
        
        tibble(
          topic = as.integer(topic),
          date = as.Date(names(topic_data)),  # Falls das nicht klappt -> Check API-Datenstruktur!
          frequency = as.numeric(unlist(topic_data))
        )
      })
      print(df_list)
      
      # NULL-Werte (falls ein Topic keine Daten hatte) entfernen
      df_list <- df_list[!sapply(df_list, is.null)]
      
      # Falls alle Topics leer sind, abbrechen
      if (length(df_list) == 0) {
        showNotification("Keine validen Daten für die ausgewählten Topics gefunden.", type = "warning")
        return()
      }
      
      # Kombiniere die Daten
      df <- bind_rows(df_list)
      
      # Speichern der Daten für das Diagramm
      plot_data(df)
    } else {
      showNotification("Fehler beim API-Aufruf!", type = "error")
    }
  })
  
  output$plot <- renderHighchart({
    df <- plot_data()
    req(df)
    
    print("Debugging: Inhalt von df")
    print(df)
    
    # Sicherstellen, dass df nicht leer ist
    if (nrow(df) == 0) {
      showNotification("Keine Daten für das Diagramm vorhanden.", type = "warning")
      return(NULL)
    }
    
    # Datum sicher in POSIXct umwandeln
    df <- df %>%
      mutate(date = as.POSIXct(date, format = "%Y-%m-%d", tz = "UTC"))
    
    print("Debugging: Struktur von df nach POSIXct-Umwandlung")
    print(df)
    
    # Konvertiere df in eine Struktur für Highcharter
    series_list <- lapply(split(df, df$topic), function(topic_df) {
      list(
        name = paste("Topic", topic_df$topic[1]),  # Linienname
        type = "line",  # Stelle sicher, dass es eine Linie wird
        data = lapply(1:nrow(topic_df), function(i) {
          list(
            as.numeric(topic_df$date[i])*1000,  # Unix-Timestamp für X-Achse
            topic_df$frequency[i]  # Häufigkeit für Y-Achse
          )
        })
      )
    })
    
    print("Debugging: Kontrolliere X/Y-Werte der Datenpunkte")
    print(lapply(series_list, function(s) s$data))
    
    # Falls series_list leer ist, Abbruch
    if (length(series_list) == 0) {
      showNotification("Fehler: Keine gültigen Daten für das Diagramm.", type = "error")
      return(NULL)
    }
    
    # Highchart erstellen mit formatierten Achsen
    hc <- highchart() %>%
      hc_chart(type = "line") %>%
      hc_chart(debug = TRUE) %>%
      hc_xAxis(
        type = "datetime",  # ✅ Korrektur des Tippfehlers
        title = list(text = "Datum"),
        labels = list(format = "{value:%d.%m.%Y}"),
        minPadding = 0.05,  # ✅ Verhindert abgeschnittene Punkte
        maxPadding = 0.05
      ) %>%
      hc_yAxis(title = list(text = "Häufigkeit"), min = 0) %>%
      hc_title(text = "Topic-Häufigkeiten über die Zeit") %>%
      hc_tooltip(shared = TRUE, xDateFormat = "%d.%m.%Y") %>%
      hc_legend(enabled = TRUE)
  })
  
  
}
shinyApp(ui = ui, server = server)

