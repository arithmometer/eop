library(shiny)
library(plotly)
library(shinythemes)
library(Rssa)

server <- function(input, output, clientData, session) {
  prefix <- "/srv/shiny-server/eop/"
  
  mjd.today <- reactive({
    date.string <- format(Sys.time(), "%Y-%m-%d")
    as.integer(as.Date(date.string) - as.Date("1858-11-17") - 1)
  })
  
  output$mjd <- reactive({
    mjd.today()
  })
  
  forecast.mjd <- reactive({
    read.csv("today/start_forecast.csv")$start.forecast
  })
  
  output$forecast.mjd <- reactive({
    forecast.mjd()
  })
  
  mjd_to_date <- function(mjd) {
    as.Date("1858-11-17") + mjd
  }
  
  output$downloadForecast365 <- downloadHandler(
    filename <- function() {
      paste(forecast.mjd(), "_ssa_spbu_365.txt", sep="")
    },
    
    content <- function(file) {
      file.copy(paste("ssa/", forecast.mjd(), "_ssa_spbu_365.txt", sep=""), file)
    },
    contentType = "text/plain"
  )
  
  output$downloadForecast90 <- downloadHandler(
    filename <- function() {
      paste(forecast.mjd(), "_ssa_spbu_90.txt", sep="")
    },
    
    content <- function(file) {
      file.copy(paste("ssa/", forecast.mjd(), "_ssa_spbu_90.txt", sep=""), file)
    },
    contentType = "text/plain"
  )
  
  get_compare_mjd <- reactive({
    mjd <- as.integer(as.Date(input$date_compare) - as.Date("1858-11-17"))
    mjd
  })
  
  get_ssa <- reactive({
    mjd <- get_compare_mjd()
    ssa.forecast <- tryCatch({read.table(paste("ssa/", mjd, "_ssa_spbu_365.txt", sep=""))},
                       silent = TRUE, condition = function(err) { NA } )
    if(!is.na(ssa.forecast)) {
      colnames(ssa.forecast) <- c("MJD", "x", "y", "LOD", "dX", "dY")
    }
    ssa.forecast
  })

  # check if these files do exist!
  get_pul_am <- reactive({
    mjd <- get_compare_mjd()
    am_pul <- tryCatch({read.table(paste("pul/", mjd - 1, "_am_pul.txt", sep = ""), skip=1)},
             silent = TRUE, condition = function(err) { NA } )
    if(!is.na(am_pul)) {
      colnames(am_pul) <- c("MJD", "x", "y", "TAI-UT1", "LOD", "dX", "dY")
      am_pul$dX <- am_pul$dX * 10**(-3)
      am_pul$dY <- am_pul$dY * 10**(-3)
      am_pul$LOD <- am_pul$LOD * 10**(-3) 
    }
    am_pul
  })

  get_pul_e1 <- reactive({
    mjd <- get_compare_mjd()
    e1_pul <- tryCatch({read.table(paste("pul/", mjd - 1, "_e1_pul.txt", sep = ""), skip=1)},
             silent = TRUE, condition = function(err) { NA } )
    if(!is.na(e1_pul)) {
      colnames(e1_pul) <- c("MJD", "x", "y", "TAI-UT1", "LOD", "dX", "dY")
      e1_pul$dX <- e1_pul$dX * 10**(-3)
      e1_pul$dY <- e1_pul$dY * 10**(-3)
      e1_pul$LOD <- e1_pul$LOD * 10**(-3)  
    }
    e1_pul
  })
  
  get_a <- reactive({
    mjd <- get_compare_mjd()
    date.string <- mjd_to_date(mjd)
    year <- as.numeric(format(as.Date(date.string), '%Y'))
    volume <- year - 1987
    week <- as.numeric(format(as.Date(date.string), '%U'))
    filename <- sprintf("ba/bulletina-%s-%03d.txt", tolower(as.roman(volume)), week)
    ba <- tryCatch({read.csv(filename, sep=";")},
                       silent = TRUE, condition = function(err) { NA } )
    if(!is.na(ba)) {
      colnames(ba) <- c("MJD", "Year", "Month", "Day", "Type", "x", "sigma_x", "y", "sigma_y",
                        "Type.1", "UT1.UTC", "sigma_UT1.UTC", "LOD", "sigma_LOD", "Type.2",
                        "dPsi", "sigma_dPsi", "dEpsilon", "sigma_dEpsilon", "dX", "sigma_dX",
                        "dY", "sigma_dY")
    }
    ind <- which(ba[, "MJD"] == mjd)
    n <- min(nrow(ba), 365)
    ba[ind:n, ]
  })

  get_final <- reactive({
    mjd <- get_compare_mjd()
    c04.file <- "eopc04_IAU2000.62-now.txt"
    c04 <- read.table(c04.file, comment.char = "#", skip = 14)
    colnames(c04) <- c("Year", "Month", "Day", "MJD", "x", "y", "UT1-UTC",
                       "LOD", "dX", "dY", "x Err", "y Err", "UT1-UTC err", "LOD err", "dX err", "dY err")
    c04
  })
  
  eop.list <- list("x", "y", "LOD", "dX", "dY")
  days.len <- list(365, 90)
  
  legend.names <- c("C04", "SSA", "Pul AM", "Pul E1", "Bull A")
  legend.colors <- c("black", "blue", "orange", "green", "purple")
  
  series.list <- c("ssa", "pul_am", "pul_e1", "ba")
  series.names <- c("SSA", "Pul AM", "Pul E1", "Bull A")
  series.getters <- list(get_ssa, get_pul_am, get_pul_e1, get_a)
  
  lapply(days.len, function(days) {
    lapply(eop.list, function(eop) {
      output[[paste(eop, "_today_", days, sep="")]] <- renderPlotly({
        if(input[[paste("mjd_labels_", days, sep="")]]) {
          lab <- "MJD"
          ticks <- forecast.mjd():(forecast.mjd()+days-1)
        } else {
          lab <- "Date"
          start.date <- mjd_to_date(forecast.mjd())
          tm <- seq(0, days-1, by = 1)
          ticks <- start.date + tm
        }
        
        ssa.forecast <- read.table(paste("today/ssa_spbu_", days, ".txt", sep=""))
        colnames(ssa.forecast) <- c("MJD", "x", "y", "LOD", "dX", "dY")
        
        plot_ly(y=ssa.forecast[[eop]], x=~ticks, type="scatter", mode="lines") %>% 
          layout(xaxis=list(title=lab)) %>% 
          layout(yaxis=list(title=eop))
      })  
    })
    
    lapply(eop.list, function(eop) {
      output[[paste(eop, "_dists_", days, sep="")]] <- renderUI({
        L_list <- read.csv(paste("today/", eop, "_", days, "_L_list.csv", sep=""))
        do.call(tabsetPanel, lapply(L_list$x, function(L) {
          dists <- read.csv(paste("today/", eop, "_", days, "_", L, "_dists.csv", sep=""))
          output[[paste(eop, "_dists_", days, "_", L, sep="")]] <- renderPlotly(plot_ly(dists, y=~x, x=~X, type="scatter", mode="markers") %>%
                                                                           layout(xaxis=list(title="Number of components")) %>% 
                                                                           layout(yaxis=list(title="MSE", type="log")))
          tabPanel(paste("L = ", L, sep=""), plotlyOutput(paste(eop, "_dists_", days, "_", L, sep="")))
        }))
      })
    })
    
    output[[paste("tabset_eop_", days, sep="")]] <- renderUI({
      do.call(tabsetPanel, lapply(eop.list, function(eop) {
        tabPanel(eop,
                 h4("Choice of parameters for this forecast:"),
                 verbatimTextOutput(paste(eop, "_params_", days, sep="")),
                 h4(eop),
                 plotlyOutput(paste(eop, "_today_", days, sep="")),
                 tags$br(),
                 h4("MSE"),
                 uiOutput(paste(eop, "_dists_", days, sep=""))
        )
      }))
    })
    
    lapply(eop.list, function(eop) {
      output[[paste(eop, "_params_", days, sep="")]] <- reactive({
        params <- read.csv(paste("today/", days, "params.csv", sep=""))
        sprintf("L: %d\np: %d", params[[eop]][1], params[[eop]][2])
      })
    })
  })
  
  lapply(eop.list, function(eop) {
    output[[paste(eop, "_comparison", sep="")]] <- renderPlotly({
      mjd <- get_compare_mjd()
      ind <- mjd - 37664
      
      if(input$"mjd_compare_labels") {
        lab <- "MJD"
        ticks <- mjd:(mjd+364)
      } else {
        lab <- "Date"
        start.date <- mjd_to_date(mjd)
        tm <- seq(0, 364, by = 1)
        ticks <- start.date + tm
      }

      p <- plot_ly(x = ~ticks, y = get_final()[(ind):(ind + 364), eop], type = 'scatter', mode = 'lines', name = 'C04') %>%
        layout(xaxis=list(title=lab)) %>% 
        layout(yaxis=list(title=eop))
      n <- length(series.list)
      for(i in 1:n) {
        if(series.list[i] %in% input$displaySeries) {
          series <- series.getters[[i]]()
          if(is.na(series)) {
            if(eop == "x") {
              showNotification(paste(series.names[i], "is not available.", sep=" "), type="error")
            }
          } else {
            p <- p %>% add_trace(y = series[1:365, eop], name = series.names[i])
          }
        }
      }
      p
    })
  })
  
  MSE <- function(a, b, n) {
    sum((a - b)**2) / n
  }
  
  output$comparison_table <- DT::renderDataTable({
    df <- data.frame(x=double(), y=double(), LOD=double(), dX=double(), dY=double())
    mjd <- get_compare_mjd()
    ind <- mjd - 37664
    n <- length(series.list)
    for(i in 1:n) {
      if(series.list[i] %in% input$displaySeries) {
        series <- series.getters[[i]]()
        if(!is.na(series)) {
          rn <- rownames(df)
          if(series.list[i] == "ba") {
            n <- nrow(df)
            df <- rbind(df, data.frame(
              x=  MSE(get_final()[(ind):(ind + n - 1), "x"],   series[1:n, "x"], n),
              y=  MSE(get_final()[(ind):(ind + n - 1), "y"],   series[1:n, "y"], n),
              LOD=NA,
              dX= NA,
              dY= NA))
          } else {
            df <- rbind(df, data.frame(
              x=  MSE(get_final()[(ind):(ind + 364), "x"],   series[1:365, "x"], 365),
              y=  MSE(get_final()[(ind):(ind + 364), "y"],   series[1:365, "y"], 365),
              LOD=MSE(get_final()[(ind):(ind + 364), "LOD"], series[1:365, "LOD"], 365),
              dX= MSE(get_final()[(ind):(ind + 364), "dX"],  series[1:365, "dX"], 365),
              dY= MSE(get_final()[(ind):(ind + 364), "dY"],  series[1:365, "dY"], 365)))
          }
          rownames(df) <- c(rn, series.names[i])
        }
      }
    }
    DT::datatable(df,
                  options=list(pageLength=10, 
                               # lengthMenu=list(c(5, 10, 30, 100, -1), c('5', '10', '30', '100', 'Все')),
                               searching=FALSE
                  ))
  }, include.rownames=TRUE, digits=10)
  
  values <- reactiveValues()
  values$showDownloadButton <- FALSE

  observe({
    values$showDownloadButton
    values$generatedForecast
  })

  output$showDownloadButton <- reactive({
    return(values$showDownloadButton)
  })
  
  observeEvent(input$generateForecast, {
    values$showDownloadButton <- TRUE
    
    date.string <- format(Sys.time(), "%Y-%m-%d")
    start.forecast <- as.integer(as.Date(date.string) - as.Date("1858-11-17"))

    c04 <- read.table(paste(prefix, "eopc04_IAU2000.62-now.txt", sep=""), comment.char = "#", skip = 14)
    colnames(c04) <- c("Year", "Month", "Day", "MJD", "x", "y", "UT1-UTC", "LOD",
                       "dX", "dY", "x Err", "y Err", "UT1-UTC err", "LOD err", "dX err", "dY err")
    finals2000 <- read.csv(paste(prefix, "csv/", start.forecast - 1, "/finals2000A.daily.csv", sep=""), sep=";")

    last.mjd.c04 <- c04[nrow(c04), "MJD"]
    mjd.from <- last.mjd.c04 + 1
    mjd.to <- start.forecast - 1
    ind.from <- which(finals2000["MJD"] == mjd.from)
    ind.to <- which(finals2000["MJD"] == mjd.to)

    gap.df <- data.frame("MJD"=mjd.from:mjd.to,
                         "x"=finals2000[ind.from:ind.to, "x_pole"],
                         "y"=finals2000[ind.from:ind.to, "y_pole"],
                         "LOD"=finals2000[ind.from:ind.to, "LOD"] / 1000,
                         "dX"=finals2000[ind.from:ind.to, "dX"] / 1000,
                         "dY"=finals2000[ind.from:ind.to, "dY"] / 1000)

    c04 <- rbind(c04[, c("MJD", "x", "y", "LOD", "dX", "dY")], gap.df)

    ind <- start.forecast - 37664
    fin <- ind - 1
    years <- 20
    period <- years * 365
    if(input$generateEOP == "LOD") {
      x <- c04[(fin - period):(fin - 1), input$generateEOP]
      forecast.len <- input$genDays + 1
    } else {
      x <- c04[(fin - period + 1):fin, input$generateEOP]
      forecast.len <- input$genDays
    }

    rf.x <- rforecast(ssa(x, L = input$L, neig = input$p), groups = list(1:input$p), len = forecast.len, only.new = TRUE)
    if(input$generateEOP == "LOD") {
      rf.x <- rf.x[-1]
    }

    df <- data.frame(start.forecast:(start.forecast + input$genDays - 1), rf.x)
    colnames(df) <- c("MJD", input$generateEOP)
    values$generatedForecast <- df
  })
  
  output$downloadGeneratedForecast <- downloadHandler(
    filename <- function() {
      paste(forecast.mjd(), "_ssa_spbu_", input$genDays, ".csv", sep="")
    },
    
    content <- function(file) {
      write.csv(values$generatedForecast, file)
    },
    contentType = "text/csv"
  )
}

ui = tagList(
  navbarPage(
    theme = shinytheme("spacelab"),
    "EOP Forecast",
    tabPanel("Forecast for 365 days",
             sidebarPanel(
               h4("MJD of today"),
               verbatimTextOutput("mjd"),
               h4("Starting MJD of forecast"),
               verbatimTextOutput("forecast.mjd"),
               checkboxInput("mjd_labels_365", "MJD labels", FALSE),
               tags$hr(),
               p(a(href = "http://tycho.usno.navy.mil/mjd.html", "What is MJD")),
               tags$hr(),
               p("Download forecasts:"),
               downloadButton("downloadForecast365", label = "Download 365 days"),
               tags$br()
             ),
             mainPanel(
               uiOutput("tabset_eop_365")
             )
    ),
    tabPanel("Forecast for 90 days",
             sidebarPanel(
               # h4("MJD of today"),
               # verbatimTextOutput("mjd"),
               # h4("Starting MJD of forecast"),
               # verbatimTextOutput("forecast.mjd"),
               checkboxInput("mjd_labels_90", "MJD labels", FALSE),
               tags$hr(),
               p(a(href = "http://tycho.usno.navy.mil/mjd.html", "What is MJD")),
               tags$hr(),
               p("Download forecasts:"),
               downloadButton("downloadForecast90", label = "Download 90 days"),
               tags$br()
             ),
             mainPanel(
               uiOutput("tabset_eop_90")
             )
    ),
    tabPanel("Generate Forecast",
             sidebarPanel(
               h4("SSA Forecast Parameters"),
               selectInput("generateEOP", "Choose EOP:",
                           c("Pole x" = "x",
                             "Pole y" = "y",
                             "LOD" = "LOD",
                             "dX" = "dX",
                             "dY" = "dY")),
               numericInput("L", "L: (between 100 and 10000)", min = 100, max = 10000, value = 500),
               numericInput("p", "p: (between 0 and 100)", min = 0, max = 100, value = 30),
               numericInput("genDays", "Days: (between 10 and 3650)", min = 10, max = 3650, value = 365),
               br(),
               actionButton("generateForecast", "Generate"),
               br(),
               conditionalPanel(condition = "output.showDownloadButton == TRUE", downloadButton("downloadGeneratedForecast", label = "Download forecast"))
             ),
             mainPanel(
               h4("Generated Forecast")
             )
    ),
    tabPanel("Compare Forecasts",
             sidebarPanel(
               p("Date input is limited between 27.08.2010 and 1.03.2016"),
               tags$hr(),
               dateInput("date_compare", label="Choose starting date", value="2015-12-11",
                         min="2010-08-26", max="2016-03-01",
                         format="dd.mm.yyyy", startview="day", weekstart=1),
               tags$hr(),
               checkboxInput("mjd_compare_labels", "MJD labels", FALSE),
               tags$hr(),
               selectizeInput(
                 'displaySeries', 'Series to display',
                 choices=list("SSA"="ssa", "Pulkovo am"="pul_am", "Pulkovo e1"="pul_e1", "Bulletin A"="ba"), multiple=TRUE,
                 selected=list("ssa", "pul_am", "pul_e1", "ba")
               )
             ),
             mainPanel(
               h4("MSE"),
               DT::dataTableOutput("comparison_table"),
               tags$hr(),
               h4("Pole x"),
               plotlyOutput("x_comparison"),
               h4("Pole y"),
               plotlyOutput("y_comparison"),
               h4("LOD"),
               plotlyOutput("LOD_comparison"),
               h4("dX"),
               plotlyOutput("dX_comparison"),
               h4("dY"),
               plotlyOutput("dY_comparison")
             )
    ),
    tabPanel("About",
             mainPanel(
               h4("Relevant Sources:"),
               tags$ol(
                 tags$li(a(href = "http://www.gistatgroup.com/cat/", "Caterpillar-SSA")), 
                 tags$li(a(href = "https://github.com/asl/rssa", "Rssa GitHub repository")),
                 tags$li(a(href = "https://hpiers.obspm.fr/iers/eop/eopc04/C04.guide.pdf", 
                           "Earth Orientation Parameters C04 Guide"))
               ),
               tags$hr(),
               p("By arithmometer"),
               a("GitHub", href="http://www.github.com/arithmometer/eop")
             )
    )
  )
)

shinyApp(ui = ui, server = server)
