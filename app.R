library(shiny)
library(plotly)
library(shinythemes)

server <- function(input, output, clientData, session) {
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
    ssa.forecast <- read.table(paste("ssa/", mjd, "_ssa_spbu_365.txt", sep="")) # ssa/55434_ssa_spbu_365.txt
    colnames(ssa.forecast) <- c("MJD", "x", "y", "LOD", "dX", "dY")
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
  
  legend.names <- c("C04", "SSA", "Pul AM", "Pul E1")
  legend.colors <- c("black", "blue", "orange", "green")
  
  series.list <- c("ssa", "pul_am", "pul_e1")
  series.names <- c("SSA", "Pul AM", "Pul E1")
  series.getters <- list(get_ssa, get_pul_am, get_pul_e1)
  
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
  
  output$comparison_table <- renderTable({
    df <- data.frame(x=double(), y=double(), LOD=double(), dX=double(), dY=double())
    mjd <- get_compare_mjd()
    ind <- mjd - 37664
    n <- length(series.list)
    for(i in 1:n) {
      if(series.list[i] %in% input$displaySeries) {
        series <- series.getters[[i]]()
        if(!is.na(series)) {
          rn <- rownames(df)
          df <- rbind(df, data.frame(
            x=  MSE(get_final()[(ind):(ind + 364), "x"],   series[1:365, "x"], 365),
            y=  MSE(get_final()[(ind):(ind + 364), "y"],   series[1:365, "y"], 365),
            LOD=MSE(get_final()[(ind):(ind + 364), "LOD"], series[1:365, "LOD"], 365),
            dX= MSE(get_final()[(ind):(ind + 364), "dX"],  series[1:365, "dX"], 365),
            dY= MSE(get_final()[(ind):(ind + 364), "dY"],  series[1:365, "dY"], 365)))
          rownames(df) <- c(rn, series.names[i])
        }
      }
    }
    df
  }, include.rownames=TRUE, digits=10)
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
                 'displaySeries', 'Series to display', choices=
                   list("SSA"="ssa", "Pulkovo am"="pul_am", "Pulkovo e1"="pul_e1"), multiple=TRUE
               )
             ),
             mainPanel(
               h4("MSE"),
               tableOutput("comparison_table"),
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
               p("By arithmometer")
             )
    )
  )
)

shinyApp(ui = ui, server = server)
