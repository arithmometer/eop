library(shiny)
library(plotly)
library(shinythemes)
library(Rssa)

server <- function(input, output, session) {
  prefix <- "/srv/shiny-server/eop/"
  # prefix <- "/home/grigory/data/R/eop/shiny-server/eop/"

  date_to_mjd <- function(date.string) {
    as.integer(as.Date(date.string) - as.Date("1858-11-17") - 1)
  }

  mjd_to_date <- function(mjd) {
    as.Date("1858-11-17") + mjd
  }

  mjd.today <- reactive({
    date_to_mjd(format(Sys.time(), "%Y-%m-%d"))
  })

  output$mjd <- reactive({
    mjd.today()
  })

  output$date <- reactive({
    mjd_to_date(mjd.today())
  })

  forecast.mjd <- reactive({
    read.csv(paste0(prefix, "rtoday/start_forecast.csv"))$start.forecast
  })

  output$forecast.mjd <- reactive({
    forecast.mjd()
  })

  output$forecast.date <- reactive({
    mjd_to_date(forecast.mjd())
  })

  output$downloadForecast365 <- downloadHandler(
    filename <- function() {
      paste0(forecast.mjd(), "_ssa_spbu_365.txt")
    },

    content <- function(file) {
      file.copy(paste0(prefix, "rssa/", forecast.mjd(), "_ssa_spbu_365.txt"), file)
    },
    contentType = "text/plain"
  )

  output$help <- downloadHandler(
    filename <- function() {
      "help.pdf"
    },
    content <- function(file) {
      file.copy("www/help.pdf", file)
    }
  )

  output$downloadForecast365atDate <- downloadHandler(
    filename <- function() {
      paste0(get_compare_mjd(), "_ssa_spbu_365.txt")
    },

    content <- function(file) {
      file.copy(paste0(prefix, "rssa/", get_compare_mjd(), "_ssa_spbu_365.txt"), file)
    },
    contentType = "text/plain"
  )

  output$downloadForecast90 <- downloadHandler(
    filename <- function() {
      paste0(forecast.mjd(), "_ssa_spbu_90.txt")
    },

    content <- function(file) {
      file.copy(paste0(prefix, "rssa/", forecast.mjd(), "_ssa_spbu_90.txt"), file)
    },
    contentType = "text/plain"
  )

  get_compare_mjd <- reactive({
    mjd <- as.integer(as.Date(input$date_compare) - as.Date("1858-11-17"))
    mjd
  })

  get_ssa <- reactive({
    mjd <- get_compare_mjd()
    ssa.forecast <- tryCatch({read.table(paste0(prefix, "rssa/", mjd, "_ssa_spbu_365.txt"))},
                       silent = TRUE, condition = function(err) { NA } )
    if(!is.na(ssa.forecast)) {
      colnames(ssa.forecast) <- c("MJD", "x", "y", "LOD", "dX", "dY")
    }
    ssa.forecast
  })

  get_pul <- function(x, mjd) {
    pul <- tryCatch({read.table(paste0(prefix, "pul/", mjd - 1, "_", x, "_pul.txt", sep = ""), skip=1)},
                       silent = TRUE, condition = function(err) { NA } )
    if(!is.na(pul)) {
      colnames(pul) <- c("MJD", "x", "y", "TAI-UT1", "LOD", "dX", "dY")
      pul$dX <- pul$dX * 10**(-3)
      pul$dY <- pul$dY * 10**(-3)
      pul$LOD <- pul$LOD * 10**(-3)
    }
    pul
  }

  # check if these files do exist!
  get_pul_am <- reactive({
    get_pul("am", get_compare_mjd())
  })

  get_pul_e1 <- reactive({
    get_pul("e1", get_compare_mjd())
  })

  get_bull_a <- function(mjd_getter) {
    mjd <- mjd_getter()
    date <- as.Date(mjd_to_date(mjd))
    # seek for previous friday
    weekday <- as.numeric(format(date, '%u'))
    date <- date - (weekday + 2) %% 7

    week <- as.numeric(format(date, '%V'))
    year <- as.numeric(format(date, '%Y'))
    volume <- year - 1987
    filename <- sprintf("%sba/bulletina-%s-%03d.csv", prefix, tolower(as.roman(volume)), week)
    ba <- tryCatch({read.csv(filename, sep=";")},
                   silent = TRUE, condition = function(err) { NA } )
    if(!is.na(ba)) {
      colnames(ba) <- c("MJD", "Year", "Month", "Day", "Type", "x", "sigma_x", "y", "sigma_y",
                        "Type.1", "LOD", "sigma_UT1.UTC", "LOD2", "sigma_LOD", "Type.2",
                        "dPsi", "sigma_dPsi", "dEpsilon", "sigma_dEpsilon", "dX", "sigma_dX",
                        "dY", "sigma_dY")
      ind <- which(ba[, "MJD"] == mjd)
      ba <- ba[ind:nrow(ba), ]

      lod <- -diff(ba[, "LOD"])
      leap <- abs(lod) > 0.8 # this is an indicator of leap second
      lod[leap] <- lod[leap] + 1

      ba[, "LOD"] <- c(lod[1], lod) # duplicate the first value because diff eats it
    }
    ba
  }

  get_a <- reactive({
    get_bull_a(get_compare_mjd)
  })

  get_a_today <- reactive({
    get_bull_a(mjd.today)
  })

  get_final <- reactive({
    mjd <- get_compare_mjd()
    c04.file <- paste0(prefix, "eopc04_IAU2000.62-now.txt")
    c04 <- tryCatch({read.table(c04.file, comment.char = "#", skip = 14)},
                   silent = TRUE, condition = function(err) { NA } )
    if(!is.na(c04)) {
      colnames(c04) <- c("Year", "Month", "Day", "MJD", "x", "y", "UT1-UTC",
                         "LOD", "dX", "dY", "x Err", "y Err", "UT1-UTC err", "LOD err", "dX err", "dY err")
    }
    c04
  })

  eop.list <- list("x", "y", "LOD", "dX", "dY")
  days.len <- list(365)

  legend.names <- c("C04", "SSA", "Pul AM", "Bull A")
  legend.colors <- c("black", "blue", "orange", "purple")

  series.list <- c("ssa", "pul_am", "ba")
  series.names <- c("SSA", "Pul AM", "Bull A")
  series.getters <- list(get_ssa, get_pul_am, get_a)

  lapply(days.len, function(days) {
    lapply(eop.list, function(eop) {
      output[[paste0(eop, "_today_", days)]] <- renderPlotly({
        if(input[[paste0("mjd_labels_", days)]]) {
          lab <- "MJD"
          ticks <- forecast.mjd():(forecast.mjd()+days-1)
        } else {
          lab <- "Date"
          start.date <- mjd_to_date(forecast.mjd())
          tm <- seq(0, days-1, by = 1)
          ticks <- start.date + tm
        }

        ssa.forecast <- read.table(paste0(prefix, "rtoday/ssa_spbu_", days, ".txt"))
        colnames(ssa.forecast) <- c("MJD", "x", "y", "LOD", "dX", "dY")

        ba <- get_a_today()
        n <- nrow(ba)
        ba.ticks <- ticks[1:n]

        plot_ly(y=ssa.forecast[[eop]], x=~ticks, type="scatter", mode="lines", name = "SSA") %>%
          add_trace(y = ba[, eop], x=~ba.ticks, name = "Bulletin A") %>%
          layout(xaxis=list(title=lab)) %>%
          layout(yaxis=list(title=eop))
      })
    })

    lapply(eop.list, function(eop) {
      output[[paste0(eop, "_dists_", days)]] <- renderUI({
        L_list <- read.csv(paste0(prefix, "rtoday/", eop, "_", days, "_L_list.csv"))
        do.call(tabsetPanel, lapply(L_list$x, function(L) {
          dists <- read.csv(paste0(prefix, "rtoday/", eop, "_", days, "_", L, "_dists.csv"))
          output[[paste0(eop, "_dists_", days, "_", L)]] <- renderPlotly(plot_ly(dists, y=~x, x=~X, type="scatter", mode="markers") %>%
                                                                           layout(xaxis=list(title="Number of components")) %>%
                                                                           layout(yaxis=list(title="MSE", type="log")))
          tabPanel(paste0("L = ", L), plotlyOutput(paste0(eop, "_dists_", days, "_", L)))
        }))
      })
    })

    output[[paste0("tabset_eop_", days)]] <- renderUI({
      do.call(tabsetPanel, lapply(eop.list, function(eop) {
        tabPanel(eop,
                 h4("Choice of parameters for this forecast:"),
                 verbatimTextOutput(paste0(eop, "_params_", days)),
                 h4(eop),
                 plotlyOutput(paste0(eop, "_today_", days)),
                 tags$br(),
                 h4("MSE"),
                 uiOutput(paste0(eop, "_dists_", days))
        )
      }))
    })

    lapply(eop.list, function(eop) {
      output[[paste0(eop, "_params_", days)]] <- reactive({
        params <- read.csv(paste0(prefix, "rtoday/", days, "params.csv"))
        sprintf("L: %d\np: %d", params[[eop]][1], params[[eop]][2])
      })
    })
  })

  output[["help_dists"]] <- renderUI({
    days <- 365
    L_list <- read.csv(paste0(prefix, "rtoday/", input$generateEOP, "_", days, "_L_list.csv"))
    do.call(tabsetPanel, lapply(L_list$x, function(L) {
      dists <- read.csv(paste0(prefix, "rtoday/", input$generateEOP, "_", days, "_", L, "_dists.csv"))
      output[[paste0(input$generateEOP, "_help_dists_", days, "_", L)]] <- renderPlotly(plot_ly(dists, y=~x, x=~X, type="scatter", mode="markers") %>%
                                                                       layout(xaxis=list(title="Number of components")) %>%
                                                                       layout(yaxis=list(title="MSE", type="log")))
      tabPanel(paste0("L = ", L), plotlyOutput(paste0(input$generateEOP, "_help_dists_", days, "_", L)))
    }))
  })

  lapply(eop.list, function(eop) {
    output[[paste0(eop, "_comparison")]] <- renderPlotly({
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
        series <- series.getters[[i]]()
        if(is.na(series)) {
          if(eop == "x") {
            showNotification(paste(series.names[i], "is not available.", sep=" "), type="error")
          }
        } else {
          if(series.list[i] == "ba") {
            p <- p %>% add_trace(x = ticks[1:nrow(series)], y = series[1:nrow(series), eop], name = series.names[i])
          } else {
            p <- p %>% add_trace(y = series[1:365, eop], name = series.names[i])
          }
        }
      }

      # if(length(input$combineSeries) > 1) {
      #   series <- rep(0, 365)
      #   name <- c()
      #   for(i in 1:(length(series.list)-1)) {
      #     if(series.list[i] %in% input$combineSeries) {
      #       if(is.na(series.getters[[i]]())) {
      #         series <- NA
      #         break
      #       }
      #       series <- series + series.getters[[i]]()[1:365, eop]
      #       name <- c(name, series.names[i])
      #     }
      #   }
      #   if(!is.na(series)) {
      #     cn <- length(input$combineSeries)
      #     series <- series / cn
      #     p <- p %>% add_trace(x = ticks[1:length(series)], y = series, name = paste(name, collapse="+"))
      #   }
      # }
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
      series <- series.getters[[i]]()
      if(!is.na(series)) {
        rn <- rownames(df)
        if(series.list[i] == "ba") {
          n <- nrow(series)
          df <- rbind(df, data.frame(
            x=  MSE(get_final()[(ind):(ind + n - 1), "x"],   series[1:n, "x"],   n),
            y=  MSE(get_final()[(ind):(ind + n - 1), "y"],   series[1:n, "y"],   n),
            LOD=MSE(get_final()[(ind):(ind + n - 1), "LOD"], series[1:n, "LOD"], n),
            dX= NA,
            dY= NA))
        } else {
          df <- rbind(df, data.frame(
            x=  MSE(get_final()[(ind):(ind + 364), "x"],   series[1:365, "x"],   365),
            y=  MSE(get_final()[(ind):(ind + 364), "y"],   series[1:365, "y"],   365),
            LOD=MSE(get_final()[(ind):(ind + 364), "LOD"], series[1:365, "LOD"], 365),
            dX= MSE(get_final()[(ind):(ind + 364), "dX"],  series[1:365, "dX"],  365),
            dY= MSE(get_final()[(ind):(ind + 364), "dY"],  series[1:365, "dY"],  365)))
        }
        rownames(df) <- c(rn, series.names[i])
      }
    }
    if(length(input$combineSeries) > 1) {
      rn <- rownames(df)
      series <- data.frame(x=rep(0, 365), y=rep(0, 365), LOD=rep(0, 365), dX=rep(0, 365), dY=rep(0, 365))
      name <- c()
      for(i in 1:(length(series.list)-1)) {
        if(series.list[i] %in% input$combineSeries) {
          if(is.na(series.getters[[i]]())) {
            series <- NA
            break
          }
          series[, "x"]   <- series[, "x"]   + series.getters[[i]]()[1:365, "x"]
          series[, "y"]   <- series[, "y"]   + series.getters[[i]]()[1:365, "y"]
          series[, "LOD"] <- series[, "LOD"] + series.getters[[i]]()[1:365, "LOD"]
          series[, "dX"]  <- series[, "dX"]  + series.getters[[i]]()[1:365, "dX"]
          series[, "dY"]  <- series[, "dY"]  + series.getters[[i]]()[1:365, "dY"]
          name <- c(name, series.names[i])
        }
      }
      if(!is.na(series)) {
        cn <- length(input$combineSeries)
        series[, "x"]   <- series[, "x"]   / cn
        series[, "y"]   <- series[, "y"]   / cn
        series[, "LOD"] <- series[, "LOD"] / cn
        series[, "dX"]  <- series[, "dX"]  / cn
        series[, "dY"]  <- series[, "dY"]  / cn
        df <- rbind(df, data.frame(
          x=  MSE(get_final()[(ind):(ind + 364), "x"],   series[1:365, "x"],   365),
          y=  MSE(get_final()[(ind):(ind + 364), "y"],   series[1:365, "y"],   365),
          LOD=MSE(get_final()[(ind):(ind + 364), "LOD"], series[1:365, "LOD"], 365),
          dX= MSE(get_final()[(ind):(ind + 364), "dX"],  series[1:365, "dX"],  365),
          dY= MSE(get_final()[(ind):(ind + 364), "dY"],  series[1:365, "dY"],  365)))
        rownames(df) <- c(rn, paste(name, collapse = "+"))
      }
    }
    DT::datatable(df,
                  options=list(pageLength=10,
                               searching=FALSE,
                               paging = FALSE
                  )) %>% DT::formatRound(columns=c("x", "y", "LOD", "dX", "dY"), digits=12)

  }, include.rownames=TRUE, digits=10)

  values <- reactiveValues()
  values$showDownloadButton <- FALSE

  output$showDownloadButton <- reactive({
    values$showDownloadButton
  })

  observe({
    values$showDownloadButton
    values$generatedForecast
  })

  observeEvent(input$generateForecast, {
    values$showDownloadButton <- TRUE

    date.string <- format(Sys.time(), "%Y-%m-%d")
    start.forecast <- as.integer(as.Date(date.string) - as.Date("1858-11-17"))

    c04 <- read.table(paste0(prefix, "eopc04_IAU2000.62-now.txt"), comment.char = "#", skip = 14)
    colnames(c04) <- c("Year", "Month", "Day", "MJD", "x", "y", "UT1-UTC", "LOD",
                       "dX", "dY", "x Err", "y Err", "UT1-UTC err", "LOD err", "dX err", "dY err")
    finals2000 <- read.csv(paste0(prefix, "csv/", start.forecast - 1, "/finals2000A.daily.csv"), sep=";")

    last.mjd.c04 <- c04[nrow(c04), "MJD"]
    mjd.from <- last.mjd.c04 + 1
    mjd.to <- start.forecast - 1
    ind.from <- which(finals2000["MJD"] == mjd.from)
    ind.to <- which(finals2000["MJD"] == mjd.to)

    gap.df <- data.frame("MJD"=mjd.from:mjd.to,
                         "x"  =finals2000[ind.from:ind.to, "x_pole"],
                         "y"  =finals2000[ind.from:ind.to, "y_pole"],
                         "LOD"=finals2000[ind.from:ind.to, "LOD"] / 1000,
                         "dX" =finals2000[ind.from:ind.to, "dX"]  / 1000,
                         "dY" =finals2000[ind.from:ind.to, "dY"]  / 1000)

    c04 <- rbind(c04[, c("MJD", "x", "y", "LOD", "dX", "dY")], gap.df)

    ind <- start.forecast - 37664
    fin <- ind - 1
    period <- input$genBase * 365
    if(input$generateEOP == "LOD") {
      x <- c04[(fin - period):(fin - 1), input$generateEOP]
      forecast.len <- input$genDays + 1
    } else {
      x <- c04[(fin - period + 1):fin, input$generateEOP]
      forecast.len <- input$genDays
    }

    rf.x <- rforecast(ssa(x, L = input$L, neig = input$r), groups = list(1:input$r), len = forecast.len, only.new = TRUE)
    if(input$generateEOP == "LOD") {
      rf.x <- rf.x[-1]
    }

    df <- data.frame(start.forecast:(start.forecast + input$genDays - 1), rf.x)
    colnames(df) <- c("MJD", input$generateEOP)
    values$generatedForecast <- df
  })

  output[["plot_generated"]] <- renderPlotly({
    validate(
      need(values$generatedForecast, "Generate your forecast")
    )
    if(input[["mjd_labels_generated"]]) {
      lab <- "MJD"
      ticks <- forecast.mjd():(forecast.mjd()+input$genDays-1)
    } else {
      lab <- "Date"
      start.date <- mjd_to_date(forecast.mjd())
      tm <- seq(0, input$genDays-1, by = 1)
      ticks <- start.date + tm
    }

    gen.forecast <- values$generatedForecast
    colnames(gen.forecast) <- c("MJD", input$generateEOP)

    plot_ly(y=gen.forecast[[input$generateEOP]], x=~ticks, type="scatter", mode="lines") %>%
      layout(xaxis=list(title=lab)) %>%
      layout(yaxis=list(title=input$generateEOP))
  })

  output$downloadGeneratedForecast <- downloadHandler(
    filename <- function() {
      paste0(forecast.mjd(), "_ssa_spbu_", input$genDays, ".csv")
    },

    content <- function(file) {
      write.csv(values$generatedForecast, file)
    },
    contentType = "text/csv"
  )

  outputOptions(output, "showDownloadButton", suspendWhenHidden=FALSE)
}

ui = tagList(
  navbarPage(
    theme = shinytheme("spacelab"),
    "SSA EOP Forecast",
    tabPanel("Forecast for 365 Days",
             sidebarPanel(
               downloadButton("help", label = "User's manual", class="btn-info"),
               h4("MJD of today"),
               verbatimTextOutput("mjd"),
               verbatimTextOutput("date"),
               h4("Starting MJD of forecast"),
               verbatimTextOutput("forecast.mjd"),
               verbatimTextOutput("forecast.date"),
               checkboxInput("mjd_labels_365", "MJD labels", FALSE),
               tags$hr(),
               p(a(href = "http://tycho.usno.navy.mil/mjd.html", "What is MJD")),
               tags$hr(),
               p("Download forecasts:"),
               downloadButton("downloadForecast365", label = "Download", class="btn-success"),
               tags$br()
             ),
             mainPanel(
               uiOutput("tabset_eop_365")
             )
    ),
    # tabPanel("Forecast for 90 days",
    #          sidebarPanel(
    #            # h4("MJD of today"),
    #            # verbatimTextOutput("mjd"),
    #            # h4("Starting MJD of forecast"),
    #            # verbatimTextOutput("forecast.mjd"),
    #            checkboxInput("mjd_labels_90", "MJD labels", FALSE),
    #            tags$hr(),
    #            p(a(href = "http://tycho.usno.navy.mil/mjd.html", "What is MJD")),
    #            tags$hr(),
    #            p("Download forecasts:"),
    #            downloadButton("downloadForecast90", label = "Download 90 days"),
    #            tags$br()
    #          ),
    #          mainPanel(
    #            uiOutput("tabset_eop_90")
    #          )
    # ),
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
               helpText("Parameter L, window length (or length of time series lag) for decomposition."),
               numericInput("r", "r: (between 0 and 100)", min = 0, max = 100, value = 30),
               helpText("Number of first eigen components to use for decomposition and forecasting."),
               numericInput("genDays", "Days: (between 10 and 3650)", min = 10, max = 3650, value = 365),
               helpText("Length of forecast being generated."),
               numericInput("genBase", "Base period in years: (between 5 and 20)", min = 5, max = 20, value = 5),
               helpText("Base period is a period of time preceding to the forecast which, based on which the forecast will be generated."),
               br(),
               checkboxInput("mjd_labels_generated", "MJD labels", FALSE),
               tags$hr(),
               actionButton("generateForecast", "Generate"),
               br(),
               conditionalPanel(condition = "output.showDownloadButton",
                                downloadButton("downloadGeneratedForecast",
                                               label = "Download forecast", class="btn-success"))
             ),
             mainPanel(
               # uiOutput("help_dists"),
               h4("Generated Forecast"),
               plotlyOutput("plot_generated")
             )
    ),
    tabPanel("Compare Forecasts",
             sidebarPanel(
               p("Choose date after 01.01.2010"),
               tags$hr(),
               dateInput("date_compare", label="Choose starting date", value="2015-01-01", min="2010-01-01",
                         format="dd.mm.yyyy", startview="day", weekstart=1),
               tags$hr(),
               checkboxInput("mjd_compare_labels", "MJD labels", FALSE),
               downloadButton("downloadForecast365atDate", label = "Download", class="btn-success")
               # tags$hr(),
               # selectizeInput(
               #   "combineSeries", "Series to combine",
               #   choices=list("SSA"="ssa", "Pulkovo am"="pul_am", "Pulkovo e1"="pul_e1", "Bulletin A"="ba"), multiple=TRUE,
               #   selected=list("ssa")
               # )
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
               tags$p("This site was created as a part of ", a("Graduation Project (in Russian, 2017)", href="thesis.pdf"),
               " of Grigory Okhotnikov, Master student of Saint Petersburg State University."),
               p("Scientific supervisor: Associate Professor Nina Golyandina, PhD, SPbU."),
               h4("Sources of forecasts for comparison:"),
               tags$ol(
                 tags$li(a(href="https://www.iers.org/IERS/EN/Publications/Bulletins/bulletins.html", "IERS Bulletin A")),
                 tags$li(a(href="http://www.gao.spb.ru/english/as/persac/", "Pulkovo Observatory, Saint Petersburg"))
               ),
               h4("Relevant Sources:"),
               tags$ol(
                 tags$li(a(href = "https://www.crcpress.com/Analysis-of-Time-Series-Structure-SSA-and-Related-Techniques/Golyandina-Nekrutkin-Zhigljavsky/p/book/9781584881940",
                           "Analysis of Time Series Structure: SSA and Related Techniques - Nina Golyandina, Vladimir Nekrutkin, Anatoly Zhigljavsky")),
                 tags$li(a(href = "http://www.gistatgroup.com/cat/", "Caterpillar-SSA website")),
                 tags$li(a(href = "https://github.com/asl/rssa", "Rssa package GitHub repository")),
                 tags$li(a(href = "https://hpiers.obspm.fr/iers/eop/eopc04/C04.guide.pdf",
                           "Earth Orientation Parameters C04 Guide"))
               ),
               tags$hr(),
               p("View the source code on ", a("GitHub.", href="http://www.github.com/arithmometer/eop")),
               p("By arithmometer. Powered by", a("R Shiny.", href="https://shiny.rstudio.com/"))
             )
    )
  )
)

shinyApp(ui = ui, server = server)
