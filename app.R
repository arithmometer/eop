library(shiny)
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
      file.copy(paste(forecast.mjd(), "_ssa_spbu_365.txt", sep=""), file)
    },
    contentType = "application/txt"
  )
  
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
  
  get_ssa_today <- reactive({
    ssa.forecast <- read.table("today/ssa_spbu_365.txt")
    colnames(ssa.forecast) <- c("MJD", "x", "y", "LOD", "dX", "dY")
    ssa.forecast
  })
  
  get_forecast_mjd <- reactive({
    mjd <- as.integer(as.Date(input$date_compare) - as.Date("1858-11-17"))
    mjd
  })
  
  get_ssa <- reactive({
    mjd <- get_forecast_mjd()
    ssa.forecast <- read.table(paste("ssa/", mjd, "_ssa_spbu_365.txt", sep="")) # ssa/55434_ssa_spbu_365.txt
    colnames(ssa.forecast) <- c("MJD", "x", "y", "LOD", "dX", "dY")
    ssa.forecast
  })

  # check if these files do exist!
  get_pul_am <- reactive({
    mjd <- get_forecast_mjd()
    am_pul <- read.table(paste("pul/", mjd - 1, "_am_pul.txt", sep = ""), skip=1) # pul/55434_am_pul.txt
    colnames(am_pul) <- c("MJD", "x", "y", "TAI-UT1", "LOD", "dX", "dY")
    am_pul$dX <- am_pul$dX * 10**(-3)
    am_pul$dY <- am_pul$dY * 10**(-3)
    am_pul$LOD <- am_pul$LOD * 10**(-3)
    am_pul
  })

  get_pul_e1 <- reactive({
    mjd <- get_forecast_mjd()
    e1_pul <- read.table(paste("pul/", mjd - 1, "_e1_pul.txt", sep = ""), skip=1) # pul/55434_am_pul.txt
    colnames(e1_pul) <- c("MJD", "x", "y", "TAI-UT1", "LOD", "dX", "dY")
    e1_pul$dX <- e1_pul$dX * 10**(-3)
    e1_pul$dY <- e1_pul$dY * 10**(-3)
    e1_pul$LOD <- e1_pul$LOD * 10**(-3)
    e1_pul
  })

  get_final <- reactive({
    mjd <- get_forecast_mjd()
    c04.file <- "eopc04_IAU2000.62-now.txt"
    c04 <- read.table(c04.file, comment.char = "#", skip = 14)
    colnames(c04) <- c("Year", "Month", "Day", "MJD", "x", "y", "UT1-UTC",
                       "LOD", "dX", "dY", "x Err", "y Err", "UT1-UTC err", "LOD err", "dX err", "dY err")
    c04
  })
  
  output$x_today <- renderPlot({
    if(input$"mjd_labels") {
      plot(forecast.mjd():(forecast.mjd()+364), get_ssa_today()[, "x"], type="l", ylab="x pole", xlab="MJD", xaxt='n')
      axis(side=1, at=seq(forecast.mjd(), forecast.mjd() + 364, 10))
    } else {
      start.date <- mjd_to_date(forecast.mjd())
      d <- seq(start.date, start.date + 364, by = "days")
      plot(d, get_ssa_today()[, "x"], type="l", ylab="x pole", xlab="", xaxt='n')
      axis.Date(side=1, at=seq(as.Date(start.date), as.Date(start.date) + 364, by = "weeks"), format="%d-%m-%Y", 
                cex.axis=0.7, las=2)  
    }
  })
  
  output$y_today <- renderPlot({
    if(input$"mjd_labels") {
      plot(forecast.mjd():(forecast.mjd()+364), get_ssa_today()[, "y"], type="l", ylab="x pole", xlab="MJD", xaxt='n')
      axis(side=1, at=seq(forecast.mjd(), forecast.mjd() + 364, 10))
    } else {
      start.date <- mjd_to_date(forecast.mjd())
      d <- seq(start.date, start.date + 364, by = "days")
      plot(d, get_ssa_today()[, "y"], type="l", ylab="x pole", xlab="", xaxt='n')
      axis.Date(side=1, at=seq(as.Date(start.date), as.Date(start.date) + 364, by = "weeks"), format="%d-%m-%Y", 
                cex.axis=0.7, las=2)  
    }
  })
  
  output$lod_today <- renderPlot({
    if(input$"mjd_labels") {
      plot(forecast.mjd():(forecast.mjd()+364), get_ssa_today()[, "LOD"], type="l", ylab="x pole", xlab="MJD", xaxt='n')
      axis(side=1, at=seq(forecast.mjd(), forecast.mjd() + 364, 10))
    } else {
      start.date <- mjd_to_date(forecast.mjd())
      d <- seq(start.date, start.date + 364, by = "days")
      plot(d, get_ssa_today()[, "LOD"], type="l", ylab="x pole", xlab="", xaxt='n')
      axis.Date(side=1, at=seq(as.Date(start.date), as.Date(start.date) + 364, by = "weeks"), format="%d-%m-%Y", 
                cex.axis=0.7, las=2)  
    }
  })
  
  output$dx_today <- renderPlot({
    if(input$"mjd_labels") {
      plot(forecast.mjd():(forecast.mjd()+364), get_ssa_today()[, "dX"], type="l", ylab="x pole", xlab="MJD", xaxt='n')
      axis(side=1, at=seq(forecast.mjd(), forecast.mjd() + 364, 10))
    } else {
      start.date <- mjd_to_date(forecast.mjd())
      d <- seq(start.date, start.date + 364, by = "days")
      plot(d, get_ssa_today()[, "dX"], type="l", ylab="x pole", xlab="", xaxt='n')
      axis.Date(side=1, at=seq(as.Date(start.date), as.Date(start.date) + 364, by = "weeks"), format="%d-%m-%Y", 
                cex.axis=0.7, las=2)  
    }
  })
  
  output$dy_today <- renderPlot({
    if(input$"mjd_labels") {
      plot(forecast.mjd():(forecast.mjd()+364), get_ssa_today()[, "dY"], type="l", ylab="x pole", xlab="MJD", xaxt='n')
      axis(side=1, at=seq(forecast.mjd(), forecast.mjd() + 364, 10))
    } else {
      start.date <- mjd_to_date(forecast.mjd())
      d <- seq(start.date, start.date + 364, by = "days")
      plot(d, get_ssa_today()[, "dY"], type="l", ylab="x pole", xlab="", xaxt='n')
      axis.Date(side=1, at=seq(as.Date(start.date), as.Date(start.date) + 364, by = "weeks"), format="%d-%m-%Y", 
                cex.axis=0.7, las=2)  
    }
  })
  
  output$x_dists_365_400 <- renderPlot({
    dists <- read.csv("today/x_365_400_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$x_dists_365_500 <- renderPlot({
    dists <- read.csv("today/x_365_500_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$x_dists_365_550 <- renderPlot({
    dists <- read.csv("today/x_365_550_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$x_dists_365_600 <- renderPlot({
    dists <- read.csv("today/x_365_600_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$x_dists_365_650 <- renderPlot({
    dists <- read.csv("today/x_365_650_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  
  output$y_dists_365_400 <- renderPlot({
    dists <- read.csv("today/y_365_400_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$y_dists_365_500 <- renderPlot({
    dists <- read.csv("today/y_365_500_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$y_dists_365_550 <- renderPlot({
    dists <- read.csv("today/y_365_550_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$y_dists_365_600 <- renderPlot({
    dists <- read.csv("today/y_365_600_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$y_dists_365_650 <- renderPlot({
    dists <- read.csv("today/y_365_650_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  
  output$lod_dists_365_2700 <- renderPlot({
    dists <- read.csv("today/LOD_365_2700_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$lod_dists_365_2750 <- renderPlot({
    dists <- read.csv("today/LOD_365_2750_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$lod_dists_365_2800 <- renderPlot({
    dists <- read.csv("today/LOD_365_2800_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$lod_dists_365_2850 <- renderPlot({
    dists <- read.csv("today/LOD_365_2850_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$lod_dists_365_3000 <- renderPlot({
    dists <- read.csv("today/LOD_365_3000_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  
  output$dx_dists_365_250 <- renderPlot({
    dists <- read.csv("today/dX_365_250_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$dx_dists_365_270 <- renderPlot({
    dists <- read.csv("today/dX_365_270_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$dx_dists_365_300 <- renderPlot({
    dists <- read.csv("today/dX_365_300_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$dx_dists_365_320 <- renderPlot({
    dists <- read.csv("today/dX_365_320_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })

  output$dy_dists_365_250 <- renderPlot({
    dists <- read.csv("today/dY_365_250_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$dy_dists_365_270 <- renderPlot({
    dists <- read.csv("today/dY_365_270_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$dy_dists_365_300 <- renderPlot({
    dists <- read.csv("today/dY_365_300_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  output$dy_dists_365_320 <- renderPlot({
    dists <- read.csv("today/dY_365_320_dists.csv")
    plot(dists, type="l", ylab="MSE", xlab="Number of components")
  })
  
  get_params_today <- reactive({
    params <- read.csv("today/365params.csv", sep="")
    params
  })
  
  output$x_params_365 <- reactive({
    params <- get_params_today()
    sprintf("L: %d\nneig: %d", params$x[1], params$x[2])
  })
  
  output$y_params_365 <- reactive({
    params <- get_params_today()
    sprintf("L: %d\nneig: %d", params$y[1], params$y[2])
  })
  
  output$lod_params_365 <- reactive({
    params <- get_params_today()
    sprintf("L: %d\nneig: %d", params$LOD[1], params$LOD[2])
  })
  
  output$dx_params_365 <- reactive({
    params <- get_params_today()
    sprintf("L: %d\nneig: %d", params$dX[1], params$dX[2])
  })
  
  output$dy_params_365 <- reactive({
    params <- get_params_today()
    sprintf("L: %d\nneig: %d", params$dY[1], params$dY[2])
  })
  
  legend.names <- c("C04", "SSA", "Pul AM", "Pul E1")
  legend.colors <- c("black", "blue", "orange", "green")
  
  output$x_comparison <- renderPlot({
    chosen <- c(TRUE, FALSE, FALSE, FALSE)
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    plot(mjd:(mjd+364), get_final()[(ind):(ind + 364), "x"], type="l", xlab="MJD", ylab="Pole x")
    if("ssa" %in% input$displaySeries) {
      chosen[2] <- TRUE
      lines(mjd:(mjd+364), get_ssa()[1:365, "x"], col="blue", lwd=2)
    }
    if("pul_am" %in% input$displaySeries) {
      chosen[3] <- TRUE
      lines(mjd:(mjd+364), get_pul_am()[1:365, "x"], col="orange")
    }
    if("pul_e1" %in% input$displaySeries) {
      chosen[4] <- TRUE
      lines(mjd:(mjd+364), get_pul_e1()[1:365, "x"], col="green")
    }
    legend("topright", legend.names[chosen], col=legend.colors[chosen], lty=1, bty='n', cex=1.25, xpd=TRUE)
  })
  
  output$y_comparison <- renderPlot({
    chosen <- c(TRUE, FALSE, FALSE, FALSE)
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    plot(mjd:(mjd+364), get_final()[(ind):(ind + 364), "y"], type="l", xlab="MJD", ylab="Pole y")
    if("ssa" %in% input$displaySeries) {
      chosen[2] <- TRUE
      lines(mjd:(mjd+364), get_ssa()[1:365, "y"], col="blue", lwd=2)
    }
    if("pul_am" %in% input$displaySeries) {
      chosen[3] <- TRUE
      lines(mjd:(mjd+364), get_pul_am()[1:365, "y"], col="orange")
    }
    if("pul_e1" %in% input$displaySeries) {
      chosen[4] <- TRUE
      lines(mjd:(mjd+364), get_pul_e1()[1:365, "y"], col="green")
    }
    legend("topright", legend.names[chosen], col=legend.colors[chosen], lty=1, bty='n', cex=1.25, xpd=TRUE)
  })
  
  output$lod_comparison <- renderPlot({
    chosen <- c(TRUE, FALSE, FALSE, FALSE)
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    plot(mjd:(mjd+364), get_final()[(ind):(ind + 364), "LOD"], type="l", xlab="MJD", ylab="LOD")
    if("ssa" %in% input$displaySeries) {
      chosen[2] <- TRUE
      lines(mjd:(mjd+364), get_ssa()[1:365, "LOD"], col="blue", lwd=2)
    }
    if("pul_am" %in% input$displaySeries) {
      chosen[3] <- TRUE
      lines(mjd:(mjd+364), get_pul_am()[1:365, "LOD"], col="orange")
    }
    if("pul_e1" %in% input$displaySeries) {
      chosen[4] <- TRUE
      lines(mjd:(mjd+364), get_pul_e1()[1:365, "LOD"], col="green")
    }
    legend("topright", legend.names[chosen], col=legend.colors[chosen], lty=1, bty='n', cex=1.25, xpd=TRUE)
  })
  
  output$dx_comparison <- renderPlot({
    chosen <- c(TRUE, FALSE, FALSE, FALSE)
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    plot(mjd:(mjd+364), get_final()[(ind):(ind + 364), "dX"], type="l", xlab="MJD", ylab="dX")
    if("ssa" %in% input$displaySeries) {
      chosen[2] <- TRUE
      lines(mjd:(mjd+364), get_ssa()[1:365, "dX"], col="blue", lwd=2)
    }
    if("pul_am" %in% input$displaySeries) {
      chosen[3] <- TRUE
      lines(mjd:(mjd+364), get_pul_am()[1:365, "dX"], col="orange")
    }
    if("pul_e1" %in% input$displaySeries) {
      chosen[4] <- TRUE
      lines(mjd:(mjd+364), get_pul_e1()[1:365, "dX"], col="green")
    }
    legend("topright", legend.names[chosen], col=legend.colors[chosen], lty=1, bty='n', cex=1.25, xpd=TRUE)
  })
  
  output$dy_comparison <- renderPlot({
    chosen <- c(TRUE, FALSE, FALSE, FALSE)
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    plot(mjd:(mjd+364), get_final()[(ind):(ind + 364), "dY"], type="l", xlab="MJD", ylab="dY")
    if("ssa" %in% input$displaySeries) {
      chosen[2] <- TRUE
      lines(mjd:(mjd+364), get_ssa()[1:365, "dY"], col="blue", lwd=2)
    }
    if("pul_am" %in% input$displaySeries) {
      chosen[3] <- TRUE
      lines(mjd:(mjd+364), get_pul_am()[1:365, "dY"], col="orange")
    }
    if("pul_e1" %in% input$displaySeries) {
      chosen[4] <- TRUE
      lines(mjd:(mjd+364), get_pul_e1()[1:365, "dY"], col="green")
    }
    legend("topright", legend.names[chosen], col=legend.colors[chosen], lty=1, bty='n', cex=1.25, xpd=TRUE)
  })
  
  MSE <- function(a, b, n) {
    sum((a - b)**2) / n
  }
  
  output$comparison_table <- renderTable({
    df <- data.frame(x=double(), y=double(), LOD=double(), dX=double(), dY=double())
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    if("ssa" %in% input$displaySeries) {
      rn <- rownames(df)
      df <- rbind(df, data.frame(
                         x=  MSE(get_final()[(ind):(ind + 364), "x"],   get_ssa()[1:365, "x"], 365),
                         y=  MSE(get_final()[(ind):(ind + 364), "y"],   get_ssa()[1:365, "y"], 365),
                         LOD=MSE(get_final()[(ind):(ind + 364), "LOD"], get_ssa()[1:365, "LOD"], 365),
                         dX= MSE(get_final()[(ind):(ind + 364), "dX"],  get_ssa()[1:365, "dX"], 365),
                         dY= MSE(get_final()[(ind):(ind + 364), "dY"],  get_ssa()[1:365, "dY"], 365)))
      rownames(df) <- c(rn, "SSA")
    }
    if("pul_am" %in% input$displaySeries) {
      rn <- rownames(df)
      df <- rbind(df, data.frame(
                         x=  MSE(get_final()[(ind):(ind + 364), "x"],   get_pul_am()[1:365, "x"], 365),
                         y=  MSE(get_final()[(ind):(ind + 364), "y"],   get_pul_am()[1:365, "y"], 365),
                         LOD=MSE(get_final()[(ind):(ind + 364), "LOD"], get_pul_am()[1:365, "LOD"], 365),
                         dX= MSE(get_final()[(ind):(ind + 364), "dX"],  get_pul_am()[1:365, "dX"], 365),
                         dY= MSE(get_final()[(ind):(ind + 364), "dY"],  get_pul_am()[1:365, "dY"], 365)))
      rownames(df) <- c(rn, "Pul AM")
    }
    if("pul_e1" %in% input$displaySeries) {
      rn <- rownames(df)
      df <- rbind(df, data.frame(
                         x=  MSE(get_final()[(ind):(ind + 364), "x"],   get_pul_e1()[1:365, "x"], 365),
                         y=  MSE(get_final()[(ind):(ind + 364), "y"],   get_pul_e1()[1:365, "y"], 365),
                         LOD=MSE(get_final()[(ind):(ind + 364), "LOD"], get_pul_e1()[1:365, "LOD"], 365),
                         dX= MSE(get_final()[(ind):(ind + 364), "dX"],  get_pul_e1()[1:365, "dX"], 365),
                         dY= MSE(get_final()[(ind):(ind + 364), "dY"],  get_pul_e1()[1:365, "dY"], 365)))
      rownames(df) <- c(rn, "Pul E1")
    }
    df
  }, include.rownames=TRUE, digits=10)
}

ui = tagList(
  navbarPage(
    theme = shinytheme("spacelab"),
    "EOP Forecast",
    tabPanel("Forecast of today",
             sidebarPanel(
               h4("MJD of today"),
               verbatimTextOutput("mjd"),
               h4("Starting MJD of forecast"),
               verbatimTextOutput("forecast.mjd"),
               checkboxInput("mjd_labels", "MJD labels", FALSE),
               tags$hr(),
               p(a(href = "http://tycho.usno.navy.mil/mjd.html", "What is MJD")),
               tags$hr(),
               p("Download forecasts:"),
               downloadButton("downloadForecast90", label = "Download 90 days"),
               tags$br(),
               downloadButton("downloadForecast365", label = "Download 365 days"),
               tags$br()
             ),
             mainPanel(
               tabsetPanel(type = "tabs", 
                           tabPanel("Pole x",
                             h4("Choice of parameters for this forecast:"),
                             verbatimTextOutput("x_params_365"),
                             h4("Pole x"),
                             plotOutput("x_today"),
                             tags$br(),
                             h4("MSE"),
                             tabsetPanel(type="tabs",
                                         tabPanel("L = 400", plotOutput("x_dists_365_400")),
                                         tabPanel("L = 500", plotOutput("x_dists_365_500")),
                                         tabPanel("L = 550", plotOutput("x_dists_365_550")),
                                         tabPanel("L = 600", plotOutput("x_dists_365_600")),
                                         tabPanel("L = 650", plotOutput("x_dists_365_650"))
                                         )
                           ), 
                           tabPanel("Pole y",
                             h4("Choice of parameters for this forecast:"),
                             verbatimTextOutput("y_params_365"),
                             h4("Pole y"),
                             plotOutput("y_today"),
                             tags$br(),
                             h4("MSE"),
                             tabsetPanel(type="tabs",
                                         tabPanel("L = 400", plotOutput("y_dists_365_400")),
                                         tabPanel("L = 500", plotOutput("y_dists_365_500")),
                                         tabPanel("L = 550", plotOutput("y_dists_365_550")),
                                         tabPanel("L = 600", plotOutput("y_dists_365_600")),
                                         tabPanel("L = 650", plotOutput("y_dists_365_650"))
                             )
                           ),
                           tabPanel("LOD",
                             h4("Choice of parameters for this forecast:"),
                             verbatimTextOutput("lod_params_365"),
                             h4("LOD"),
                             plotOutput("lod_today"),
                             tags$br(),
                             h4("MSE"),
                             tabsetPanel(type="tabs",
                                         tabPanel("L = 2700", plotOutput("lod_dists_365_2700")),
                                         tabPanel("L = 2750", plotOutput("lod_dists_365_2750")),
                                         tabPanel("L = 2800", plotOutput("lod_dists_365_2800")),
                                         tabPanel("L = 2850", plotOutput("lod_dists_365_2850")),
                                         tabPanel("L = 3000", plotOutput("lod_dists_365_3000"))
                             )
                           ),
                           tabPanel("dX",
                             h4("Choice of parameters for this forecast:"),
                             verbatimTextOutput("dx_params_365"),
                             h4("dX"),
                             plotOutput("dx_today"),
                             tags$br(),
                             h4("MSE"),
                             tabsetPanel(type="tabs",
                                         tabPanel("L = 250", plotOutput("dx_dists_365_250")),
                                         tabPanel("L = 270", plotOutput("dx_dists_365_270")),
                                         tabPanel("L = 300", plotOutput("dx_dists_365_300")),
                                         tabPanel("L = 320", plotOutput("dx_dists_365_320"))
                             )
                           ),
                           tabPanel("dY",
                             h4("Choice of parameters for this forecast:"),
                             verbatimTextOutput("dy_params_365"),
                             h4("dY"),
                             plotOutput("dy_today"),
                             tags$br(),
                             h4("MSE"),
                             tabsetPanel(type="tabs",
                                         tabPanel("L = 250", plotOutput("dy_dists_365_250")),
                                         tabPanel("L = 270", plotOutput("dy_dists_365_270")),
                                         tabPanel("L = 300", plotOutput("dy_dists_365_300")),
                                         tabPanel("L = 320", plotOutput("dy_dists_365_320"))
                             )
                           )
               )
             )
    ),
    tabPanel("Compare Forecasts",
             sidebarPanel(
               p("Date input is limited between 27.08.2010 and 1.02.2016"),
               tags$hr(),
               dateInput("date_compare", label="Choose starting date", value="2010-08-27",
                         min="2010-08-26", max="2017-02-04",
                         format="dd.mm.yyyy", startview="day", weekstart=1),
               tags$hr(),
               selectizeInput(
                 'displaySeries', 'Series to display', choices=
                   list("SSA"="ssa", "Pulkovo am"="pul_am", "Pulkovo e1"="pul_e1"), multiple=TRUE
               )
             ),
             mainPanel(
               h4("MSE"),
               tableOutput('comparison_table'),
               tags$hr(),
               h4("Pole x"),
               plotOutput("x_comparison"),
               h4("Pole y"),
               plotOutput("y_comparison"),
               h4("LOD"),
               plotOutput("lod_comparison"),
               h4("dX"),
               plotOutput("dx_comparison"),
               h4("dY"),
               plotOutput("dy_comparison")
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
