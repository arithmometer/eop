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
  
  output$x_today <- renderPlotly({
    if(input$"mjd_labels") {
      lab <- "MJD"
      ticks <- forecast.mjd():(forecast.mjd()+364)
    } else {
      lab <- "Date"
      start.date <- mjd_to_date(forecast.mjd())
      tm <- seq(0, 364, by = 1)
      ticks <- start.date + tm
    }
    
    plot_ly(get_ssa_today(), y=~x, x=~ticks, type="scatter", mode="markers") %>% 
      layout(xaxis=list(title=lab)) %>% 
      layout(yaxis=list(title="x"))
  })
  
  output$y_today <- renderPlotly({
    if(input$"mjd_labels") {
      lab <- "MJD"
      ticks <- forecast.mjd():(forecast.mjd()+364)
    } else {
      lab <- "Date"
      start.date <- mjd_to_date(forecast.mjd())
      tm <- seq(0, 364, by = 1)
      ticks <- start.date + tm
    }
    
    plot_ly(get_ssa_today(), y=~y, x=~ticks, type="scatter", mode="markers") %>% 
      layout(xaxis=list(title=lab)) %>% 
      layout(yaxis=list(title="y"))
  })
  
  output$LOD_today <- renderPlotly({
    if(input$"mjd_labels") {
      lab <- "MJD"
      ticks <- forecast.mjd():(forecast.mjd()+364)
    } else {
      lab <- "Date"
      start.date <- mjd_to_date(forecast.mjd())
      tm <- seq(0, 364, by = 1)
      ticks <- start.date + tm
    }
    
    plot_ly(get_ssa_today(), y=~LOD, x=~ticks, type="scatter", mode="markers") %>% 
      layout(xaxis=list(title=lab)) %>% 
      layout(yaxis=list(title="LOD"))
  })
  
  output$dX_today <- renderPlotly({
    if(input$"mjd_labels") {
      lab <- "MJD"
      ticks <- forecast.mjd():(forecast.mjd()+364)
    } else {
      lab <- "Date"
      start.date <- mjd_to_date(forecast.mjd())
      tm <- seq(0, 364, by = 1)
      ticks <- start.date + tm
    }
    
    plot_ly(get_ssa_today(), y=~dX, x=~ticks, type="scatter", mode="markers") %>% 
      layout(xaxis=list(title=lab)) %>% 
      layout(yaxis=list(title="dX"))
  })
  
  output$dY_today <- renderPlotly({
    if(input$"mjd_labels") {
      lab <- "MJD"
      ticks <- forecast.mjd():(forecast.mjd()+364)
    } else {
      lab <- "Date"
      start.date <- mjd_to_date(forecast.mjd())
      tm <- seq(0, 364, by = 1)
      ticks <- start.date + tm
    }
    
    plot_ly(get_ssa_today(), y=~dY, x=~ticks, type="scatter", mode="markers") %>% 
      layout(xaxis=list(title=lab)) %>% 
      layout(yaxis=list(title="dY"))
  })

  output$x_dists <- renderUI({
    L_list <- read.csv("today/x_365_L_list.csv")
    do.call(tabsetPanel, lapply(L_list$x, function(L) {
      dists <- read.csv(paste("today/x_365_", L, "_dists.csv", sep=""))
      output[[paste("x_dists_365_", L, sep="")]] <- renderPlotly(plot_ly(dists, y=~x, x=~X, type="scatter", mode="markers") %>%
                                                                   layout(xaxis=list(title="Number of components")) %>% 
                                                                   layout(yaxis=list(title="MSE")))
      tabPanel(paste("L = ", L, sep=""), plotlyOutput(paste("x_dists_365_", L, sep="")))
    }))
  })
  
  output$y_dists <- renderUI({
    L_list <- read.csv("today/y_365_L_list.csv")
    do.call(tabsetPanel, lapply(L_list$x, function(L) {
      dists <- read.csv(paste("today/y_365_", L, "_dists.csv", sep=""))
      output[[paste("y_dists_365_", L, sep="")]] <- renderPlotly(plot_ly(dists, y=~x, x=~X, type="scatter", mode="markers") %>%
                                                                   layout(xaxis=list(title="Number of components")) %>% 
                                                                   layout(yaxis=list(title="MSE")))
      tabPanel(paste("L = ", L, sep=""), plotlyOutput(paste("y_dists_365_", L, sep="")))
    }))
  })
  
  output$LOD_dists <- renderUI({
    L_list <- read.csv("today/LOD_365_L_list.csv")
    do.call(tabsetPanel, lapply(L_list$x, function(L) {
      dists <- read.csv(paste("today/LOD_365_", L, "_dists.csv", sep=""))
      output[[paste("LOD_dists_365_", L, sep="")]] <- renderPlotly(plot_ly(dists, y=~x, x=~X, type="scatter", mode="markers") %>%
                                                                   layout(xaxis=list(title="Number of components")) %>% 
                                                                   layout(yaxis=list(title="MSE")))
      tabPanel(paste("L = ", L, sep=""), plotlyOutput(paste("LOD_dists_365_", L, sep="")))
    }))
  })
  
  output$dX_dists <- renderUI({
    L_list <- read.csv("today/dX_365_L_list.csv")
    do.call(tabsetPanel, lapply(L_list$x, function(L) {
      dists <- read.csv(paste("today/dX_365_", L, "_dists.csv", sep=""))
      output[[paste("dX_dists_365_", L, sep="")]] <- renderPlotly(plot_ly(dists, y=~x, x=~X, type="scatter", mode="markers") %>%
                                                                     layout(xaxis=list(title="Number of components")) %>% 
                                                                     layout(yaxis=list(title="MSE")))
      tabPanel(paste("L = ", L, sep=""), plotlyOutput(paste("dX_dists_365_", L, sep="")))
    }))
  })
  
  output$dY_dists <- renderUI({
    L_list <- read.csv("today/dY_365_L_list.csv")
    do.call(tabsetPanel, lapply(L_list$x, function(L) {
      dists <- read.csv(paste("today/dY_365_", L, "_dists.csv", sep=""))
      output[[paste("dY_dists_365_", L, sep="")]] <- renderPlotly(plot_ly(dists, y=~x, x=~X, type="scatter", mode="markers") %>%
                                                                    layout(xaxis=list(title="Number of components")) %>% 
                                                                    layout(yaxis=list(title="MSE")))
      tabPanel(paste("L = ", L, sep=""), plotlyOutput(paste("dY_dists_365_", L, sep="")))
    }))
  })
  
  eop.list <- list("x", "y", "LOD", "dX", "dY")
  
  output$tabset_eop <- renderUI({
    do.call(tabsetPanel, lapply(eop.list, function(eop) {
      tabPanel(eop,
        h4("Choice of parameters for this forecast:"),
        verbatimTextOutput(paste(eop, "_params_365", sep="")),
        h4(eop),
        plotlyOutput(paste(eop, "_today", sep="")),
        tags$br(),
        h4("MSE"),
        uiOutput(paste(eop, "_dists", sep=""))
      )
    }))
  })
  
  get_params_today <- reactive({
    params <- read.csv("today/365params.csv")
    params
  })
  
  output$x_params_365 <- reactive({
    params <- get_params_today()
    sprintf("L: %d\np: %d", params$x[1], params$x[2])
  })
  
  output$y_params_365 <- reactive({
    params <- get_params_today()
    sprintf("L: %d\np: %d", params$y[1], params$y[2])
  })
  
  output$lod_params_365 <- reactive({
    params <- get_params_today()
    sprintf("L: %d\np: %d", params$LOD[1], params$LOD[2])
  })
  
  output$dx_params_365 <- reactive({
    params <- get_params_today()
    sprintf("L: %d\np: %d", params$dX[1], params$dX[2])
  })
  
  output$dy_params_365 <- reactive({
    params <- get_params_today()
    sprintf("L: %d\np: %d", params$dY[1], params$dY[2])
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
               uiOutput("tabset_eop")
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
