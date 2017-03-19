library(shiny)
library(shinythemes)

server <- function(input, output, clientData, session) {
  mjd_today <- reactive({
    date.string <- format(Sys.time(), "%Y-%m-%d")
    as.integer(as.Date(date.string) - as.Date("1858-11-17"))
  })
  
  output$mjd <- reactive({
    mjd_today()
  })
  
  output$downloadForecast365 <- downloadHandler(
    filename <- function() {
      paste(mjd_today(), "_ssa_spbu_365.txt", sep="")
    },
    
    content <- function(file) {
      file.copy(paste(mjd_today(), "_ssa_spbu_365.txt", sep=""), file)
    },
    contentType = "application/txt"
  )
  
  output$downloadForecast365 <- downloadHandler(
    filename <- function() {
      paste(mjd_today(), "_ssa_spbu_365.txt", sep="")
    },
    
    content <- function(file) {
      file.copy(paste("ssa/", mjd_today(), "_ssa_spbu_365.txt", sep=""), file)
    },
    contentType = "text/plain"
  )
  
  output$downloadForecast90 <- downloadHandler(
    filename <- function() {
      paste(mjd_today(), "_ssa_spbu_90.txt", sep="")
    },
    
    content <- function(file) {
      file.copy(paste("ssa/", mjd_today(), "_ssa_spbu_90.txt", sep=""), file)
    },
    contentType = "text/plain"
  )
  
  get_ssa_today <- reactive({
    ssa.forecast <- read.table(paste("ssa/", mjd_today(), "_ssa_spbu_365.txt", sep=""))
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

  # check if these files do not exist!
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
    plot(mjd_today():(mjd_today()+364), get_ssa_today()[, "x"], type="l", ylab="x pole", xlab="MJD")
    # start.date <- input$dateCompare
    # axis.Date(1, at=seq(as.Date(start.date), as.Date(start.date) + 365, by = "1 month"), format="%d-%m-%Y", cex.axis=0.7)
  })
  
  output$y_today <- renderPlot({
    plot(mjd_today():(mjd_today()+364), get_ssa_today()[, "y"], type="l", ylab="y pole", xlab="MJD")
  })
  
  output$lod_today <- renderPlot({
    plot(mjd_today():(mjd_today()+364), get_ssa_today()[, "LOD"], type="l", ylab="LOD", xlab="MJD")
  })
  
  output$dx_today <- renderPlot({
    plot(mjd_today():(mjd_today()+364), get_ssa_today()[, "dX"], type="l", ylab="dX", xlab="MJD")
  })
  
  output$dy_today <- renderPlot({
    plot(mjd_today():(mjd_today()+364), get_ssa_today()[, "dY"], type="l", ylab="dX", xlab="MJD")
  })
  
  output$x_comparison <- renderPlot({
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    plot(mjd:(mjd+364), get_final()[(ind):(ind + 364), "x"], type="l", xlab="MJD", ylab="Pole x")
    if("ssa" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_ssa()[1:365, "x"], col="blue", lwd=2)
    }
    if("pul_am" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_pul_am()[1:365, "x"], col="orange")
    }
    if("pul_e1" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_pul_e1()[1:365, "x"], col="green")
    }
    # legend("topright", inset = c(-0.33, 0), legend.names[chosen],
    # col = legend.colors[chosen], lty = 1, bty = 'n', cex = .75, xpd = TRUE)
  })
  
  output$y_comparison <- renderPlot({
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    plot(mjd:(mjd+364), get_final()[(ind):(ind + 364), "y"], type="l", xlab="MJD", ylab="Pole y")
    if("ssa" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_ssa()[1:365, "y"], col="blue", lwd=2)
    }
    if("pul_am" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_pul_am()[1:365, "y"], col="orange")
    }
    if("pul_e1" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_pul_e1()[1:365, "y"], col="green")
    }
    # legend("topright", inset = c(-0.33, 0), legend.names[chosen],
    # col = legend.colors[chosen], lty = 1, bty = 'n', cex = .75, xpd = TRUE)
  })
  
  output$lod_comparison <- renderPlot({
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    plot(mjd:(mjd+364), get_final()[(ind):(ind + 364), "LOD"], type="l", xlab="MJD", ylab="LOD")
    if("ssa" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_ssa()[1:365, "LOD"], col="blue", lwd=2)
    }
    if("pul_am" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_pul_am()[1:365, "LOD"], col="orange")
    }
    if("pul_e1" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_pul_e1()[1:365, "LOD"], col="green")
    }
    # legend("topright", inset = c(-0.33, 0), legend.names[chosen],
    # col = legend.colors[chosen], lty = 1, bty = 'n', cex = .75, xpd = TRUE)
  })
  
  output$dx_comparison <- renderPlot({
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    plot(mjd:(mjd+364), get_final()[(ind):(ind + 364), "dX"], type="l", xlab="MJD", ylab="dX")
    if("ssa" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_ssa()[1:365, "dX"], col="blue", lwd=2)
    }
    if("pul_am" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_pul_am()[1:365, "dX"], col="orange")
    }
    if("pul_e1" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_pul_e1()[1:365, "dX"], col="green")
    }
    # legend("topright", inset = c(-0.33, 0), legend.names[chosen],
    # col = legend.colors[chosen], lty = 1, bty = 'n', cex = .75, xpd = TRUE)
  })
  
  output$dy_comparison <- renderPlot({
    mjd <- get_forecast_mjd()
    ind <- mjd - 37664
    plot(mjd:(mjd+364), get_final()[(ind):(ind + 364), "dY"], type="l", xlab="MJD", ylab="dY")
    if("ssa" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_ssa()[1:365, "x"], col="blue", lwd=2)
    }
    if("pul_am" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_pul_am()[1:365, "dY"], col="orange")
    }
    if("pul_e1" %in% input$displaySeries) {
      lines(mjd:(mjd+364), get_pul_e1()[1:365, "dY"], col="green")
    }
    # legend("topright", inset = c(-0.33, 0), legend.names[chosen],
    # col = legend.colors[chosen], lty = 1, bty = 'n', cex = .75, xpd = TRUE)
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
                           tabPanel("Pole x", {
                             h4("Pole x")
                             plotOutput("x_today")
                           }), 
                           tabPanel("Pole y", {
                             h4("Pole y")
                             plotOutput("y_today")
                           }),
                           tabPanel("LOD", {
                             h4("LOD")
                             plotOutput("lod_today")
                           }),
                           tabPanel("dX", {
                             h4("dX")
                             plotOutput("dx_today")
                           }),
                           tabPanel("dY", {
                             h4("dY")
                             plotOutput("dy_today")
                           })
               )
             )
    ),
    tabPanel("Compare Forecasts",
             sidebarPanel(
               p("Date input is limited between 26.08.2010 and 1.02.2016"),
               tags$hr(),
               dateInput("date_compare", label="Choose starting date", value="2016-02-01",
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
