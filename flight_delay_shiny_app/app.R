# app.R
# 5/18/21
# Allie, Barb, Iver, Do
# edited bug 5/23/21

# PACKAGES
library(shiny)          # run install.packages("package_name")
library(dplyr)          # in console to install missing packages
library(lubridate)
library(stringr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(wesanderson)

# DATA LINK
# link to the original data download website
dataLink <- "https://www.transtats.bts.gov/DL_SelectFields.asp?gnoyr_VQ=FGK"

# READ IN DATA
flNY <- read.csv("files/NY-flights.csv")


# FORMAT COLUMNS
flNY$date <- ymd(flNY$date)

monthCat <- c("March", "April")
flNY$month.name <- factor(flNY$month.name, levels=monthCat)

weekdaysCat <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                 "Friday", "Saturday", "Sunday")
flNY$weekday.name <- factor(flNY$weekday.name, levels=weekdaysCat)

flNY$network_airline <- factor(flNY$network_airline)
flNY$operating_airline <- factor(flNY$operating_airline)

delayCat <- c("Early", "On Time", "Delay 0:15 to 0:29", "Delay 0:30 to 0:44",
              "Delay 0:45 to 0:59", "Delay 1:00 to 1:14", "Delay 1:14 to 1:29", 
              "Delay 1:30 to 1:44", "Delay 1:45 to 1:59", "Delay 2:00 to 2:14",
              "Delay 2:15 to 2:29", "Delay 2:30 to 2:44", "Delay 2:45 to 2:59",
              "Delay 3:00 and above", "Cancelled or Diverted")
flNY$dep_delay.info <- factor(flNY$dep_delay.info, levels=delayCat, ordered=TRUE)
flNY$arr_delay.info <- factor(flNY$arr_delay.info, levels=delayCat, ordered=TRUE)


# GENERATE OTHER VARIABLES
# dates
startDate20 <- min(flNY$date) + years(1)
endDate20 <- max(flNY$date)

# airline lookup
lookup_carrier <- read.csv("files/L_UNIQUE_CARRIERS.csv_")
getAirline <- lookup_carrier$Description
names(getAirline) <- lookup_carrier$Code

# list of network airlines
netAirlines.code <- levels(flNY$network_airline)
netAirlines.name <- getAirline[netAirlines.code]

# list of operating airlines
opAirlines.code <- levels(flNY$operating_airline)
opAirlines.name <- getAirline[opAirlines.code]

# list of NY airports
airports.code <- c("EWR", "JFK", "LGA")
airports.name <- c("Newark Liberty International", 
                   "John F. Kennedy International", "LaGuardia")
airports <- paste(airports.code, "-", airports.name)

# delay causes list
delayCauses <- c("Carrier", "Weather", "National Air System", 
                 "Security", "Late Aircraft")
delayCols <- c("carrier_delay", "weather_delay", "nas_delay",
               "security_delay", "late_aircraft_delay")


# FORMAT COLUMNS (MORE)
flNY$origin <- factor(flNY$origin, levels=airports.code)
flNY$dest <- factor(flNY$dest, levels=airports.code)
flNY$origin.name <- factor(flNY$origin.name, levels=airports.name)
flNY$dest.name <- factor(flNY$dest.name, levels=airports.name)
flNY$network_airline.name <- factor(flNY$network_airline.name, 
                                    levels=netAirlines.name)
flNY$operating_airline.name <- factor(flNY$operating_airline.name,
                                      levels=opAirlines.name)



### CLIENT SIDE ###------------------------------------------------------------

ui <- fluidPage(
  titlePanel("COVID-19 Impact on Flight Lateness in New York"),
  sidebarLayout(
    # input panel
    sidebarPanel(
      # flight direction input
      radioGroupButtons(inputId="flightDir", label="Inbound/Outbound:",
                        choiceNames=c("Inbound - Arrival Delay", 
                                      "Outbound - Departure Delay"),
                        choiceValues = c("Inbound", "Outbound"), 
                        selected = "Inbound", direction = "vertical",
                        justified = TRUE,
                        checkIcon = list(yes=icon("ok", lib="glyphicon"),
                                         no=icon("ok", lib="glyphicon", 
                                                 style="visibility: hidden"))),
      br(),
      # date input
      sliderInput(inputId="dateRange", label="Date Range:", min=startDate20, 
                  max=endDate20, value=c(startDate20, endDate20),
                  timeFormat="%b %e"),
      br(),
      # airport input
      checkboxGroupButtons(inputId="airport", label="Airports:", 
                           choiceNames=airports, choiceValues=airports.code,
                           selected=airports.code, direction="vertical",
                           justified=TRUE,
                           checkIcon=list(yes=icon("ok", lib="glyphicon"),
                                          no=icon("ok", lib="glyphicon",
                                                  style="visibility: hidden"))),
      br(),
      # network airline input
      pickerInput(inputId="networkAirline", label="Network Airlines:", 
                  choices=netAirlines.code, selected=netAirlines.code,
                  choicesOpt=list(subtext=netAirlines.name),
                  options=list(`actions-box`=TRUE, size=10), multiple=TRUE),
      br(),
      # operating airline input
      pickerInput(inputId="operatingAirline", label="Operating Airlines:", 
                  choices=opAirlines.code, selected=opAirlines.code,
                  choicesOpt=list(subtext=opAirlines.name),
                  options=list(`actions-box`=TRUE, size=10), multiple=TRUE),
      br(),
      # exact delay time input
      sliderInput(inputId="minlate", label="Delay Length in Minutes:", 
                  min=1, max=300, value=c(1,300)),
      # include extra late flights and/or cancelled flights
      checkboxGroupButtons(inputId="extraDelay", 
                             choiceNames=c("Include flights delayed over 300 minutes",
                                           "Include cancelled or diverted flights"), 
                             choiceValues=c("extraLate", "cancDiv"),
                             direction="vertical", justified=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"),
                                            no=icon("ok", lib="glyphicon",
                                                    style="visibility: hidden"))),
      br(),
      # delay cause input
      conditionalPanel(
        condition="input.flightDir == \"Inbound\"",
        checkboxGroupButtons(inputId="delayCause", label="Causes for Delay:",
                             choiceNames=delayCauses, choiceValues=delayCols,
                             selected=delayCols, direction="vertical", 
                             justified=TRUE,
                             checkIcon=list(yes=icon("ok", lib="glyphicon"),
                                            no=icon("ok", lib="glyphicon",
                                                    style="visibility: hidden"))),
        br()
      ),
      # highlight category input
      pickerInput(inputId="graphCat",label="Highlight Categorical Variable:",
                  choices=c("Airport", "Network Airline", "Operating Airline",
                            "Weekday", "Month", "Delay Group", "None"),
                  selected="Network Airline")
    ),
    # Main panel
    mainPanel(
      # Tabs
      tabsetPanel(
        type="tabs",
        id="tab",
        tabPanel(
          title="Flights by Date",
          value=1,
          plotlyOutput("plot1a"),
          br(),
          plotlyOutput("plot2a"),
          br()
        ),
        tabPanel(
          title="Flights by Minutes Late",
          value=2,
          plotlyOutput("plot3a"),
          conditionalPanel(
            condition="input.graphCat!=\"None\" & input.graphCat!=\"Delay Group\"",
            br(),
            plotlyOutput("plot4a")
          ),
          br()
        ),
        tabPanel(
          title="Flights by Year: 2019 vs. 2020",
          value=3,
          plotlyOutput("plot1b"),
          br(),
          plotlyOutput("plot2b"),
          br(),
          plotlyOutput("plot3b"),
          conditionalPanel(
            condition="input.graphCat!=\"None\" & input.graphCat!=\"Delay Group\"",
            br(),
            plotlyOutput("plot4b")
          ),
          br()
        )
      )
    )
  ),
  # footer
  hr(),
  fluidRow(
    tags$p("Created by Allie Klump, Barb Garrison, Do Lee, & Iver Warburton",
           style="text-indent: 12px"),
    tags$p("Data from the Bureau of Transportation Statistics: ", 
           tags$a(href=dataLink, dataLink, target="_blank", .noWS="outside"), 
           .noWS = c("after-begin", "before-end"),
           style="text-indent: 12px")
  )
)



### SERVER SIDE ###------------------------------------------------------------

server <- function(input, output) {
  
  
  
  # FILTERS
  # filter inbound/outbound arrival/departure delay
  flNYfilt1 <- reactive({
    if(input$flightDir == "Inbound") {
      flNY %>%
        filter(dest %in% input$airport) %>%
        mutate(airport = dest,
               airport.name = dest.name,
               delay.c = arr_delay.c,
               delay.info = arr_delay.info)
    } else {
      flNY %>%
        filter(origin %in% input$airport) %>%
        mutate(airport = origin,
               airport.name = origin.name,
               delay.c = dep_delay.c,
               delay.info = dep_delay.info)
    }
  })
  
  # filer airlines
  flNYfilt2 <- reactive({
    flNYfilt1() %>%
      filter(network_airline %in% input$networkAirline) %>%
      filter(operating_airline %in% input$operatingAirline)
  })
  
  # filter dates
  flNYfilt3 <- reactive({
    if(input$tab == 3) {
      flNYfilt2() %>%
        filter(date %within% interval(input$dateRange[1], input$dateRange[2]) | 
                 date %within% interval(input$dateRange[1]-years(1),
                                        input$dateRange[2]-years(1)))
    } else {
      flNYfilt2() %>%
        filter(date %within% interval(input$dateRange[1], input$dateRange[2]))
    }
  })
  
  # filter delay
  flNYfilt4 <- reactive({
    df <- flNYfilt3()
    if("extraLate" %in% input$extraDelay) {
      if("cancDiv" %in% input$extraDelay) {
        # include extraLate and cancDiv
        df <- df %>%
          filter(delay.c >= input$minlate[1] | is.na(delay.c))
      } else {
        # include extraLate only
        df <- df %>%
          filter(delay.c >= input$minlate[1])
      }
    } else if("cancDiv" %in% input$extraDelay) {
      # include cancDiv only
      df <- df %>%
        filter(delay.c >= input$minlate[1] & delay.c <= input$minlate[2] |
                 is.na(delay.c))
    } else {
      # include neither, just normal delay filtering
      df <- df %>%
        filter(delay.c >= input$minlate[1], delay.c <= input$minlate[2])
    }
    if(input$flightDir == "Inbound" & length(input$delayCause) < 5) {
      if(length(input$delayCause) > 0) {
        delayLogic <- paste0(str_c(input$delayCause, collapse=" > 0 | "), " > 0")
        if("cancDiv" %in% input$extraDelay) {
          delayLogic <- paste0(delayLogic, " | is.na(delay.c)")
        }
        eval(parse(text=paste0("df %>% filter(", delayLogic, ")")))
      } else {
        df[FALSE,]
      }
    } else {
      df
    }
  })
  
  # VARIABLES
  
  # theme
  ggTheme <- theme(plot.title=element_text(size=15,face="bold"),
                   axis.title=element_text(size=12,face="bold"),
                   axis.text=element_text(size=10),
                   strip.text=element_text(size=14,face="bold"))
  # color palettes
  cAirport <- wes_palette(3, name="Darjeeling1", type="discrete")
  cNetAirline <- colorRampPalette(wes_palette(5, name="Darjeeling1", 
                                              type="discrete"))(10)
  cOpAirline <- colorRampPalette(wes_palette(5, name="Darjeeling1", 
                                             type="discrete"))(21)
  cWeekday <- colorRampPalette(wes_palette(5, name="Darjeeling1", 
                                           type="discrete"))(7)
  cMonth <- wes_palette(2, name="Darjeeling1", type="discrete")
  
  
  # category
  colorCat <- reactive({
    input$graphCat
  })
  
  # graph title text variables
  yearText <- reactive({
    if(input$tab == 3) {
      "2019 vs. 2020"
    } else {
      "2020"
    }
  })
  delayText <- reactive({
    if(input$flightDir == "Inbound") {
      "Arrival Delay"
    } else {
      "Departure Delay"
    }
  })
  numFlText <- "Number of Flights"
  titleText1 <- reactive({
    c(paste(numFlText, "over Time -", yearText()),
      "Date", numFlText)
  })
  titleText2 <- reactive({
    c(paste("Number of Delayed Flights over Time -", yearText()),
      "Date", numFlText)
  })
  titleText3 <- reactive({
    c(paste("Histogram of", delayText(), "-", yearText()), 
      paste(delayText(), "(minutes)"), numFlText)
  })
  titleText4 <- reactive({
    c(paste("Proportion of Delayed Flights by", colorCat(), "-", yearText()), 
      colorCat(), "Proportion of Flights")
  })
  
  
  # GRAPH REACTIVES
  
  ggBase1 <- reactive({
    flNYfilt3 () %>%
      ggplot() +
      labs(title=titleText1()[1], x=titleText1()[2], y=titleText2()[3]) +
      ggTheme
  })
  ggBase2 <- reactive({
    flNYfilt4 () %>%
      ggplot() +
      labs(title=titleText2()[1], x=titleText2()[2], y=titleText2()[3]) +
      ggTheme
  })
  ggBase3 <- reactive({
    flNYfilt4 () %>%
      ggplot() +
      labs(title=titleText3()[1], x=titleText3()[2], y=titleText3()[3]) +
      ggTheme
  })
  ggBase4 <- reactive({
    flNYfilt4 () %>%
      ggplot() +
      labs(title=titleText4()[1], x=titleText4()[2], y=titleText4()[3]) +
      ggTheme + scale_fill_ordinal(drop=FALSE) + scale_x_discrete(drop=FALSE)
  })
  
  ggPlot1 <- reactive({
    if(colorCat() == "Airport") {
      ggBase1() + geom_histogram(aes(x=date, fill=airport)) +
        scale_fill_manual(values=cAirport, drop=FALSE)
    } else if(colorCat() == "Network Airline") {
      ggBase1() + geom_histogram(aes(x=date, fill=network_airline)) +
        scale_fill_manual(values=cNetAirline, drop=FALSE)
    } else if(colorCat() == "Operating Airline") {
      ggBase1() + geom_histogram(aes(x=date, fill=operating_airline)) +
        scale_fill_manual(values=cOpAirline, drop=FALSE)
    } else if(colorCat() == "Weekday") {
      ggBase1() + geom_histogram(aes(x=date, fill=weekday.name)) +
        scale_fill_manual(values=cWeekday, drop=FALSE)
    } else if(colorCat() == "Month") {
      ggBase1() + geom_histogram(aes(x=date, fill=month.name)) +
        scale_fill_manual(values=cMonth, drop=FALSE)
    } else if(colorCat() == "Delay Group") {
      ggBase1() + geom_histogram(aes(x=date, fill=delay.info)) +
        scale_fill_ordinal(drop=FALSE)
    } else { # colorCat() == "None"
      ggBase1() + geom_histogram(aes(x=date))
    }
  })
  
  ggPlot2 <- reactive({
    if(colorCat() == "Airport") {
      ggBase2() + geom_histogram(aes(x=date, fill=airport)) +
        scale_fill_manual(values=cAirport, drop=FALSE)
    } else if(colorCat() == "Network Airline") {
      ggBase2() + geom_histogram(aes(x=date, fill=network_airline)) +
        scale_fill_manual(values=cNetAirline, drop=FALSE)
    } else if(colorCat() == "Operating Airline") {
      ggBase2() + geom_histogram(aes(x=date, fill=operating_airline)) +
        scale_fill_manual(values=cOpAirline, drop=FALSE)
    } else if(colorCat() == "Weekday") {
      ggBase2() + geom_histogram(aes(x=date, fill=weekday.name)) +
        scale_fill_manual(values=cWeekday, drop=FALSE)
    } else if(colorCat() == "Month") {
      ggBase2() + geom_histogram(aes(x=date, fill=month.name)) +
        scale_fill_manual(values=cMonth, drop=FALSE)
    } else if(colorCat() == "Delay Group") {
      ggBase2() + geom_histogram(aes(x=date, fill=delay.info)) +
        scale_fill_ordinal(drop=FALSE)
    } else { # colorCat() == "None"
      ggBase2() + geom_histogram(aes(x=date))
    }
  })
  
  ggPlot3 <- reactive({
    if(colorCat() == "Airport") {
      ggBase3() + geom_histogram(aes(x=delay.c, fill=airport)) +
        scale_fill_manual(values=cAirport, drop=FALSE)
    } else if(colorCat() == "Network Airline") {
      ggBase3() + geom_histogram(aes(x=delay.c, fill=network_airline)) +
        scale_fill_manual(values=cNetAirline, drop=FALSE)
    } else if(colorCat() == "Operating Airline") {
      ggBase3() + geom_histogram(aes(x=delay.c, fill=operating_airline)) +
        scale_fill_manual(values=cOpAirline, drop=FALSE)
    } else if(colorCat() == "Weekday") {
      ggBase3() + geom_histogram(aes(x=delay.c, fill=weekday.name)) +
        scale_fill_manual(values=cWeekday, drop=FALSE)
    } else if(colorCat() == "Month") {
      ggBase3() + geom_histogram(aes(x=delay.c, fill=month.name)) +
        scale_fill_manual(values=cMonth, drop=FALSE)
    } else if(colorCat() == "Delay Group") {
      ggBase3() + geom_histogram(aes(x=delay.c, fill=delay.info)) +
        scale_fill_ordinal(drop=FALSE)
    } else { # colorCat() == "None"
      ggBase3() + geom_histogram(aes(x=delay.c))
    }
  })

  ggPlot4 <- reactive({
    if(colorCat() == "Airport") {
      ggBase4() + 
        geom_bar(aes(x=airport, fill=delay.info), position="fill")
    } else if(colorCat() == "Network Airline") {
      ggBase4() + 
        geom_bar(aes(x=network_airline, fill=delay.info), position="fill")
    } else if(colorCat() == "Operating Airline") {
      ggBase4() + 
        geom_bar(aes(x=operating_airline, fill=delay.info), position="fill")
    } else if(colorCat() == "Weekday") {
      ggBase4() + 
        geom_bar(aes(x=weekday.name, fill=delay.info), position="fill")
    } else if(colorCat() == "Month") {
      ggBase4() + 
        geom_bar(aes(x=month.name, fill=delay.info), position="fill")
    } else { # colorCat() == "None" or "Delay Group"
      #we shouldn't ever get here
      ggBase4() + geom_bar(aes(x=month, fill=delay.info), position="fill")
    }
  })
  
  
  # GRAPH RENDERS
  
  # 2020
  # tab 1
  output$plot1a <- renderPlotly({
    ggplotly(ggPlot1())
  })
  
  output$plot2a <- renderPlotly({
    ggplotly(ggPlot2())
  })
  
  # tab 2
  output$plot3a <- renderPlotly({
    ggplotly(ggPlot3())
  })
  
  output$plot4a <- renderPlotly({
    ggplotly(ggPlot4())
  })
  
  # 2019 vs 2020 - tab 3
  output$plot1b <- renderPlotly({
    gg <- ggPlot1() + facet_wrap(~ year, scales="free_x")
    ggplotly(gg)
  })
  
  output$plot2b <- renderPlotly({
    gg <- ggPlot2() + facet_wrap(~ year, scales="free_x")
    ggplotly(gg)
  })
  
  output$plot3b <- renderPlotly({
    gg <- ggPlot3() + facet_wrap(~ year)
    ggplotly(gg)
  })
  
  output$plot4b <- renderPlotly({
    gg <- ggPlot4() + facet_wrap(~ year)
    ggplotly(gg)
  })
  
}

shinyApp(ui, server)

