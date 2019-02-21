# Mirko Mantovani - Ashwani Khemani - Abhishek Vasudevan - 02/20/2019

# libraries
library(shiny)
library(devtools)
library(ggplot2)
library(shinydashboard)
library(scales) # needed for percent function
library(shinythemes) # themes for bootstrapPage, fluidPage, navbarPage, or fixedPage
library(dashboardthemes)
library(ggthemes) 
library(shinyalert)
library(leaflet)
library(rgdal)
library(geojson)
library(geojsonio)
library(colourpicker)
library(shinyWidgets)

# importing datasets
temp = list.files(pattern="*.csv")
datasets = lapply(temp, read.csv)
dataset <- do.call(rbind, datasets)

# needed for counties coordinates
sites <- read.table(file = "sites/aqs_sites.csv", sep=",",header = TRUE)

# geojson file for counties shape
xy <- geojsonio::geojson_read("gz_2010_us_050_00_20m.json", what = "sp")



########################################### PREPROCESSING #########################################

years<-c(1980:2018)
states<-unique(dataset$State)
t<-subset(dataset, State == 'Illinois')
counties<-unique(t$County)

pollutants <- c("CO","NO2","Ozone","SO2","PM2.5","PM10")
statistics <- c("Median","Max","90th percentile")


# All counties with state
all_counties <- c()
for(s in states){
  coun <- subset(dataset, State == s)
  counti<-unique(coun$County)
  counti <- paste(counti,"-",s)
  all_counties <- c(all_counties,counti)
}


############################################### UI ################################################

ui <- dashboardPage(
  dashboardHeader(
    title = "Visual Analytics - Just Breathe",
    titleWidth = 300 
  ),
  dashboardSidebar(disable = FALSE, collapsed = TRUE,
                   width = 300, 
                   sidebarMenu(
                     useShinyalert(),
                     menuItem("Year details for County", tabName = "pie"),
                     menuItem("Yearly trends", tabName = "time"),
                     # menuItem("Compare Counties", tabName = "compare"),
                     menuItem("Monthly AQI level", tabName = "monthly_aqi"),
                     menuItem("Daily AQI", tabName = "daily_aqi"),
                     menuItem("Hourly Pollutants", tabName = "hourly_pollutants"),
                     menuItem("Pollutants map", tabName = "pollutants_map"),
                     menuItem("About", tabName = "about")
                   ),
                   # custom CSS 
                   includeCSS("style.css"),
                   selectInput(inputId = "State", "Select State", states, selected = 'Illinois',width = "200%"),
                   tags$style("#County {background-color:blue;}"),
                   selectInput("County", "Select County", counties, selected = 'Adams',width = "200%"),
                   div(id="nozoom",sliderInput(inputId = "Year", 
                                               sep = "",
                                               label = "Select Year", 
                                               value = 2018, min = 1980, max = 2018,width = "90%"))
                   ),
  dashboardBody(tags$head(
    # Include custom JS
    #includeCSS("styles.css"),
    includeScript("sage2responsive.js")
    ),
    shinyDashboardThemes(
      # Blue theme mainly for sidebar
      theme = "blue_gradient"
    ),
    # content of each main tab (selectable from sidebar)
    tabItems(
      # FIRST MENU TAB
      tabItem("pie",
              fluidRow(
                column(6, 
                       box(title = "AQI levels", width = NULL,status = "primary",
                           fluidRow(column(8,
                                           box(title = "Percentage of AQI level", width = NULL,status = "primary",div(plotOutput("aqi_pie", height = "42vmin")))),
                                    column(4,
                                           textOutput("missing_data"))),
                           plotOutput("aqi_bar", height = "30vmin"),
                           div(DT::dataTableOutput("aqi_table"), style = "font-size:80%")
                       )
                ),
                column(6, 
                       box(title = "Pollutants",status = "primary", width = NULL, 
                           tabsetPanel(
                             tabPanel("Percentage of days as main Pollutant",
                                      fluidRow(column(4,plotOutput("co_pie", height = "38vmin")),column(4,plotOutput("no2_pie", height = "38vmin")),column(4,plotOutput("ozone_pie", height = "38vmin"))),
                                      fluidRow(column(4,plotOutput("so2_pie", height = "38vmin")),column(4,plotOutput("pm25_pie", height = "38vmin")),column(4,plotOutput("pm10_pie", height = "38vmin")))
                             ),
                             tabPanel("Bar chart", plotOutput("pollutants_bar", height = "76vmin"))
                           ),
                           div(DT::dataTableOutput("pollutants_table"), style = "font-size:80%")
                           
                       )
                )
              )
      ),
      
      # SECOND MENU TAB
      tabItem("time",
              fluidRow(
                # Input county with search
                column(2,box(title = "County Selection and customization",status = "success", width = NULL,
                             div(column(12, 
                                        
                                        dropdownButton(
                                          tags$h3("Other colors"),
                                          colourInput("colorCO", h5("Select color CO"), value = "#c6c60f"),
                                          colourInput("colorNO2", h5("Select color NO2"), value = "#13c649"),
                                          colourInput("colorOZONE", h5("Select color Ozone"), value = "#0fa2af"),
                                          colourInput("colorSO2", h5("Select color SO2"), value = "#5610a8"),
                                          colourInput("colorPM25", h5("Select color PM2.5"), value = "#cc8112"),
                                          colourInput("colorPM10", h5("Select color PM10"), value = "#ba1010"),
                                          circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                                          tooltip = tooltipOptions(title = "Click to open")
                                        ),
                                        colourInput("backgroundColor", h3("Select color"), value = "#005669"),
                                        checkboxGroupButtons(
                                          inputId = "textColor", label = h5("Text and Grid color"), # moved in main input panel 
                                          choices = c("white", "black"), 
                                          justified = TRUE, status = "primary", selected = "white",
                                          checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
                                        ),
                                        selectizeInput("CountySearch", label = h4("Search County"), sort(all_counties), selected = NULL, multiple = FALSE, options = NULL),
                                        h3("State:"),
                                        h4(textOutput("sel_state")),
                                        h3("County:"),
                                        h4(textOutput("sel_county")),
                                        h3("Data:"),
                                        h6(textOutput("data_years")),
                                        h6(textOutput("data_days")),
                                        div(id="nozoomslider",ticks = FALSE, sliderInput("range", sep = "", label = "Select Year range", min = 1980, 
                                                                                         max = 2018, value = c(1980, 2018))
                                        )
                                        
                             ),class = "boxtozoom")
                )
                ),
                # 2 tabs, (line plots and table, map)
                column(10,
                       tabsetPanel(
                         tabPanel("AQI Time Series",
                                  plotOutput("aqi_time", height = "85vmin")
                         ),
                         tabPanel("Pollutants Percentage Time Series",
                                  tabsetPanel(
                                    tabPanel("Line Plot",
                                             plotOutput("pollutants_time", height = "80vmin")
                                    ),
                                    tabPanel("Table",
                                             div(DT::dataTableOutput("pollutants_time_table"), style = "font-size:90%")
                                    )
                                  )
                         ),
                         tabPanel("Map",
                                  leafletOutput("map_county")
                         )
                       )
                )
              )
      ),
      
      # THIRD MENU TAB
      # tabItem("compare",
      #         fluidRow(
      #           # Input county with search
      #           column(2,box(title = "Counties Selection",status = "success", width = NULL,
      #                        div(column(12, 
      #                                   dropdownButton(
      #                                     tags$h3("Select inputs to visualize"),
      #                                     selectInput(inputId = "Statistic", h5("Select AQI statistic"), statistics, selected = 'Median',width = "200%"),
      #                                     selectInput(inputId = "Pollutant", h5("Select pollutant"), pollutants, selected = 'CO',width = "200%"),
      #                                     # selectInput(inputId = "State", "Select pollutant", pollutants, selected = 'CO',width = "200%"),
      #                                     circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
      #                                     tooltip = tooltipOptions(title = "Click to open")
      #                                   ),
      #                                   fluidRow(selectizeInput("SelCounty1", label = h4("Search County 1"), sort(all_counties), selected = NULL, multiple = FALSE,options = NULL)),
      #                                   fluidRow(selectizeInput("SelCounty2", label = h4("Search County 2"), sort(all_counties), selected = NULL, multiple = FALSE,options = NULL)),
      #                                   fluidRow(selectizeInput("SelCounty3", label = h4("Search County 3"), sort(all_counties), selected = NULL, multiple = FALSE,options = NULL))
      #                        ),class = "boxtozoom")
      #           ),
      #           box(title = "Counties location",status = "success", width = NULL,
      #               leafletOutput("map_counties"))
      #           ),
      #           column(10,
      #                  tabsetPanel(
      #                    tabPanel("AQI Time Series",
      #                             plotOutput("aqi_time_comp", height = "85vmin")
      #                    ),
      #                    tabPanel("Pollutants Percentage Time Series",
      #                             plotOutput("pollutants_time_comp", height = "85vmin")
      #                    ),
      #                    
      #                    tabPanel("Days as main pollutant in specific year",
      #                             plotOutput("pollutants_bar_comp", height = "85vmin")
      #                    )
      #                  )
      #           )
      #           
      #           
      #         )),
      
      # FOURTH MENU TAB
      tabItem("about",
              htmlOutput("about_out")
      )
      
      
      # Finish tabs
    )
    )
  
  )





############################################# SERVER ##############################################

server <- function(input, output, session) {
  
  # customizing values for responsitivity in normal display and SAGE display
  v <- reactiveValues(axis_title_size = 14, 
                      axis_text_size = 12,
                      margin_y = 30,
                      margin_x = 0,
                      legend_text_size = 5,
                      legend_title_size = 5,
                      legend_key_size = 1,
                      pie_text_size = 5,
                      slant_text_angle = 45,
                      point_size = 1,
                      zoom_level = 6,
                      tooltip_width = 100,
                      tooltip_hieght = 60,
                      tooltip_text_size = 14,
                      line_size = 1,
                      tbl_pagelength = 20,
                      annotate_text_size = 4,
                      marker_text_size = '12px',
                      select_input_width = '100%'
  )
  
  observeEvent(input$dimension, {
    if(input$dimension[1] >= 2000){
      v$axis_title_size <<- 40
      v$axis_text_size <<- 40
      v$margin_y <<- 40
      v$margin_x <<- 40
      v$legend_title_size <<- 40
      v$legend_text_size <<- 40
      v$legend_key_size <<- 8
      v$pie_text_size <<- 15
      v$slant_text_angle <<- 0
      v$point_size <<- 4
      v$zoom_level <<- 9
      v$tooltip_width <<- 180
      v$tooltip_height <<- 80
      v$tooltip_text_size <<- 28
      v$line_size <<- 5
      v$tbl_pagelength <<- 20
      v$annotate_text_size <<- 8
      v$marker_text_size <<- '60px'
      v$select_input_width <<- '200%'
    } else {
      v$axis_title_size = 14 
      v$axis_text_size = 12
      v$margin_y = 45
      v$margin_x = 0
      v$legend_text_size = 10
      v$legend_title_size = 10
      v$legend_key_size = 2
      v$pie_text_size = 5
      v$slant_text_angle = 45
      v$point_size = 1
      v$zoom_level = 8
      v$tooltip_width = 100
      v$tooltip_hieght = 60
      v$tooltip_text_size = 14
      v$line_size = 1
      v$tbl_pagelength = 20
      v$annotate_text_size = 4
      v$marker_text_size = '12px'
      v$select_input_width = '100%'
    }
  })
  
  axis_title_size <- reactive({v$axis_title_size})
  axis_text_size <- reactive({v$axis_text_size})
  margin_x <- reactive({v$margin_x})
  margin_y <- reactive({v$margin_y})
  legend_text_size <- reactive({v$legend_text_size})
  legend_key_size <- reactive({v$legend_key_size})
  legend_title_size <- reactive({v$legend_title_size})
  pie_text_size <- reactive({v$pie_text_size})
  slant_text_angle <- reactive({v$slant_text_angle})
  point_size <- reactive({v$point_size})
  zoom_level <- reactive({v$zoom_level})
  tooltip_width <- reactive({v$tooltip_width})
  tooltip_height <- reactive({v$tooltip_height})
  tooltip_text_size <- reactive({v$tooltip_text_size})
  line_size <- reactive({v$line_size})
  tbl_pagelength <- reactive({v$tbl_pagelength})
  annotate_text_size <- reactive({v$annotate_text_size})
  
  marker_text_size <- reactive({v$marker_text_size})
  select_input_width <- reactive({v$select_input_width})
  
  output$dimension_display <- renderText({
    paste(input$dimension[1], input$dimension[2], input$dimension[1]/input$dimension[2])
  })
  

  # computing subset of data based on user selection of year, state, county
  current <- reactive({
    # print("reactive")
    subset(dataset, County == input$County & State == input$State & Year == input$Year)
    
  })
  
  observeEvent(priority = 10,input$State,{
    selected_state_data <- subset(dataset, State == input$State)
    counties_in_state <- unique(selected_state_data$County)
    
    updateSelectInput(session, inputId = "County", choices = counties_in_state)
    county <- input$County
    
  })
  
  selected_state <- reactive({
    strsplit(input$CountySearch," - ")[[1]][2]
  })
  
  selected_county <- reactive({
    strsplit(input$CountySearch," - ")[[1]][1]
  })
  
  selected_state1 <- reactive({
    strsplit(input$SelCounty1," - ")[[1]][2]
  })
  
  selected_county1 <- reactive({
    strsplit(input$SelCounty1," - ")[[1]][1]
  })
  
  selected_state2 <- reactive({
    strsplit(input$SelCounty2," - ")[[1]][2]
    # }
  })
  
  selected_county2 <- reactive({
    strsplit(input$SelCounty2," - ")[[1]][1]
  })
  
  selected_state3 <- reactive({
    strsplit(input$SelCounty3," - ")[[1]][2]
    # }
  })
  
  selected_county3 <- reactive({
    strsplit(input$SelCounty3," - ")[[1]][1]
  })
  
  # pie chart of aqi
  output$aqi_pie <- renderPlot({
    c<-subset(dataset, County == input$County & State == isolate(input$State) & Year == input$Year)
    if(length(c$State) == 1){
      
      df <- data.frame(
        
        group = c("Percentage of Good Days", "Percentage of Moderate Days", "Percentage of Unhealthy for Sensitive Groups Days", "Percentage of Very Unhealthy Days", "Percentage of Hazardous Days"),
        value = c(isolate(current())$Good.Days/isolate(current())$Days.with.AQI*100, isolate(current())$Moderate.Days/isolate(current())$Days.with.AQI*100, 
                  isolate(current())$Unhealthy.for.Sensitive.Groups.Days/isolate(current())$Days.with.AQI*100,
                  isolate(current())$Very.Unhealthy.Days/isolate(current())$Days.with.AQI*100,
                  isolate(current())$Hazardous.Days/isolate(current())$Days.with.AQI*100)
      )
      
      df$group <- factor(df$group, levels = c("Percentage of Good Days", "Percentage of Moderate Days", "Percentage of Unhealthy for Sensitive Groups Days", "Percentage of Very Unhealthy Days", "Percentage of Hazardous Days"))
      
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + #theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_brewer(palette="Greys","AQI Level") +
        theme(
          axis.title.x = element_blank(),
          plot.background = element_rect(color = NA, fill = "#bcdae0"),
          legend.background = element_rect(color = NA, fill = "#bcdae0"),
          panel.background = element_rect(fill = "#bcdae0", color  =  NA),
          strip.background = element_rect(fill = "#bcdae0", color = "#bcdae0"),
          plot.margin=grid::unit(c(margin_y(),margin_x()+8.5,margin_y(),margin_x()+8.5), "mm"),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          legend.text = element_text(size = legend_text_size()), 
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size()),
          axis.title = element_text(size = axis_title_size()),
          legend.title = element_text(size = legend_title_size())
        ) 
      
      pie
    }
    # Signaling missing data
    else {
      shinyalert("Oops!", "No data for this County in this Year", type = "error")
    }
  })
  
  # bar chart of aqi
  output$aqi_bar <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        
        group = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Very Unhealthy", "Hazardous"),
        value = c(current()$Good.Days, current()$Moderate.Days, 
                  current()$Unhealthy.for.Sensitive.Groups.Days,
                  current()$Very.Unhealthy.Days,
                  current()$Hazardous.Days)
      )
      
      df$group <- factor(df$group, levels = c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Very Unhealthy", "Hazardous"))
      
      
      bar <-ggplot(data=df, aes(x=group, y=value, fill = group)) + scale_fill_brewer(palette="Greys") +
        geom_bar(stat="identity") + coord_flip() + 
        theme(
          text = element_text(size=12),
          legend.position="none"
        )+ 
        xlab("AQI level") + ylab("Days count")+
        theme(
          axis.title.x = element_text(color = "black"),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(color = NA, fill = "#bcdae0"),
          legend.background = element_rect(color = NA, fill = "#bcdae0"),
          panel.background = element_rect(fill = "#bcdae0", color  =  NA),
          panel.grid.major = element_line(color = "black"),  
          panel.grid.minor = element_line(color = "black"),
          legend.text = element_text(size = legend_text_size()), 
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size(),color = "black"),
          axis.title = element_text(size = axis_title_size()),
          legend.title = element_text(size = legend_title_size())
        )
      bar
    }
  })
  
  # table of aqi
  output$aqi_table <- DT::renderDataTable(current()[, c('Good.Days', 'Moderate.Days',"Unhealthy.for.Sensitive.Groups.Days", "Very.Unhealthy.Days", "Hazardous.Days")],
                                          rownames = FALSE,
                                          colnames = c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 'Very Unhealthy','Hazardous'), 
                                          options = list(searching = FALSE,paging = FALSE,
                                                         dom = 't'
                                          ))
  # pie chart of CO
  output$co_pie <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without CO","Days with CO"),
        value = c((current()$Days.with.AQI-current()$Days.CO)/current()$Days.with.AQI*100,current()$Days.CO/current()$Days.with.AQI*100)
      )
      
      df$group <- factor(df$group, levels = c("Days without CO","Days with CO"))
      
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_manual(values=c("#efefba", "#d6d600")) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          plot.margin=grid::unit(c(margin_y()+0.22,margin_x(),margin_y()+0.22,margin_x()), "mm"),
          plot.background = element_rect(color = NA, fill = "#bcdae0"),
          legend.background = element_rect(color = NA, fill = "#bcdae0"),
          panel.background = element_rect(fill = "#bcdae0", color  =  NA),
          strip.background = element_rect(fill = "#bcdae0", color = "#bcdae0"),
          legend.text = element_text(size = legend_text_size()), 
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size()),
          axis.title = element_text(size = axis_title_size()),
          legend.title = element_text(size = legend_title_size())
        ) 
      pie
    }
  })
  
  # pie chart of NO2
  output$no2_pie <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without NO2", "Days NO2"),
        value = c((current()$Days.with.AQI-current()$Days.NO2)/current()$Days.with.AQI*100, current()$Days.NO2/current()$Days.with.AQI*100)
      )
      
      df$group <- factor(df$group, levels = c("Days without NO2","Days NO2"))
      
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_manual(values=c("#bee5ca", "#03c63e")) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          plot.margin=grid::unit(c(margin_y()+1.1,margin_x(),margin_y()+1.1,margin_x()), "mm"),
          plot.background = element_rect(color = NA, fill = "#bcdae0"),
          legend.background = element_rect(color = NA, fill = "#bcdae0"),
          panel.background = element_rect(fill = "#bcdae0", color  =  NA),
          strip.background = element_rect(fill = "#bcdae0", color = "#bcdae0"),
          legend.text = element_text(size = legend_text_size()), 
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size()),
          axis.title = element_text(size = axis_title_size()),
          legend.title = element_text(size = legend_title_size())
        )
      pie
    }
  })
  
  # pie chart of Ozone
  output$ozone_pie <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without Ozone", "Days Ozone"),
        value = c((current()$Days.with.AQI-current()$Days.Ozone)/current()$Days.with.AQI*100, current()$Days.Ozone/current()$Days.with.AQI*100)
      )
      
      df$group <- factor(df$group, levels = c("Days without Ozone","Days Ozone"))
      
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_manual(values=c("#b7dfe2", "#01a6b5")) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          plot.margin=grid::unit(c(margin_y()+2.655,margin_x(),margin_y()+2.655,margin_x()), "mm"),
          plot.background = element_rect(color = NA, fill = "#bcdae0"),
          legend.background = element_rect(color = NA, fill = "#bcdae0"),
          panel.background = element_rect(fill = "#bcdae0", color  =  NA),
          strip.background = element_rect(fill = "#bcdae0", color = "#bcdae0"),
          legend.text = element_text(size = legend_text_size()), 
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size()),
          axis.title = element_text(size = axis_title_size()),
          legend.title = element_text(size = legend_title_size())
        )
      pie
    }
  })
  
  # pie chart of SO2
  output$so2_pie <- renderPlot({
    
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without SO2", "Days SO2"),
        value = c((current()$Days.with.AQI-current()$Days.SO2)/current()$Days.with.AQI*100, current()$Days.SO2/current()$Days.with.AQI*100)
      )
      
      df$group <- factor(df$group, levels = c("Days without SO2","Days SO2"))
      
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_manual(values=c("#c6b6d8", "#5807b7")) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          plot.margin=grid::unit(c(margin_y()+0.23,margin_x(),margin_y()+0.23,margin_x()), "mm"),
          plot.background = element_rect(color = NA, fill = "#bcdae0"),
          legend.background = element_rect(color = NA, fill = "#bcdae0"),
          panel.background = element_rect(fill = "#bcdae0", color  =  NA),
          strip.background = element_rect(fill = "#bcdae0", color = "#bcdae0"),
          legend.text = element_text(size = legend_text_size()), 
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size()),
          axis.title = element_text(size = axis_title_size()),
          legend.title = element_text(size = legend_title_size())
        )
      pie
    }
  })
  
  # pie chart of PM2.5
  output$pm25_pie <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without PM2.5", "Days PM2.5"),
        value = c((current()$Days.with.AQI-current()$Days.PM2.5)/current()$Days.with.AQI*100, current()$Days.PM2.5/current()$Days.with.AQI*100)
      )
      
      df$group <- factor(df$group, levels = c("Days without PM2.5","Days PM2.5"))
      
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_manual(values=c("#e2d0b5", "#c97c08")) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          plot.margin=grid::unit(c(margin_y()+2.11,margin_x(),margin_y()+2.11,margin_x()), "mm"),
          plot.background = element_rect(color = NA, fill = "#bcdae0"),
          legend.background = element_rect(color = NA, fill = "#bcdae0"),
          panel.background = element_rect(fill = "#bcdae0", color  =  NA),
          strip.background = element_rect(fill = "#bcdae0", color = "#bcdae0"),
          legend.text = element_text(size = legend_text_size()), 
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size()),
          axis.title = element_text(size = axis_title_size()),
          legend.title = element_text(size = legend_title_size())
        )
      pie
    }
  })
  
  # pie chart of PM10
  output$pm10_pie <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        group = c("Days without PM10", "Days PM10"),
        value = c((current()$Days.with.AQI-current()$Days.PM10)/current()$Days.with.AQI*100, current()$Days.PM10/current()$Days.with.AQI*100)
      )
      
      df$group <- factor(df$group, levels = c("Days without PM10","Days PM10"))
      
      
      pie <- ggplot(df, aes(x="", y=value, fill=group)) + theme_minimal() +
        geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_manual(values=c("#e0b1b1", "#c40909")) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          plot.margin=grid::unit(c(margin_y()+1.97,margin_x(),margin_y()+1.97,margin_x()), "mm"),
          plot.background = element_rect(color = NA, fill = "#bcdae0"),
          legend.background = element_rect(color = NA, fill = "#bcdae0"),
          panel.background = element_rect(fill = "#bcdae0", color  =  NA),
          strip.background = element_rect(fill = "#bcdae0", color = "#bcdae0"),
          legend.text = element_text(size = legend_text_size()), 
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size()),
          axis.title = element_text(size = axis_title_size()),
          legend.title = element_text(size = legend_title_size())
        )
      pie
    }
  })
  
  # table of pollutants
  output$pollutants_table <- DT::renderDataTable(current()[, c('Days.CO', 'Days.NO2',"Days.Ozone", "Days.SO2", "Days.PM2.5", "Days.PM10")],
                                                 rownames = FALSE,
                                                 colnames = c('CO', 'NO2', 'Ozone', 'SO2','PM2.5','PM10'), 
                                                 options = list(searching = FALSE,paging = FALSE,
                                                                dom = 't'
                                                 ))
  
  # bar chart of pollutants
  output$pollutants_bar <- renderPlot({
    if(length(current()$State)==1){
      
      df <- data.frame(
        
        group = c('CO', 'NO2', 'Ozone', 'SO2','PM2.5','PM10'),
        value = c(current()$Days.CO, current()$Days.NO2, 
                  current()$Days.Ozone,
                  current()$Days.SO2,
                  current()$Days.PM2.5,
                  current()$Days.PM10)
      )
      
      bar <-ggplot(data=df, aes(x=group, y=value, fill = group)) +
        geom_bar(stat="identity") + coord_flip() + 
        theme(
          text = element_text(size=12),
          legend.position="none"
        )+ 
        xlab("Detected Pollutant") + ylab("Days count") +
        scale_fill_manual(values=c("#C3B5DB", "#ABB6D4", "#83BDDF","#A2DFA8", "#98D5B3", "#93D8CD"))+
        theme(
          axis.title.x = element_text(color = "black"),
          axis.title.y = element_text(color = "black"),
          panel.border = element_blank(),
          plot.background = element_rect(color = NA, fill = "#bcdae0"),
          legend.background = element_rect(color = NA, fill = "#bcdae0"),
          panel.background = element_rect(fill = "#bcdae0", color  =  NA),
          panel.grid.major = element_line(color = "black"),  
          panel.grid.minor = element_line(color = "black"),
          legend.text = element_text(size = legend_text_size()), 
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size(), color = "black"),
          axis.title = element_text(size = axis_title_size()),
          legend.title = element_text(size = legend_title_size())
        ) # + labs(y="Number of days as main pollutant")
      bar
    }
  })
  
  output$sel_state <- renderText({ 
    selected_state()
  })
  
  output$sel_county <- renderText({ 
    selected_county()
  })
  
  output$data_years <- renderText({ 
    paste(nrow(subset(dataset, State == selected_state() & County == selected_county())),"years of data available")
  })
  
  output$data_days <- renderText({ 
    d<-subset(dataset, State == selected_state() & County == selected_county())
    paste(round(mean(d$Days.with.AQI)),"days per year with data on average")
  })
  
  output$missing_data <- renderText({
    d <- current()
    if(round(d$Days.with.AQI/365*100) == 100){
      paste("Selected County:",input$County,"-",input$State,", the number of days with AQI data for the year",input$Year,"is:",d$Days.with.AQI,",",round(d$Days.with.AQI/365*100),"% of data is available. The percentages are accurate")
    } else{
      paste("Selected County:",input$County,"-",input$State,", the number of days with AQI data for the year",input$Year,"is:",d$Days.with.AQI,", only the",round(d$Days.with.AQI/365*100),"% of data is available. The percentages are therefore estimates")
    }
  })
  
  
  
  
  # Time series of AQI statistics
  output$aqi_time <- renderPlot({
    df<-subset(dataset, State == selected_state() & County == selected_county() & Year > input$range[1] & Year < input$range[2])
    ggplot(data = df, aes(x = Year)) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = input$textColor),
        panel.border = element_blank(),
        plot.background = element_rect(color = NA, fill = input$backgroundColor),
        legend.background = element_rect(color = NA, fill = input$backgroundColor),
        legend.key = element_rect(color = NA, fill = input$backgroundColor),
        panel.background = element_rect(fill = input$backgroundColor, color  =  NA),
        panel.grid.major = element_line(color = input$textColor),  
        panel.grid.minor = element_line(color = input$textColor),
        legend.text = element_text(size = legend_text_size(), color = input$textColor), 
        legend.key.size = unit(legend_key_size(), 'line'),
        axis.text = element_text(size = axis_text_size(), color = input$textColor),
        axis.title = element_text(size = axis_title_size()),
        legend.title = element_text(size = legend_title_size(), color = input$textColor)
      ) +
      geom_line(aes(y = Max.AQI, color = "Max"), size = line_size(), group = 1) + 
      geom_point(aes(y = Max.AQI, color = "Max"), size = line_size()*3) +
      geom_line(aes(y = X90th.Percentile.AQI, color = "90th Percentile"), size = line_size(), group = 3) +
      geom_point(aes(y = X90th.Percentile.AQI, color = "90th Percentile"), size = line_size()*3) +
      geom_line(aes(y = Median.AQI, color = "Median"), size = line_size(), group = 2) +
      geom_point(aes(y = Median.AQI, color = "Median"), size = line_size()*3) +
      labs(x = "Year", y = "Air Quality Index") +
      scale_x_continuous(breaks = round(seq(max(min(df$Year),input$range[1]), min(max(df$Year),input$range[2]), by = 1),1)) +
      # scale_color_manual(name = "Statistics",
      #                    values = c("Max" = "firebrick1", 
      #                               "90th Percentile" = "firebrick4",
      #                               "Median" = "steelblue1")) +
      scale_color_discrete(breaks=c("Max","90th Percentile","Median"))
  })
  
  # Time series of Pollutants Percentage
  output$pollutants_time <- renderPlot({
    s_county<-subset(dataset, State == selected_state() & County == selected_county())
    s_county[,14:19]<- s_county[14:19]/s_county$Days.with.AQI*100
    # df <- data.frame(
    #   
    #   group = c("CO", "NO2", "Ozone", "SO2", "PM2.5","PM10"),
    #   value = c(s_county$Days.CO/s_county$Days.with.AQI*100, s_county$Days.NO2/s_county$Days.with.AQI*100, 
    #             s_county$Days.Ozone/s_county$Days.with.AQI*100,
    #             s_county$Days.SO2/s_county$Days.with.AQI*100,
    #             s_county$Days.PM2.5/s_county$Days.with.AQI*100,
    #             s_county$Days.PM10/s_county$Days.with.AQI*100)
    # )
    ggplot(data = s_county, aes(x = Year)) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(color = input$textColor),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(color = NA, fill = input$backgroundColor),
        legend.background = element_rect(color = NA, fill = input$backgroundColor),
        legend.key = element_rect(color = NA, fill = input$backgroundColor),
        panel.background = element_rect(fill = input$backgroundColor, color  =  NA),
        panel.grid.major = element_line(color = input$textColor),  
        panel.grid.minor = element_line(color = input$textColor),
        legend.text = element_text(size = legend_text_size(), color = input$textColor), 
        legend.key.size = unit(legend_key_size(), 'line'),
        axis.text = element_text(size = axis_text_size(), color = input$textColor),
        axis.title = element_text(size = axis_title_size()),
        legend.title = element_text(size = legend_title_size(), color = input$textColor)
      ) + labs(y = "Percentage of days as main Pollutant") +
      geom_line(aes(y = Days.CO, color = "CO"), size = line_size(), group = 1) + 
      geom_point(aes(y = Days.CO, color = "CO"), size = line_size()*3) +
      geom_line(aes(y = Days.NO2, color = "NO2"), size = line_size(), group = 2) +
      geom_point(aes(y = Days.NO2, color = "NO2"), size = line_size()*3) +
      geom_line(aes(y = Days.Ozone, color = "Ozone"), size = line_size(), group = 3) +
      geom_point(aes(y = Days.Ozone, color = "Ozone"), size = line_size()*3) +
      geom_line(aes(y = Days.SO2, color = "SO2"), size = line_size(), group = 4) +
      geom_point(aes(y = Days.SO2, color = "SO2"), size = line_size()*3) +
      geom_line(aes(y = Days.PM2.5, color = "PM2.5"), size = line_size(), group = 5) +
      geom_point(aes(y = Days.PM2.5, color = "PM2.5"), size = line_size()*3) +
      geom_line(aes(y = Days.PM10, color = "PM10"), size = line_size(), group = 6) +
      geom_point(aes(y = Days.PM10, color = "PM10"), size = line_size()*3) +
      labs(x = "Year", y = "Percentage of Pollutant") +
      scale_x_continuous(breaks = round(seq(max(min(s_county$Year),input$range[1]), min(max(s_county$Year),input$range[2]), by = 1),1)) +
      scale_y_continuous(breaks = round(seq(min(s_county[14:19]), max(s_county[14:19]), by = 10),1)) +
      scale_color_manual(name = "Statistics",
                         values = c("CO" = input$colorCO,
                                    "NO2" = input$colorNO2,
                                    "Ozone" = input$colorOZONE,
                                    "SO2" = input$colorSO2,
                                    "PM2.5" = input$colorPM25,
                                    "PM10" = input$colorPM10))
    # scale_fill_manual(values=c("#9B77D8", "#758fd6", "#68aed6","#6ed378", "#6ad197", "#66d6c4"))
    
    # scale_color_discrete(breaks=c("Max","90th Percentile","Median"))
  })
  
  # table of pollutants
  output$pollutants_time_table <- DT::renderDataTable(subset(dataset, State == selected_state() & County == selected_county())[, c('Year','Days.CO', 'Days.NO2',"Days.Ozone", "Days.SO2", "Days.PM2.5", "Days.PM10")],
                                                      rownames = FALSE,
                                                      colnames = c('Year','CO', 'NO2', 'Ozone', 'SO2','PM2.5','PM10'), 
                                                      options = list(searching = TRUE,paging = TRUE,lengthMenu = c(5, 10, 40), pageLength = tbl_pagelength()
                                                                     # dom = 't'
                                                      ))
  
  # County on Leaflet Map
  output$map_county <- renderLeaflet({
    
    # Extracting long and lat of selected county from sites
    site<-subset(sites, State.Name == selected_state() & County.Name == selected_county())
    
    latit <- site$Latitude
    latit <- latit[latit!=0] # Eliminating 0 values
    latit <- latit[!is.na(latit)] # Eliminating NAs
    computed_lat <- mean(latit)
    longit <- site$Longitude
    longit <- longit[longit!=0] # Eliminating 0 values
    longit <- longit[!is.na(longit)] # Eliminating NAs
    computed_lng <- mean(longit)
    
    # xy <- geojsonio::geojson_read("gz_2010_us_050_00_20m.json", what = "sp")
    
    # nyc <- xy[xy$STATE == 36, ]
    
    leaflet(xy) %>%
      addTiles() %>%
      addPolygons(color = "#962121", weight = 0.8, smoothFactor = 0.2,
                  opacity = 1.0, fillOpacity = 0.1,
                  # fillColor = ~colorQuantile("YlOrRd"),
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE)) %>%
      setView(lng = computed_lng, lat = computed_lat, zoom = zoom_level()) %>%
      addMarkers(lng = computed_lng, lat = computed_lat, 
                 label = paste(selected_state(),"-",selected_county()),
                 labelOptions = labelOptions(textsize = marker_text_size())
                 )
  })
  
  # 3 Counties on LeafLet Map
  # output$map_counties <- renderLeaflet({
  #   
  #   # Extracting long and lat of selected county from sites
  #   site1<-subset(sites, State.Name == selected_state1() & County.Name == selected_county1())
  #   site2<-subset(sites, State.Name == selected_state2() & County.Name == selected_county2())
  #   site3<-subset(sites, State.Name == selected_state3() & County.Name == selected_county3())
  #   
  #   
  #   latit1 <- site1$Latitude
  #   latit2 <- site2$Latitude
  #   latit3 <- site3$Latitude
  #   
  #   latit1 <- latit1[latit1!=0] # Eliminating 0 values
  #   latit1 <- latit1[!is.na(latit1)] # Eliminating NAs
  #   latit2 <- latit2[latit2!=0] # Eliminating 0 values
  #   latit2 <- latit2[!is.na(latit2)] # Eliminating NAs
  #   latit3 <- latit3[latit3!=0] # Eliminating 0 values
  #   latit3 <- latit3[!is.na(latit3)] # Eliminating NAs
  #   
  #   computed_lat1 <- mean(latit1)
  #   computed_lat2 <- mean(latit2)
  #   computed_lat3 <- mean(latit3)
  #   
  #   longit1 <- site1$Longitude
  #   longit1 <- longit1[longit1!=0] # Eliminating 0 values
  #   longit1 <- longit1[!is.na(longit1)] # Eliminating NAs
  #   longit2 <- site2$Longitude
  #   longit2 <- longit2[longit2!=0] # Eliminating 0 values
  #   longit2 <- longit2[!is.na(longit2)] # Eliminating NAs
  #   longit3 <- site3$Longitude
  #   longit3 <- longit3[longit3!=0] # Eliminating 0 values
  #   longit3 <- longit3[!is.na(longit3)] # Eliminating NAs
  #   
  #   computed_lng1 <- mean(longit1)
  #   computed_lng2 <- mean(longit2)
  #   computed_lng3 <- mean(longit3)
  #   
  #   mean_lng <- mean(computed_lng1,computed_lng2,computed_lng3)
  #   mean_lat <- mean(computed_lat1,computed_lat2,computed_lat3)
  #   
  #   
  #   # xy <- geojsonio::geojson_read("gz_2010_us_050_00_20m.json", what = "sp")
  #   
  #   # nyc <- xy[xy$STATE == 36, ]
  #   
  #   leaflet(xy) %>%
  #     addTiles() %>%
  #     addPolygons(color = "#962121", weight = 0.8, smoothFactor = 0.2,
  #                 opacity = 1.0, fillOpacity = 0.1,
  #                 # fillColor = ~colorQuantile("YlOrRd"),
  #                 highlightOptions = highlightOptions(color = "white", weight = 3,
  #                                                     bringToFront = TRUE)) %>%
  #     setView(lng = -87.72265, lat = 41.8518, zoom = zoom_level()-6) %>%
  #     addMarkers(lng = computed_lng1, lat = computed_lat1, label = paste(selected_state1(),"-",selected_county1()),
  #                labelOptions = labelOptions(textsize = marker_text_size())
  #     ) %>%
  #     addMarkers(lng = computed_lng2, lat = computed_lat2, label = paste(selected_state2(),"-",selected_county2()),
  #                labelOptions = labelOptions(textsize = marker_text_size())
  #                ) %>%
  #     addMarkers(lng = computed_lng3, lat = computed_lat3, label = paste(selected_state3(),"-",selected_county3()),
  #                labelOptions = labelOptions(textsize = marker_text_size())) 
  # })
  
  # Time series of AQI statistics
  # output$aqi_time_comp <- renderPlot({
  #   df1<-subset(dataset, State == selected_state1() & County == selected_county1())
  #   df1 <- data.frame(df1$Median.AQI,df1$Max.AQI,df1$X90th.Percentile.AQI,df1$Year)
  #   df2<-subset(dataset, State == selected_state2() & County == selected_county2())
  #   df2 <- data.frame(df2$Median.AQI,df2$Max.AQI,df2$X90th.Percentile.AQI,df2$Year)
  #   df3<-subset(dataset, State == selected_state3() & County == selected_county3())
  #   df3 <- data.frame(df3$Median.AQI,df3$Max.AQI,df3$X90th.Percentile.AQI,df3$Year)
  #   
  #   df <- merge(df1,df2,by.x = "df1.Year",by.y = "df2.Year")
  #   df <- merge(df,df3,by.x = "df1.Year",by.y = "df3.Year")
  #   
  #   
  #   plot <- ggplot(data = df, aes(x = df1.Year)) +
  #     theme(
  #       axis.text.x = element_text(angle = 45, hjust = 1),
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_text(color = input$textColor),
  #       panel.border = element_blank(),
  #       plot.background = element_rect(color = NA, fill = "#005669"),
  #       legend.background = element_rect(color = NA, fill = "#005669"),
  #       legend.key = element_rect(color = NA, fill = "#005669"),
  #       panel.background = element_rect(fill = "#005669", color  =  NA),
  #       panel.grid.major = element_line(color = "white"),  
  #       panel.grid.minor = element_line(color = "white"),
  #       legend.text = element_text(size = legend_text_size(), color = "white"), 
  #       legend.key.size = unit(legend_key_size(), 'line'),
  #       axis.text = element_text(size = axis_text_size(), color = "white"),
  #       axis.title = element_text(size = axis_title_size()),
  #       legend.title = element_text(size = legend_title_size(), color = "white")
  #     ) + labs(x = "Year", y = "Air Quality Index")
  #     
  #     addLines <- function(plot, xx, yy, zz) {
  #       xx <- enquo(xx)
  #       yy <- enquo(yy)
  #       zz <- enquo(zz)
  #       
  #       plot <- plot +
  #         geom_line(aes(y = !!xx, color = paste(input$Statistic,"AQI",selected_county1(),"-",selected_state1())), size = line_size(), group = 1) + 
  #         geom_point(aes(y = !!xx, color = paste(input$Statistic,"AQI",selected_county1(),"-",selected_state1())), size = line_size()*3) +
  #         geom_line(aes(y = !!yy, color = paste(input$Statistic,"AQI",selected_county2(),"-",selected_state2())), size = line_size(), group = 3) +
  #         geom_point(aes(y = !!yy, color = paste(input$Statistic,"AQI",selected_county2(),"-",selected_state2())), size = line_size()*3) +
  #         geom_line(aes(y = !!zz, color = paste(input$Statistic,"AQI",selected_county3(),"-",selected_state3())), size = line_size(), group = 2) +
  #         geom_point(aes(y = !!zz, color = paste(input$Statistic,"AQI",selected_county3(),"-",selected_state3())), size = line_size()*3) +
  #         scale_color_discrete(name = "Selected counties",breaks=c(paste(input$Statistic,"AQI",selected_county1(),"-",selected_state1()),
  #                                                                  paste(input$Statistic,"AQI",selected_county2(),"-",selected_state2()),
  #                                                                  paste(input$Statistic,"AQI",selected_county3(),"-",selected_state3())))
  #       return(plot)
  #     }
  #     
  #     if(input$Statistic == "Median")
  #       plot <- addLines(plot,df1.Median.AQI,df2.Median.AQI,df3.Median.AQI)
  #     else if(input$Statistic == "Max")
  #       plot <- addLines(plot,df1.Max.AQI,df2.Max.AQI,df3.Max.AQI)
  #     else if(input$Statistic == "90th percentile")
  #       plot <- addLines(plot,df1.X90th.Percentile.AQI,df2.X90th.Percentile.AQI,df3.X90th.Percentile.AQI)
  #     
  #     
  #     plot
  #     
  # })
  
  # Time series of AQI statistics
  # output$pollutants_time_comp <- renderPlot({
  #   
  #   s_county1<-subset(dataset, State == selected_state1() & County == selected_county1())
  #   s_county1[,14:19]<- s_county1[14:19]/s_county1$Days.with.AQI*100
  #   s_county2<-subset(dataset, State == selected_state2() & County == selected_county2())
  #   s_county2[,14:19]<- s_county2[14:19]/s_county2$Days.with.AQI*100
  #   s_county3<-subset(dataset, State == selected_state3() & County == selected_county3())
  #   s_county3[,14:19]<- s_county3[14:19]/s_county3$Days.with.AQI*100
  #   
  #   
  #   s_county1 <- merge(s_county1,s_county2,by = "Year")
  #   s_county1 <- merge(s_county1,s_county3,by = "Year")
  # 
  #   plot <- ggplot(data = s_county1, aes(x = Year)) +
  #     theme(
  #       axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
  #       axis.title.y = element_text(color = "white"),
  #       axis.title.x = element_blank(),
  #       panel.border = element_blank(),
  #       plot.background = element_rect(color = NA, fill = "#005669"),
  #       legend.background = element_rect(color = NA, fill = "#005669"),
  #       legend.key = element_rect(color = NA, fill = "#005669"),
  #       panel.background = element_rect(fill = "#005669", color  =  NA),
  #       panel.grid.major = element_line(color = "white"),  
  #       panel.grid.minor = element_line(color = "white"),
  #       legend.text = element_text(size = legend_text_size(), color = "white"), 
  #       legend.key.size = unit(legend_key_size(), 'line'),
  #       axis.text = element_text(size = axis_text_size(), color = "white"),
  #       axis.title = element_text(size = axis_title_size()),
  #       legend.title = element_text(size = legend_title_size(), color = "white")
  #     ) + labs(y = "Percentage of days as main Pollutant")
  #   # scale_x_continuous(breaks = round(seq(min(s_county1$df1.Year), max(s_county1$df1.Year), by = 1),1)) +
  #   
  #   # Quosure to use input as a function to pass to aesthetics
  #   addLines <- function(plot, xx, yy, zz) {
  #     xx <- enquo(xx)
  #     yy <- enquo(yy)
  #     zz <- enquo(zz)
  #     
  #     plot <- plot +
  #       geom_line(aes(y = !!xx, color = paste(input$Pollutant,selected_county1(),"-",selected_state1())), size = line_size(), group = 1) + 
  #       geom_point(aes(y = !!xx, color = paste(input$Pollutant,selected_county1(),"-",selected_state1())), size = line_size()*3) +
  #       geom_line(aes(y = !!yy, color = paste(input$Pollutant,selected_county2(),"-",selected_state2())), size = line_size(), group = 3) +
  #       geom_point(aes(y = !!yy, color = paste(input$Pollutant,selected_county2(),"-",selected_state2())), size = line_size()*3) +
  #       geom_line(aes(y = !!zz, color = paste(input$Pollutant,selected_county3(),"-",selected_state3())), size = line_size(), group = 2) +
  #       geom_point(aes(y = !!zz, color = paste(input$Pollutant,selected_county3(),"-",selected_state3())), size = line_size()*3) +
  #       scale_color_discrete(name = "Selected counties",breaks=c(paste(input$Pollutant,selected_county1(),"-",selected_state1()),
  #                                                                paste(input$Pollutant,selected_county2(),"-",selected_state2()),
  #                                                                paste(input$Pollutant,selected_county3(),"-",selected_state3())))
  #     return(plot)
  #   }
  #   
  #   if(input$Pollutant == "CO")
  #     plot <- addLines(plot,Days.CO.x,Days.CO.y,Days.CO)
  #   else if(input$Pollutant == "NO2")
  #     plot <- addLines(plot,Days.NO2.x,Days.NO2.y,Days.NO2)
  #   else if(input$Pollutant == "Ozone")
  #     plot <- addLines(plot,Days.Ozone.x,Days.Ozone.y,Days.Ozone)
  #   else if(input$Pollutant == "SO2")
  #     plot <- addLines(plot,Days.SO2.x,Days.SO2.y,Days.SO2)
  #   else if(input$Pollutant == "PM2.5")
  #     plot <- addLines(plot,Days.PM2.5.x,Days.PM2.5.y,Days.PM2.5)
  #   else if(input$Pollutant == "PM10")
  #     plot <- addLines(plot,Days.PM10.x,Days.PM10.y,Days.PM10)
  #   
  #   plot
  #   })
  
  # bar chart of pollutants comparison
  # output$pollutants_bar_comp <- renderPlot({
  # 
  #   df1<-subset(dataset, Year == input$Year & State == selected_state1() & County == selected_county1())
  #   df2<-subset(dataset, Year == input$Year & State == selected_state2() & County == selected_county2())
  #   df3<-subset(dataset, Year == input$Year & State == selected_state3() & County == selected_county3())
  #   
  #   if(length(df1$Year)!=0 & length(df2$Year)!=0 & length(df3$Year)!=0){
  #   
  #     df1 <- data.frame(
  #       
  #       group = c('CO', 'NO2', 'Ozone', 'SO2','PM2.5','PM10'),
  #       value = c(df1$Days.CO, 
  #                 df1$Days.NO2, 
  #                 df1$Days.Ozone,
  #                 df1$Days.SO2,
  #                 df1$Days.PM2.5,
  #                 df1$Days.PM10),
  #       county = c(
  #         paste(selected_county1(),"-",selected_state1())
  #       )
  #       
  #     )
  #     
  #     df2 <- data.frame(
  #       
  #       group = c('CO', 'NO2', 'Ozone', 'SO2','PM2.5','PM10'),
  #       value = c(df2$Days.CO, 
  #                 df2$Days.NO2, 
  #                 df2$Days.Ozone,
  #                 df2$Days.SO2,
  #                 df2$Days.PM2.5,
  #                 df2$Days.PM10),
  #       county = c(
  #         paste(selected_county2(),"-",selected_state2())
  #       )
  #       
  #     )
  #     
  #     df3 <- data.frame(
  #       
  #       group = c('CO', 'NO2', 'Ozone', 'SO2','PM2.5','PM10'),
  #       value = c(df3$Days.CO, 
  #                 df3$Days.NO2, 
  #                 df3$Days.Ozone,
  #                 df3$Days.SO2,
  #                 df3$Days.PM2.5,
  #                 df3$Days.PM10),
  #       county = c(
  #         paste(selected_county3(),"-",selected_state3())
  #       )
  #       
  #     )
  #     
  #     df <- rbind(df1, df2, df3)
  #     
  #     bar <-ggplot(data=df, aes(x=group, y=value, fill = county)) +
  #       geom_bar(stat="identity", position=position_dodge()) + 
  #       theme(
  #         text = element_text(size=12)
  #       ) + 
  #       ylab(paste("Days as main pollutant in",input$Year)) +
  #       theme(
  #         axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_text(color = "white"),
  #         
  #         panel.border = element_blank(),
  #         plot.background = element_rect(color = NA, fill = "#005669"),
  #         panel.background = element_rect(fill = "#005669", color  =  NA),
  #         panel.grid.major = element_line(color = "white"),
  #         panel.grid.minor = element_line(color = "white"),
  #         legend.text = element_text(size = legend_text_size(), color = "white"),
  #         legend.key.size = unit(legend_key_size(), 'line'),
  #         axis.text = element_text(size = axis_text_size(), color = "white"),
  #         axis.title = element_text(size = axis_title_size()),
  #         legend.title = element_text(size = legend_title_size(), color = "white"),
  #         legend.key = element_rect(color = NA, fill = "#005669"),
  #         legend.background = element_rect(color = NA, fill = "#005669")
  #       )
  #     bar
  #     
  # } # Signaling missing data
  # else {
  #   if(length(df1$Year)==0)
  #     shinyalert("Oops!", paste("No data for",selected_county1(),"-",selected_state1(),"in year",input$Year), type = "error")
  #   else if(length(df2$Year)==0)
  #     shinyalert("Oops!", paste("No data for",selected_county2(),"-",selected_state2(),"in year",input$Year), type = "error")
  #   else if(length(df3$Year)==0)
  #     shinyalert("Oops!", paste("No data for",selected_county3(),"-",selected_state3(),"in year",input$Year), type = "error")
  # }
  # })
  
  
  # About HTML
  output$about_out <- renderUI({
    author <- "<h1>Mirko Mantovani</h1>
    <br>
    <a href='https://mmanto2.people.uic.edu/projects/JustBreathe.html'>Project webpage</a>
    <br/>
    <a href='https://github.com/mirkomantovani/VisualAnalytics-JustBreathe'>Github repository</a><br>"
    libraries <- "<b>Used R libraries: </b> <br><br> 
    <ul>
    <li>shiny</li>
    <li>shinydashboard</li>
    <li>ggplot2</li>
    <li>scales</li>
    <li>shinythemes</li>
    <li>dashboardthemes</li>
    <li>ggthemes</li>
    <li>shinyalert</li>
    <li>leaflet</li>
    <li>rgdal</li>
    <li>geojson</li>
    <li>geojsonio</li>
    <li>colourpicker</li>
    <li>shinyWidgets</li>

    </ul><br>"
    data <- "<b>Dataset Source:</b></br> <a href='https://aqs.epa.gov/aqsweb/airdata/download_files.html'>United States Environmental Protection Agency</a><br>
    <a href='http://eric.clst.org/tech/usgeojson/e'>United States Counties shape in GeoJSON</a>"
    HTML(paste(author, libraries, data))
  })
  
  
  # End of server
}

shinyApp(ui = ui, server = server)