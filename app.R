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
library(viridis) # Color palette
library(cdlTools) # convert FIPS codes into names
library(htmltools) # to use htmlEscape function
library(plotly)
library(RColorBrewer)
library(reshape2)
library(fst)
library(future)
library(data.table)
library(ggvis)
library(dplyr)
library(tidyr)

# importing datasets
setwd("./csv/")
temp = list.files(pattern="*.csv")
datasets = lapply(temp, read.csv)
dataset <- do.call(rbind, datasets)
setwd("../")

daily_df <- read_fst("fst/daily_all_aqi_by_county.fst")
names(daily_df) <- c("state","county","aqi","category","pollutant","year","month","day")

daily_all <- read_fst("fst/daily_all_pollutants_2018.fst")

hourly_df <- read_fst("fst/hourly_all_data_2018.fst")

# needed for counties coordinates
sites <- fread(file = "sites/aqs_sites.csv", sep=",",header = TRUE)
# geojson file for counties shape

f_xy <- future({
  xy <- geojsonio::geojson_read("gz_2010_us_050_00_20m.json", what = "sp")
  # Since the xy has factored FIPS code for state instead of names, converting them in numeric and then
  # getting the names
  converted_states_names <- fips(as.numeric(levels(xy$STATE))[xy$STATE],to="name")
  xy$STATENAME<-converted_states_names
  xy
}) %plan% multiprocess




########################################### PREPROCESSING #########################################

years<-c(1980:2018)
H_years<-c(2018) #years available for hourly data
H_years_italy<-c(2018,2019)
H_months<-c("January","February","March","April","May","June","July","August","September","October","November","December")
H_days<-c(1:31)
states<-unique(dataset$State)
t<-subset(dataset, State == 'Illinois')
counties<-unique(t$County)

pollutants <- c("CO","NO2","Ozone","SO2","PM2.5","PM10")
pollutants_2 <- c("PM2.5","PM10","CO","NO2","Ozone","SO2")

statistics <- c("Median","Max","90th percentile")


# All counties with state
all_counties <- c()
for(s in states){
  coun <- subset(dataset, State == s)
  counti<-unique(coun$County)
  counti <- paste(counti,"-",s)
  all_counties <- c(all_counties,counti)
}

############################################################ ITALY ########################################################################################
italy_df <- read_fst("italy/daily_italy.fst")
hourly_df_italy <- read_fst("italy/hourly_italy.fst")

cities_italy <- levels(unique(italy_df$city)) #preprocess data such that capitalization is proper

print("done reading data")
############################################### UI ################################################

ui <- dashboardPage(
  dashboardHeader(
    # FOR PRESENTATION
    # title = "Visual Analytics - Every Breath You Take",
    # titleWidth = 370
    title = "Every Breath You Take",
    titleWidth = 250
  ),
  dashboardSidebar(
    # FOR PRESENTATION
    disable = FALSE,
    # collapsed = TRUE,
    # width = 370,
    collapsed = FALSE,
    width = 250,
    sidebarMenu(
      useShinyalert(),
      menuItem("Yearly visualizations",
               menuItem("Yearly trends", tabName = "time"),
               menuItem("Year details for County", tabName = "pie")),
      # menuItem("Yearly trends", tabName = "time"),
      # menuItem("Year details for County", tabName = "pie"),
      # menuItem("Compare Counties", tabName = "compare"),
      # menuItem("Monthly AQI levels", tabName = "monthly_aqi"),
      menuItem("Daily AQI", tabName = "daily_aqi"),
      menuItem("Hourly Pollutants", tabName = "hourly_pollutants"),
      menuItem("Pollutants Heatmap", tabName = "pollutants_map"),
      menuItem("Italy",
               menuItem("Daily trends", tabName = "italy_daily"),
               menuItem("Hourly trends", tabName = "italy_hourly"),
               menuItem("Totals over 90 days?", tabName = "italy_totals")),
      menuItem("Inputs",
               selectInput(inputId = "State", "Select State", states, selected = 'Illinois',width = "200%"),
               tags$style("#County {background-color:blue;}"),
               selectInput("County", "Select County", counties, selected = 'Adams',width = "200%"),
               div(id="nozoom",sliderInput(inputId = "Year",
                                           sep = "",
                                           label = "Select Year",
                                           value = 2018, min = 1980, max = 2018,width = "90%")),
               materialSwitch(inputId = "switch_units", label = "Switch to Imperial units", status = "primary"),
               startExpanded = TRUE),
      menuItem("About", tabName = "about")
      
    ),
    # custom CSS
    includeCSS("style.css")
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
                                         box(title = "Percentage of AQI level", width = NULL,status = "primary",div(plotOutput("aqi_pie", height = "40vmin")))),
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
                                    fluidRow(column(4,plotOutput("co_pie", height = "35vmin")),column(4,plotOutput("no2_pie", height = "35vmin")),column(4,plotOutput("ozone_pie", height = "35vmin"))),
                                    fluidRow(column(4,plotOutput("so2_pie", height = "35vmin")),column(4,plotOutput("pm25_pie", height = "35vmin")),column(4,plotOutput("pm10_pie", height = "35vmin")))
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
                                      div(id="notforsage",
                                          h3("State:"),
                                          h4(textOutput("sel_state")),
                                          h3("County:"),
                                          h4(textOutput("sel_county")),
                                          h3("Data:"),
                                          h6(textOutput("data_years")),
                                          h6(textOutput("data_days"))),
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
    tabItem("daily_aqi",
            fluidRow(
              # 2 tabs, (line plot, bar chart, table)
              column(12,
                     tabsetPanel(
                       tabPanel("AQI Time Series",
                                ggvisOutput("daily_aqi_line")
                       ),
                       tabPanel("Bar chart",
                                plotOutput("daily_bar", height = "80vmin")
                       ),
                       tabPanel("Table",
                                div(DT::dataTableOutput("daily_aqi_table"), style = "font-size:130%")
                       )
                     )
              )
            )
    ),
    tabItem("hourly_pollutants",
            fluidRow(
              dropdownButton(	
                tags$h3("Other colors"),	
                colourInput("colorCO_hp", h5("Select color CO"), value = "#c6c60f"),	
                colourInput("colorNO2_hp", h5("Select color NO2"), value = "#13c649"),	
                colourInput("colorOZONE_hp", h5("Select color Ozone"), value = "#0fa2af"),	
                colourInput("colorSO2_hp", h5("Select color SO2"), value = "#A877E0"),	
                colourInput("colorPM25_hp", h5("Select color PM2.5"), value = "#cc8112"),	
                colourInput("colorPM10_hp", h5("Select color PM10"), value = "#ba1010"),	
                colourInput("colorWS_hp", h5("Select color Wind Speed"), value = "#E3446E"),	
                colourInput("colorTemp_hp", h5("Select color Temperature"), value = "#6B1F13"),	
                
                circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",	
                tooltip = tooltipOptions(title = "Click to open"),	
                
              # Input county with search
              column(2,box(title = "County and date Selection ",status = "success", width = NULL,
                           div(column(12,
                                      colourInput("backgroundColor_hp", h3("Select color"), value = "#005669"),	
                                      selectizeInput("CountySearch_hp", label = h4("Search County"), sort(all_counties), selected = "Cook - Illinois", multiple = FALSE, options = NULL),
                                      selectizeInput(inputId = "H_year", label = h4("Select Year"), H_years, selected = '2018',width = "200%",multiple = FALSE, options = NULL),
                                      selectizeInput(inputId = "H_month", label = h4("Select Month"), H_months, selected = 'January',width = "200%",multiple = FALSE, options = NULL),
                                      selectizeInput(inputId = "H_day", label = h4("Select Day"), H_days, selected = '1',width = "200%",multiple = FALSE, options = NULL)
                                      # selectInput(inputId = "pollutant_chart", "Select Pollutant", c(pollutants), multiple = TRUE, selected = 'AQI',width = "100%")
                           ),class = "boxtozoom")
              ))
              ,
              column(10,plotOutput("hourly_data",height = "85vmin"),checkboxGroupButtons(
                inputId = "hourly_data", # moved in main input panel
                choices = c("NO2","CO", "SO2","Ozone","PM2.5","PM10","Wind Speed","Temperature"),size="lg",
                justified = TRUE, status = "primary", selected = c("NO2","PM2.5","PM10","SO2"),
                checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
              ))
              
              
            )),
    tabItem("pollutants_map",
            div(class="outer",
                # If not using custom CSS, set height of leafletOutput to a number instead of percent
                leafletOutput("map_controllers", width="100%", height="100%"),
                
                # Shiny versions prior to 0.11 should use class = "modal" instead.
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 330, height = "auto",
                              
                              h2("Time and Pollutant"),
                              selectInput(inputId = "pollutant_map", "Select Pollutant", c(pollutants_2,"AQI"), selected = 'PM2.5',width = "100%"),
                              materialSwitch(inputId = "switch_daily", label = "Switch to Daily Data (for 2018)", status = "primary"),
                              numericInput("year_map", "Select Year", min=1990, max=2018, value=2018),
                              div( id="yearly_inputs",
                                   selectInput(inputId = "D_month", "Select Month", H_months, selected = 'January',width = "100%"),
                                   selectInput(inputId = "D_day", "Select Day", H_days, selected = '1',width = "100%")
                              )
                ),
                
                absolutePanel(id = "counties_panel", class = "panel panel-default", fixed = TRUE,
                              draggable = FALSE, top = "auto", left = "auto", right = 20, bottom = -40,
                              width = 330, height = "auto",
                              h2("Shown counties"),
                              knobInput(
                                inputId = "num_counties",
                                label = "Select number of counties",
                                value = 1000,
                                min = 0,
                                max = 1100,
                                displayPrevious = TRUE,
                                lineCap = "round",
                                fgColor = "#428BCA",
                                inputColor = "#428BCA"
                              ),
                              sliderInput(inputId = "Opacity",
                                          sep = "",
                                          label = "Confidence level control",
                                          step = 0.1,
                                          value = 0, min = 0, max = 1
                                          # ,width = "90%"
                              )
                ),
                
                tags$div(id="cite",
                         'Visual Analytics, University of Illinois at Chicago 2019'
                )
            )),
    tabItem("italy_daily",
            # h1("WIP")
            fluidRow(
              # Input county with search
              column(2,box(title = "City selection",status = "success", width = NULL,
                           div(column(12,
                                      h3("City:"),
                                      selectizeInput("CitySearch", label = h4("Search City"), sort(cities_italy), selected = NULL, multiple = FALSE, options = NULL)
                                      
                                      #h4(textOutput("sel_city")) #can get rid of this line
                                      
                           ),class = "boxtozoom")
              )
              ),
              # 2 tabs, (line plots and table, map)
              #   column(10,
              #          tabsetPanel(
              #            tabPanel("AQI Time Series",
              #                     plotlyOutput("daily_aqi_line_italy", height = "85vmin")
              #            ),
              #            tabPanel("Bar chart",
              #                     plotOutput("daily_bar_italy", height = "60vmin")
              #            ),
              #            tabPanel("Table",
              #                     div(DT::dataTableOutput("daily_aqi_table_italy"), style = "font-size:100%")
              #            )
              #   )
              # ),
              column(10,ggvisOutput("daily_aqi_line_italy"),checkboxGroupButtons(
                inputId = "daily_data_italy", # moved in main input panel
                choices = c("NO2","CO", "SO2","Ozone","PM2.5","PM10"),
                justified = TRUE, status = "primary", selected = c("NO2","PM2.5","PM10","SO2"),
                checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
              ))
              
            )
    ),
    
    tabItem("italy_hourly",
      fluidRow(
      # Input city with search
      column(2,box(title = "City and date Selection ",status = "success", width = NULL,
                   dropdownButton(	
                     tags$h3("Other colors"),	
                     colourInput("colorCO_hp_italy", h5("Select color CO"), value = "#c6c60f"),	
                     colourInput("colorNO2_hp_italy", h5("Select color NO2"), value = "#13c649"),	
                     colourInput("colorOZONE_hp_italy", h5("Select color Ozone"), value = "#0fa2af"),	
                     colourInput("colorSO2_hp_italy", h5("Select color SO2"), value = "#A877E0"),	
                     colourInput("colorPM25_hp_italy", h5("Select color PM2.5"), value = "#cc8112"),	
                     colourInput("colorPM10_hp_italy", h5("Select color PM10"), value = "#ba1010"),	
                     circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",	
                     tooltip = tooltipOptions(title = "Click to open")	
                   ),
                   
                   div(column(12,
                              colourInput("backgroundColor_hp_italy", h3("Select color"), value = "#005669"),	
                              
                              selectizeInput("CitySearch_hp_italy", label = h4("Search City"), sort(cities_italy), selected = "roma", multiple = FALSE, options = NULL),
                              selectizeInput(inputId = "H_year_italy", "Select Year", H_years_italy, selected = '2019',width = "200%",multiple = FALSE, options = NULL),
                              selectizeInput(inputId = "H_month_italy", "Select Month", H_months, selected = 'January',width = "200%",multiple = FALSE, options = NULL),
                              selectizeInput(inputId = "H_day_italy", "Select Day", H_days, selected = '1',width = "200%",multiple = FALSE, options = NULL)
                   ),class = "boxtozoom")
      )),
      column(10,plotOutput("hourly_data_italy",height = "85vmin"),checkboxGroupButtons(
        inputId = "hourly_data_italy",
        choices = c("NO2","CO", "SO2","Ozone","PM2.5","PM10"),
        justified = TRUE, status = "primary", selected = c("NO2","Ozone","SO2"),size = "lg",
        checkIcon = list(yes = icon("ok-sign", lib = "glyphicon"), no = icon("remove-sign", lib = "glyphicon"))
    ))
    )),

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
                      zoom_level = 5,
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
      v$zoom_level <<- 6
      v$tooltip_width <<- 180
      v$tooltip_height <<- 80
      v$tooltip_text_size <<- 28
      v$line_size <<- 5
      v$tbl_pagelength <<- 20
      v$annotate_text_size <<- 8
      v$marker_text_size <<- '60px'
      v$select_input_width <<- '200%'
      v$width_daily = "100%"
      v$height_daily = 1200
      v$daily_axis_stroke = 4
      v$daily_axis_labels = 30
      v$daily_axis_title = 50
      v$daily_legend_font = 30
      v$daily_legend_size = 100
      v$daily_point_size <- 700 #not used
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
      v$zoom_level = 5
      v$tooltip_width = 100
      v$tooltip_hieght = 60
      v$tooltip_text_size = 14
      v$line_size = 1
      v$tbl_pagelength = 20
      v$annotate_text_size = 4
      v$marker_text_size = '12px'
      v$select_input_width = '100%'
      v$width_daily = "100%"
      v$height_daily = 700
      v$daily_axis_stroke = 3
      v$daily_axis_labels = 10
      v$daily_axis_title = 20
      v$daily_legend_font = 10
      v$daily_point_size <- 5 #not used
      v$daily_legend_size = 30
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
  
  
  ###NEEDS MODIFICATION
  observeEvent(priority = 10,input$location_italy,{
    selected_state_data <- subset(dataset, State == input$State)
    counties_in_state <- unique(selected_state_data$County)
    
    updateSelectInput(session, inputId = "County", choices = counties_in_state)
    county <- input$County
    
  })
  
  observeEvent(priority = 10,input$H_year,{
    year_sub <- subset(hourly_df, `State Name` == selected_state_hp() & Year == input$H_year)
    months <- unique(year_sub$Month)
    
    updateSelectInput(session, inputId = "H_month", choices = months)
    # county <- input$County
    
  })
  
  observeEvent(priority = 10,input$H_year_italy,{
    year_sub <- subset(hourly_df_italy, `City` == selected_city_hp_italy() & Year == input$H_year_italy)
    months <- unique(year_sub$Month)
    updateSelectInput(session, inputId = "H_month_italy", choices = months)
    # county <- input$County
    
  })
  
  observeEvent(priority = 10,input$H_month,{
    month_sub <- subset(hourly_df, `State Name` == selected_state_hp() & Year == input$H_year & Month == input$H_month)
    days <- unique(month_sub$Day)
    
    updateSelectInput(session, inputId = "H_day", choices = days)
  })
  
  observeEvent(priority = 10,input$H_month_italy,{
    month_sub <- subset(hourly_df_italy, Year == input$H_year_italy & Month == input$H_month_italy)
    days <- unique(month_sub$Day)
    
    updateSelectInput(session, inputId = "H_day_italy", choices = days)
  })
  
  # observeEvent(priority = 10,input$pollutant_map,{
  #   selected_state_data <- subset(daily_df, State == input$State)
  #   counties_in_state <- unique(selected_state_data$County)
  #
  #   updateSelectInput(session, inputId = "County", choices = counties_in_state)
  #   county <- input$County
  #
  # })
  
  observeEvent(priority = 10,input$D_month,{
    month_sub <- subset(daily_all, Year == 2018 & Month == input$D_month)
    days <- unique(month_sub$Day)
    days <- sort(days)
    
    updateSelectInput(session, inputId = "D_day", choices = days)
  })
  
  observeEvent(priority = 10,input$switch_daily,{
    if(input$switch_daily){
      updateSelectInput(session, inputId = "pollutant_map", choices = pollutants_2)
    } else {
      updateSelectInput(session, inputId = "pollutant_map", choices = c(pollutants_2,"AQI"))
    }
    
  })
  
  selected_state <- reactive({
    strsplit(input$CountySearch," - ")[[1]][2]
  })
  
  selected_county <- reactive({
    strsplit(input$CountySearch," - ")[[1]][1]
  })
  
  selected_state_hp <- reactive({
    strsplit(input$CountySearch_hp," - ")[[1]][2]
  })
  
  selected_county_hp <- reactive({
    strsplit(input$CountySearch_hp," - ")[[1]][1]
  })
  
  selected_city_hp_italy <- reactive({
    input$CitySearch_hp_italy
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
  
  output$sel_state_hp <- renderText({
    selected_state_hp()
  })
  
  output$sel_county_hp <- renderText({
    selected_county_hp()
  })
  output$year_hp <- renderText({
    "2018"
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
    s_county<-subset(dataset, State == selected_state() & County == selected_county() & Year > input$range[1] & Year < input$range[2])
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
    site<-subset(sites, sites$`State Name` == selected_state() & sites$`County Name` == selected_county())
    
    latit <- site$Latitude
    latit <- latit[latit!=0] # Eliminating 0 values
    latit <- latit[!is.na(latit)] # Eliminating NAs
    computed_lat <- mean(latit)
    longit <- site$Longitude
    longit <- longit[longit!=0] # Eliminating 0 values
    longit <- longit[!is.na(longit)] # Eliminating NAs
    computed_lng <- mean(longit)
    
    leaflet(value(f_xy)) %>%
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
  
  
  # Daily AQI for selected year - PART C
  daily_aqi_line_react <- reactive({
    
    months = c("January","February","March","April","May","June","July","August","September","October","November","December")
    a <- subset(daily_df,year== input$Year & county==input$County & state==input$State)
    a$month <- match(a$month,months)
    a$date <- as.Date(with(a, paste(year, day, month,sep="-")), "%Y-%d-%m")
    a = a[order(as.Date(a$date, format="%Y-%m-%d")),]
    if(length(a$category)==0)
    {
      shinyalert("Oops!", paste("No data for",input$County," in year ",input$Year), type = "error")
      a<-data.frame(date = NaN, aqi = NaN, pollutant= "No data")
      a
    }
    else{
      # a$color <- ifelse(a$pollutant == "CO", "#c6c60f", 
      #                        ifelse(a$pollutant == "NO2", "#13c649", 
      #                               ifelse(a$pollutant == "Ozone", "#0fa2af",ifelse(a$pollutant == "SO2", "#5610a8",ifelse(a$pollutant == "PM2.5", "#cc8112",ifelse(a$pollutant == "PM10", "#ba1010",""))))))
      a$pollutant<-factor(a$pollutant, levels=c("Ozone","CO","NO2","PM2.5","PM10","SO2"))
      a
    }
    
  })
  all_values <- function(x) {
    print(x)
    if(is.null(x)) return(NULL)
    if(length(x$pollutant)==0) return(NULL)
    # if(names(x)=="aqi")
    # paste0("AQI: ",x,collapse = "<br />")
    # else if(names(x)=="date")
    # paste0("Date: ",as.Date(as.POSIXct(x$date/1000, origin="1970-01-01")),collapse = "<br />")
    # else
    x$date = as.Date(as.POSIXct(x$date/1000, origin="1970-01-01"))
    names(x) = c("Date","AQI","Major pollutant")
    paste0("<h4>",names(x), ": ", format(x), collapse = "</h4><br />")
  }
  
  observe({
    daily_aqi_line_react %>%
      ggvis(x=~date, y=~aqi,stroke=~pollutant) %>%
      layer_points(fill= ~pollutant, size := 700) %>%
      layer_lines(stroke= ~pollutant, strokeWidth := 10) %>%
      scale_nominal("fill", range = c("#c6c60f","#13c649","#0fa2af","#5610a8","#cc8112","#ba1010")) %>%
      scale_nominal("stroke",range=c("#c6c60f","#13c649","#0fa2af","#5610a8","#cc8112","#ba1010")) %>%
      add_tooltip(all_values, "click") %>%
      #layer_paths(stroke= ~pollutant, strokeWidth := 10) %>%
      set_options(width=v$width_daily,height=v$height_daily)  %>%
      add_axis("x", title = "Month", properties = axis_props(
        axis = list(stroke = "white",strokeWidth = v$daily_axis_stroke),
        labels = list(stroke = "white",fill="white",align = "left", fontSize = v$axis_text_size),
        title = list(stroke = "white",fill="white",fontSize = v$axis_title_size)
      )) %>%
      add_axis("y", title_offset = 50,offset=0,title = "AQI value", properties = axis_props(
        axis = list(stroke = "white",strokeWidth = v$daily_axis_stroke),
        labels = list(stroke = "white",fill="white",align = "left", fontSize = v$axis_text_size),
        title = list(stroke = "white",fill="white",fontSize = v$axis_title_size)
      )) %>%
      hide_legend('stroke')  %>% 
      add_legend("fill",title="", properties=legend_props(
        labels=list(fontSize=v$daily_legend_font),symbols=list(size=v$daily_legend_size))) %>%
      bind_shiny("daily_aqi_line", "plot_ui")
  })
  
  # Daily AQI for ITALY - GRAD PART
  daily_aqi_line_italy_react <- reactive({
    months = c("January","February","March","April","May","June","July","August","September","October","November","December")
    a <- subset(italy_df,city==input$CitySearch)
    a$date <- as.Date(with(a, paste(year, day, month,sep="-")), "%y-%d-%m")
    a = a[order(as.Date(a$date, format="%Y-%m-%d")),]
    if(length(a$co)==0)
      shinyalert("Oops!", paste("No data for",input$CitySearch," in year "), type = "error")
    else{
      pollutant_input <- c()
      if ("CO" %in% input$daily_data_italy){
        pollutant_input <- c(pollutant_input,"CO")
      }
      if ("NO2" %in% input$daily_data_italy){
        pollutant_input <- c(pollutant_input,"NO2")
      }
      if ("Ozone" %in% input$daily_data_italy){
        pollutant_input <- c(pollutant_input,"Ozone")
      }
      if ("SO2" %in% input$daily_data_italy){
        pollutant_input <- c(pollutant_input,"SO2")
      }
      if ("PM2.5" %in% input$daily_data_italy){
        pollutant_input <- c(pollutant_input,"PM2.5")
      }
      if ("PM10" %in% input$daily_data_italy){
        pollutant_input <- c(pollutant_input,"PM10")
      }
      names(a) <- c("city","day","month","year","NO2","SO2","CO","PM10","PM2.5","Ozone","date")
      print(class(pollutant_input))
      b <- gather_(a, "pollutant", "value", pollutant_input)
      print(b)
      b
    }
    
  })
  all_values_italy <- function(x) {
    print(x)
    if(is.null(x)) return(NULL)
    if(length(x$pollutant)==0) return(NULL)
    # if(names(x)=="aqi")
    # paste0("AQI: ",x,collapse = "<br />")
    # else if(names(x)=="date")
    # paste0("Date: ",as.Date(as.POSIXct(x$date/1000, origin="1970-01-01")),collapse = "<br />")
    # else
    x$date = as.Date(as.POSIXct(x$date/1000, origin="1970-01-01"))
    names(x) = c("Date","AQI","Major pollutant")
    paste0("<h4>",names(x), ": ", format(x), collapse = "</h4><br />")
  }
  observe({
    daily_aqi_line_italy_react %>%
      ggvis(x=~date, y=~value, stroke=~pollutant) %>%
      # layer_points(fill= ~pollutant, size := 700) %>%
      scale_nominal("fill", range = c("#c6c60f","#13c649","#0fa2af","#5610a8","#cc8112","#ba1010")) %>%
      add_tooltip(all_values_italy, "click") %>%
      layer_lines()  %>%
      layer_points() %>%
      set_options(width=v$width_daily,height=v$height_daily)  %>%
      add_axis("x", title = "Month", properties = axis_props(
        axis = list(strokeWidth = v$daily_axis_stroke),
        labels = list(align = "left", fontSize = v$axis_text_size),
        title = list(fontSize = v$axis_title_size)
      )) %>%
      #modify below line for imperial unit
      add_axis("y", offset=10,title = "Pollutant value", properties = axis_props(
        axis = list(strokeWidth = v$daily_axis_stroke),
        labels = list(align = "left", fontSize = v$axis_text_size),
        title = list(fontSize = v$axis_title_size)
      )) %>%
      add_legend("fill",title="", properties=legend_props(
        labels=list(fontSize=v$daily_legend_font),symbols=list(size=v$daily_legend_size))) %>%
      bind_shiny("daily_aqi_line_italy", "plot_ui")
  })
  
  # Stacked bar chart for PART C
  output$daily_bar <- renderPlot({
    
    p1 <- subset(daily_df,year== input$Year & county==input$County & state==input$State)
    if(length(p1$category)==0)
    {
      shinyalert("Oops!", paste("No data for",input$County," in year ",input$Year), type = "error")
      emp <- data.frame()
      ggplot(emp)+annotate("text", x=0, y=0, label= "",size=20) +theme(
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
      )
    }
    else{
      
      df = data.frame(Month=character(),good=integer(0),mod=integer(0),uhs=integer(0),uh=integer(0),vu=integer(0),haz=integer(0),unknown=integer(0))
      names(df) = c("Month","Good","Moderate","Unhealthy for Sensitive Groups","Unhealthy","Very Unhealthy","Hazardous","Unknown")
      months = c("January","February","March","April","May","June","July","August","September","October","November","December")
      month_days = c(31,28,31,30,31,30,31,31,30,31,30,31) #HORRIBLE hardcoding but hey as long as it works
      for(i in 1:12)
      {
        month1 = months[i]
        
        p2 <- subset(p1,month==month1)
        df_row = c(months[i],0,0,0,0,0,0,0)
        
        t1 <- nrow(subset(p2,category=="Good"))
        t2 <- nrow(subset(p2,category=="Moderate"))
        
        t3 <- nrow(subset(p2,category=="Unhealthy for Sensitive Groups"))
        t4 <- nrow(subset(p2,category=="Unhealthy"))
        t5 <- nrow(subset(p2,category=="Very Unhealthy"))
        t6 <- nrow(subset(p2,category=="Hazardous"))
        t7 <- month_days[i] - (t1+t2+t3+t4+t5+t6)
        
        df_row = data.frame(months[i],t1,t2,t3,t4,t5,t6,t7)
        names(df_row) = c("Month","Good","Moderate","Unhealthy for Sensitive Groups","Unhealthy","Very Unhealthy","Hazardous","Unknown")
        df <- rbind(df,df_row)
      }
      
      DF1 <- melt(df, id.var="Month")
      
      p <- ggplot(data = DF1, aes(x = Month, y = value, fill=variable)) + geom_bar(stat="identity")+ scale_fill_manual("AQI Category", values = c("#01665e","#5ab4ac","#c7eae5","#f6e8c3","#d8b365","#8c510a","#C0C0C0"))+
        theme(
          text = element_text(size=12)
        ) + labs(x = "Month", y = "Number of days") +
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
          axis.text = element_text(size = axis_text_size(),color = "black"),
          axis.title = element_text(size = axis_title_size()),
          legend.title = element_text(size = legend_title_size())
        )
      p
    }
    
  })
  
  # Time series of Hourly Data -- ITALY GRAD PART
  output$hourly_data_italy <- renderPlot({
    s_county_italy<-subset(hourly_df_italy, hourly_df_italy$`City` == selected_city_hp_italy() & hourly_df_italy$Month == input$H_month_italy & hourly_df_italy$Day == input$H_day_italy)
    
    if(length(s_county_italy$`Time`) > 0 ){
      gl <- ggplot(data = s_county_italy, aes(x = s_county_italy$`Time`)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(color = "#FFFFFF"),
          axis.title.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(color = NA, fill = input$backgroundColor_hp_italy),	
          legend.background = element_rect(color = NA, fill = input$backgroundColor_hp_italy),	
          legend.key = element_rect(color = NA, fill = input$backgroundColor_hp_italy),	
          panel.background = element_rect(fill = input$backgroundColor_hp_italy, color  =  NA),	
          panel.grid.major = element_line(color = input$textColor_hp_italy),	
          panel.grid.minor = element_line(color = input$textColor_hp_italy),	
          legend.text = element_text(size = legend_text_size(), color = input$textColor_hp_italy),	
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size(), color = input$textColor_hp_italy),	
          axis.title = element_text(size = axis_title_size()),	
          legend.title = element_text(size = legend_title_size(), color = input$textColor_hp_italy)	
        )+labs(x = "Hours", y = "Measurement of Hourly Data")
      
      labs <-c()
      vals <-c()
      if ("CO" %in% input$hourly_data_italy){
        suffx_CO = "(ppm)"
        labs <-c(labs,"CO" = paste("CO",suffx_CO, sep=" "))
        vals <-c(vals,c("CO" = input$colorCO_hp_italy))
        gl <- gl + geom_line(aes(y = CO, color = "CO"), size = line_size(), group = 1) +
          geom_point(aes(y = CO, color = "CO"), size = line_size()*3)
      }
      if ("NO2" %in% input$hourly_data_italy){
        suffx_NO2 = "(ppb)"
        labs <-c(labs,"NO2" = paste("NO2",suffx_NO2, sep=" "))
        vals <-c(vals,"NO2" = input$colorNO2_hp_italy)
        gl <- gl + geom_line(aes(y = NO2, color = "NO2"), size = line_size(), group = 2) +
          geom_point(aes(y = NO2, color = "NO2"), size = line_size()*3)
      }
      if ("Ozone" %in% input$hourly_data_italy){
        suffx_Ozone = "(ppm)"
        labs <-c(labs,"Ozone" = paste("Ozone",suffx_Ozone, sep=" "))
        vals <-c(vals,"Ozone" = input$colorOZONE_hp_italy)
        gl <- gl+geom_line(aes(y = O3, color = "Ozone"), size = line_size(), group = 3) +
          geom_point(aes(y = O3, color = "Ozone"), size = line_size()*3)
      }
      if ("SO2" %in% input$hourly_data_italy){
        suffx_SO2 = "(ppb)"
        labs <-c(labs,"SO2"=paste("SO2",suffx_SO2, sep=" "))
        vals <-c(vals,"SO2" = input$colorSO2_hp_italy)
        gl <- gl +geom_line(aes(y = SO2, color = "SO2"), size = line_size(), group = 4) +
          geom_point(aes(y = SO2, color = "SO2"), size = line_size()*3)
      }
      convert_to_imperial <- function(values){
        return(values*1000000000000* 0.000000035274/35315)
      }
      
      if ("PM2.5" %in% input$hourly_data_italy){
        if(input$switch_units){
          s_county_italy$data_conv <-s_county_italy$"PM2.5"
          s_county_italy$data_conv <- convert_to_imperial(s_county_italy$data_conv)
          names(s_county_italy)[names(s_county_italy)=="data_conv"] <- paste("PM2.5","conv",sep="_")
          suffx_PM2.5 = "(e-12 oz/ft3)"
          gl <- gl + geom_line(aes(y = s_county_italy$PM2.5_conv, color = "PM2.5"), size = line_size(), group = 5)+
            geom_point(aes(y = s_county_italy$PM2.5_conv, color = "PM2.5"), size = line_size()*3)
        }
        else{
          gl <- gl + geom_line(aes(y = s_county_italy$PM2.5, color = "PM2.5"), size = line_size(), group = 5)+
            geom_point(aes(y = s_county_italy$PM2.5, color = "PM2.5"), size = line_size()*3)
          suffx_PM2.5 = "(ug/m3)"
        }
        labs <-c(labs,"PM2.5"=paste("PM2.5",suffx_PM2.5, sep=" "))
        vals <-c(vals,"PM2.5" = input$colorPM25_hp_italy)
        
      }
      if ("PM10" %in% input$hourly_data_italy){
        if(input$switch_units){
          s_county_italy$data_conv <-s_county_italy$"PM10"
          s_county_italy$data_conv <- convert_to_imperial(s_county_italy$data_conv)
          names(s_county_italy)[names(s_county_italy)=="data_conv"] <- paste("PM10","conv",sep="_")
          suffx_PM10 = "(e-12 oz/ft3)"
          gl <- gl + geom_line(aes(y = s_county_italy$PM10_conv, color = "PM10"), size = line_size(), group = 6) +
            geom_point(aes(y = s_county_italy$PM10_conv, color = "PM10"), size = line_size()*3)
        }
        else{
          suffx_PM10 = "(ug/m3)"
          gl <- gl + geom_line(aes(y = s_county_italy$PM10, color = "PM10"), size = line_size(), group = 6) +
            geom_point(aes(y = s_county_italy$PM10, color = "PM10"), size = line_size()*3)
          
        }
        labs <-c(labs,"PM10"= paste("PM10",suffx_PM10, sep=" "))
        vals <-c(vals,"PM10" = input$colorPM10_hp_italy)
        
      }
      gl <- gl + scale_color_manual(name = "Measurements",labels=labs,
                                    values = vals)
      gl
      # scale_x_continuous(breaks = round(seq(max(min(s_county$`Time Local`),1), min(max(s_county$`Time Local`),24), by = 1),1)) +
      # scale_y_continuous(breaks = round(seq(min(s_county[4:9]), max(s_county[4:9]), by = 10),1))
      
    }
    # Signaling missing data
    else {
      shinyalert("Oops!", "No data for this County for this day", type = "error")
    }
    
  })
  
  # table of daily aqi
  output$daily_aqi_table <- DT::renderDataTable({
    p1 <- subset(daily_df,year== input$Year & county==input$County & state==input$State)
    if(length(p1$category)==0)
      shinyalert("Oops!", paste("No data for",input$County," in year ",input$Year), type = "error")
    else{
      df = data.frame(Month=character(),good=integer(0),mod=integer(0),uhs=integer(0),uh=integer(0),vu=integer(0),haz=integer(0),unknown=integer(0))
      names(df) = c("Month","Good","Moderate","Unhealthy for Sensitive Groups","Unhealthy","Very Unhealthy","Hazardous","Unknown")
      months = c("January","February","March","April","May","June","July","August","September","October","November","December")
      month_days = c(31,28,31,30,31,30,31,31,30,31,30,31) #HORRIBLE hardcoding but hey as long as it works
      for(i in 1:12)
      {
        #get monthly data
        month1 = months[i]
        p2 <- subset(p1,month==month1)
        df_row = c(months[i],0,0,0,0,0,0,0)
        
        t1 <- nrow(subset(p2,category=="Good"))
        t2 <- nrow(subset(p2,category=="Moderate"))
        
        t3 <- nrow(subset(p2,category=="Unhealthy for Sensitive Groups"))
        t4 <- nrow(subset(p2,category=="Unhealthy"))
        t5 <- nrow(subset(p2,category=="Very Unhealthy"))
        t6 <- nrow(subset(p2,category=="Hazardous"))
        t7 <- month_days[i] - (t1+t2+t3+t4+t5+t6)
        
        df_row = data.frame(months[i],t1,t2,t3,t4,t5,t6,t7)
        # print(df_row)
        names(df_row) = c("Month","Good","Moderate","Unhealthy for Sensitive Groups","Unhealthy","Very Unhealthy","Hazardous","Unknown")
        df <- rbind(df,df_row)
      }
      df}
  },options = list(searching = FALSE,paging = FALSE,
                   dom = 't'))
  
  
  translate_to_column_name <- function(pollutant) {
    if(pollutant == "CO"){
      return("Days.CO")
    } else if (pollutant == "NO2"){
      return("Days.NO2")
    } else if (pollutant == "Ozone"){
      return("Days.Ozone")
    } else if (pollutant == "SO2"){
      return("Days.SO2")
    } else if (pollutant == "PM2.5"){
      return("Days.PM2.5")
    } else if (pollutant == "PM10"){
      return("Days.PM10")
    } else if (pollutant == "AQI"){
      return("Median.AQI")
    }
    
    return("Days.CO")
  }
  
  convert_to_imperial <- function(values){
    return(values*1000000000000* 0.000000035274/35315)
  }
  
  # Mirko
  # !Important
  # The round county numbers updates continously invalidating the input for as many times as the there are
  # in the difference between the starting point and the final point, recalculating so many times and
  # make the app unusable. I will try to use a reactive value to compute the county number input based on
  # the round number picker and use debounce (or throttle) to delay the execution for a number of ms needed
  # for the user to choose the right number
  
  delayes_num_counties <- reactive({
    input$num_counties
  })
  
  delayes_num_counties_debounced <- delayes_num_counties %>% debounce(300)
  
  # MAP rendering
  output$map_controllers <- renderLeaflet({
    feature <- translate_to_column_name(input$pollutant_map)
    # value = c((current()$Days.with.AQI-current()$Days.Ozone)/current()$Days.with.AQI*100, current()$Days.Ozone/current()$Days.with.AQI*100)
    
    if(!input$switch_daily){ # Yearly
      sub<-subset(dataset, Year == input$year_map)
      if(feature !="Median.AQI"){
        sub$sel_feat<-sub[[feature]]/sub$Days.with.AQI*100
        suffx = "%"
      } else {
        sub$sel_feat<-sub[[feature]]
        suffx = ""
      }
    } else { # Daily
      sub<-subset(daily_all, Month == input$D_month & Day == input$D_day)
      sub$sel_feat<-sub[[input$pollutant_map]]
      if(input$switch_units){
        if(input$pollutant_map == "PM2.5" || input$pollutant_map == "PM10"){
          sub$sel_feat <- convert_to_imperial(sub$sel_feat)
          suffx = "e-12 oz/ft3"
        }
      } else {
        if(input$pollutant_map == "PM2.5" || input$pollutant_map == "PM10"){
          suffx = " ug/m3"
        }
      }
      if(input$pollutant_map == "CO" || input$pollutant_map == "Ozone"){
        suffx = " ppm"
      } else if(input$pollutant_map == "SO2" || input$pollutant_map == "NO2"){
        suffx = " ppb"
      }
    }
    
    sub <- sub[order(sub$sel_feat,decreasing = TRUE),]
    df <- head(sub,delayes_num_counties_debounced())
    
    if(!input$switch_daily){ # Yearly
      temp <- merge(value(f_xy), df,
                    by.x = c("STATENAME","NAME"), by.y = c("State","County"),
                    all.x = TRUE)
    } else { # Daily
      temp <- merge(value(f_xy), df,
                    by.x = c("STATENAME","NAME"), by.y = c("State Name","County Name"),
                    all.x = TRUE)
    }
    
    
    # Create a color palette
    mypal <- colorNumeric(palette = "viridis", reverse = TRUE, domain = temp$sel_feat
                          ,na.color = "#ffffff11"
    )
    
    pop <- if(!input$switch_daily) ~paste(sep = "<br/>",
                                          paste("<b><a href='https://en.wikipedia.org/wiki/",value(f_xy)$NAME,"_County,_",value(f_xy)$STATENAME,"' target='_blank'>",value(f_xy)$NAME," on Wikipedia</a></b>"),
                                          value(f_xy)$NAME,
                                          value(f_xy)$STATENAME,
                                          paste("Confidence level:",temp$Days.with.AQI/365*100, "%"),
                                          paste("Days with data:",temp$Days.with.AQI),
                                          paste(signif(temp$sel_feat,3),suffx)
    )
    else
      ~paste(sep = "<br/>",
             paste("<b><a href='https://en.wikipedia.org/wiki/",value(f_xy)$NAME,"_County,_",value(f_xy)$STATENAME,"' target='_blank'>",value(f_xy)$NAME," on Wikipedia</a></b>"),
             value(f_xy)$NAME,
             value(f_xy)$STATENAME,
             paste(signif(temp$sel_feat,3),suffx)
      )
    
    
    spread <- function(num){
      sp <- input$Opacity
      return(if(sp < 0.4) ((num+sp)*(num+sp)-sp*sp)/(1+sp*2)-0.2+sp else ((num+sp)*(num+sp)-sp*sp)/(1+sp*2)+sp-0.2)
    }
    
    
    # factpal <- colorQuantile("Blues", ccc, n=20)
    # year_map, pollutant_map
    leaflet() %>%
      # addPolygons(data = USA, color = ~factpal(ccc), weight = 0.8, smoothFactor = 0.2,
      addPolygons(data = value(f_xy), color = ~mypal(temp$sel_feat), weight = 0.8, smoothFactor = 0.2,
                  opacity = spread(temp$Days.with.AQI/365+0.2), fillOpacity = spread(temp$Days.with.AQI/365+0.2),#opacity will be a param
                  label = ~htmlEscape(value(f_xy)$NAME),
                  popup = pop,
                  # fillColor = ~colorQuantile("YlOrRd"),
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE)) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = zoom_level()) %>%
      addLegend(position = "bottomright", pal = mypal, values = temp$sel_feat,
                title = "Legend",
                labFormat = labelFormat(suffix = suffx,
                                        digits = 3
                ),
                opacity = 1)
  })
  
  # Time series of Hourly Data
  output$hourly_data <- renderPlot({
    s_county<-subset(hourly_df, hourly_df$`State Name` == selected_state_hp() & hourly_df$`County Name` == selected_county_hp() & hourly_df$Month == input$H_month & hourly_df$Day == input$H_day)
    
    if(length(s_county$`Time Local`) > 0 ){
      gl <- ggplot(data = s_county, aes(x = s_county$`Time Local`)) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(color = "#FFFFFF"),
          axis.title.x = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(color = NA, fill = input$backgroundColor_hp),	
          legend.background = element_rect(color = NA, fill = input$backgroundColor_hp),	
          legend.key = element_rect(color = NA, fill = input$backgroundColor_hp),	
          panel.background = element_rect(fill = input$backgroundColor_hp, color  =  NA),	
          panel.grid.major = element_line(color = input$textColor_hp),	
          panel.grid.minor = element_line(color = input$textColor_hp),	
          legend.text = element_text(size = legend_text_size(), color = input$textColor_hp),	
          legend.key.size = unit(legend_key_size(), 'line'),
          axis.text = element_text(size = axis_text_size(), color = input$textColor_hp),	
          axis.title = element_text(size = axis_title_size()),	
          legend.title = element_text(size = legend_title_size(), color = input$textColor_hp)	
        )+labs(x = "Hours", y = "Measurement of Hourly Data")
      
      labs <-c()
      vals <-c()
      if ("CO" %in% input$hourly_data){
        suffx_CO = "(ppm)"
        labs <-c(labs,"CO" = paste("CO",suffx_CO, sep=" "))
        vals <-c(vals,c("CO" = input$colorCO_hp))
        gl <- gl + geom_line(aes(y = CO, color = "CO"), size = line_size(), group = 1) +
          geom_point(aes(y = CO, color = "CO"), size = line_size()*3)
      }
      if ("NO2" %in% input$hourly_data){
        suffx_NO2 = "(ppb)"
        labs <-c(labs,"NO2" = paste("NO2",suffx_NO2, sep=" "))
        vals <-c(vals,"NO2" = input$colorNO2_hp)
        gl <- gl + geom_line(aes(y = NO2, color = "NO2"), size = line_size(), group = 2) +
          geom_point(aes(y = NO2, color = "NO2"), size = line_size()*3)
      }
      if ("Ozone" %in% input$hourly_data){
        suffx_Ozone = "(ppm)"
        labs <-c(labs,"Ozone" = paste("Ozone",suffx_Ozone, sep=" "))
        vals <-c(vals,"Ozone" = input$colorOzone_hp)
        gl <- gl+geom_line(aes(y = Ozone, color = "Ozone"), size = line_size(), group = 3) +
          geom_point(aes(y = Ozone, color = "Ozone"), size = line_size()*3)
      }
      if ("SO2" %in% input$hourly_data){
        suffx_SO2 = "(ppb)"
        labs <-c(labs,"SO2"=paste("SO2",suffx_SO2, sep=" "))
        vals <-c(vals,"SO2" = input$colorSO2_hp)
        gl <- gl +geom_line(aes(y = SO2, color = "SO2"), size = line_size(), group = 4) +
          geom_point(aes(y = SO2, color = "SO2"), size = line_size()*3)
      }
      convert_to_imperial <- function(values){
        return(values*1000000000000* 0.000000035274/35315)
      }
      
      if ("PM2.5" %in% input$hourly_data){
        if(input$switch_units){
          s_county$data_conv <-s_county$"PM2.5"
          s_county$data_conv <- convert_to_imperial(s_county$data_conv)
          names(s_county)[names(s_county)=="data_conv"] <- paste("PM2.5","conv",sep="_")
          suffx_PM2.5 = "(e-12 oz/ft3)"
          gl <- gl + geom_line(aes(y = s_county$PM2.5_conv, color = "PM2.5"), size = line_size(), group = 5)+
            geom_point(aes(y = s_county$PM2.5_conv, color = "PM2.5"), size = line_size()*3)
        }
        else{
          gl <- gl + geom_line(aes(y = s_county$PM2.5, color = "PM2.5"), size = line_size(), group = 5)+
            geom_point(aes(y = s_county$PM2.5, color = "PM2.5"), size = line_size()*3)
          suffx_PM2.5 = "(ug/m3)"
        }
        labs <-c(labs,"PM2.5"=paste("PM2.5",suffx_PM2.5, sep=" "))
        vals <-c(vals,"PM2.5" = input$colorPM25_hp)
        
      }
      if ("PM10" %in% input$hourly_data){
        if(input$switch_units){
          s_county$data_conv <-s_county$"PM10"
          s_county$data_conv <- convert_to_imperial(s_county$data_conv)
          names(s_county)[names(s_county)=="data_conv"] <- paste("PM10","conv",sep="_")
          suffx_PM10 = "(e-12 oz/ft3)"
          gl <- gl + geom_line(aes(y = s_county$PM10_conv, color = "PM10"), size = line_size(), group = 6) +
            geom_point(aes(y = s_county$PM10_conv, color = "PM10"), size = line_size()*3)
        }
        else{
          suffx_PM10 = "(ug/m3)"
          gl <- gl + geom_line(aes(y = s_county$PM10, color = "PM10"), size = line_size(), group = 6) +
            geom_point(aes(y = s_county$PM10, color = "PM10"), size = line_size()*3)
          
        }
        labs <-c(labs,"PM10"= paste("PM10",suffx_PM10, sep=" "))
        vals <-c(vals,"PM10" = input$colorPM10_hp)
        
      }
      convert_temp_to_imperial <- function(values){
        return((values-32)/1.8)
      }
      if ("Temperature" %in% input$hourly_data){
        if(input$switch_units){
          s_county$data_conv <-s_county$"Temperature"
          s_county$data_conv <- convert_temp_to_imperial(s_county$data_conv)
          names(s_county)[names(s_county)=="data_conv"] <- paste("Temperature","conv",sep="_")
          temp_suffx = "(Degrees Celsius)"
          gl <- gl + geom_line(aes(y = s_county$Temperature_conv, color = "Temperature"), size = line_size(), group = 7) +
            geom_point(aes(y = s_county$Temperature_conv, color = "Temperature"), size = line_size()*3)
        }
        else{
          temp_suffx = "(Degrees Fahrenheit)"
          gl <- gl + geom_line(aes(y = Temperature, color = "Temperature"), size = line_size(), group = 7) +
            geom_point(aes(y = Temperature, color = "Temperature"), size = line_size()*3)
        }
        labs <-c(labs,"Temperature"= paste("Temperature",temp_suffx, sep=" "))
        vals <-c(vals,"Temperature" = input$colorTemp_hp)
        
      }
      convert_wind_to_imperial <- function(values){
        return(values*0.51)
      }
      if ("Wind Speed" %in% input$hourly_data){
        if(input$switch_units){
          s_county$data_conv <-s_county$"Wind Speed"
          s_county$data_conv <- convert_wind_to_imperial(s_county$data_conv)
          names(s_county)[names(s_county)=="data_conv"] <- paste("Wind","conv",sep="_")
          wind_suffx = "(m/s)"
          gl <- gl + geom_line(aes(y = s_county$Wind_conv, color = "Wind Speed"), size = line_size(), group = 8) +
            geom_point(aes(y = s_county$Wind_conv, color = "Wind Speed"), size = line_size()*3)
        }
        else{
          wind_suffx = "(knots)"
          gl <- gl + geom_line(aes(y = s_county$`Wind Speed`, color = "Wind Speed"), size = line_size(), group = 8) +
            geom_point(aes(y = s_county$`Wind Speed`, color = "Wind Speed"), size = line_size()*3)
        }
        labs <-c(labs,"Wind Speed" = paste("Wind Speed",wind_suffx, sep=" "))
        vals <-c(vals,"Wind Speed" = input$colorWS_hp)
      }
      gl <- gl + scale_color_manual(name = "Measurements",labels=labs,
                                    values = vals)
      gl
      # scale_x_continuous(breaks = round(seq(max(min(s_county$`Time Local`),1), min(max(s_county$`Time Local`),24), by = 1),1)) +
      # scale_y_continuous(breaks = round(seq(min(s_county[4:9]), max(s_county[4:9]), by = 10),1))
      
    }
    # Signaling missing data
    else {
      shinyalert("Oops!", "No data for this County for this day", type = "error")
    }
    
  })
  
  
  
  # About HTML
  output$about_out <- renderUI({
    author <- "<h1>Mirko Mantovani - Ashwani Khemani - Abhishek Vasudevan</h1>
    
    <a href='https://mirkomantovani.com/projects/EveryBreathYouTake.html'>Project webpage</a>
    <br/>
    <a href='https://github.com/mirkomantovani/VisualAnalytics-EveryBreathYouTake'>Github repository</a><br>"
    libraries <- "<b>Used R libraries: </b> <br>
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
    <li>viridis</li>
    <li>cdlTools</li>
    <li>htmltools</li>
    <li>plotly</li>
    <li>RColorBrewer</li>
    <li>reshape2</li>
    
    </ul>"
    data <- "<b>Dataset Source:</b></br> <a href='https://aqs.epa.gov/aqsweb/airdata/download_files.html'>United States Environmental Protection Agency</a><br>
    <a href='http://eric.clst.org/tech/usgeojson/e'>United States Counties shape in GeoJSON</a>"
    HTML(paste(author, libraries, data))
  })
  
  
  # End of server
}

shinyApp(ui = ui, server = server)
