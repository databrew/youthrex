library(shinydashboard)
library(sp)
library(raster)
library(plotly)
library(maptools)
library(googledrive)
library(yaml)
library(stringr)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(broom)
library(feather)
library(memisc)
library(shiny)
library(googleVis)
library(DT)
library(leaflet)
library(RColorBrewer)
library(networkD3)
options(gvis.plot.tag = 'chart')
options(scipen = 999)
library(shinyBS)
library(shinyLP)
library(ggplot2)
library(shinythemes)
library(ggthemes)
library(gridExtra)



source('global.R')


ui <- dashboardPage(skin = 'blue',
                    
                    dashboardHeader(disable = TRUE),
                    
                    dashboardSidebar(disable = TRUE),
                    
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                        fluidRow(column(12,
                                        align = 'center',
                                        h1('Welcome to the Ontario Youth Compass data portal')))
                      ),
                      
                      tags$style(HTML("

                    .box.box-solid.box-danger>.box-header{
                    color:#fff;
                    background:darkred;
                    }

                    .box.box-solid.box-danger{
                    border-bottom-color:#222d32;
                    border-left-color:#222d32;
                    border-right-color:#222d32;
                    }

                   .box.box-solid>.box-header{
                    color:#fff;
                    background:#2C3E50
                    }

                    .box.box-solid{
                    border-bottom-color:#222d32;
                    border-left-color:#222d32;
                    border-right-color:#222d32;
                    }


                    ")),
                      
                      fluidPage(
                        fluidRow(
                          shinydashboard::box(
                          title = '',
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = FALSE,
                          collapsed = FALSE,
                          column(4,
                                 tags$head(tags$style("
                                                      #location ~ .selectize-control.single .selectize-input {
                                                      border: 1px solid #000000; background-color: #D2DAE3;
                                                      
                                                      }")),
                                 selectInput('location',
                                             'Location',
                                             choices = unique(census$Geography),
                                             selected = 'Ontario')),
                          tags$head(tags$style("
                                               #location ~ .selectize-control.single .selectize-input {
                                               border: 1px solid #000000; background-color: #D2DAE3;
                                               
                                               }")),
                          column(4,
                                 selectInput('years',
                                             'Years',
                                             choices = unique(census$year),
                                             selected = unique(census$year),
                                             multiple = TRUE)),
                          
                          align = 'center',
                          box(
                            title = 'Map of Ontario',
                            solidHeader = FALSE,
                            width = 12,
                            background = 'light-blue',
                            collapsible = TRUE,
                            collapsed = TRUE,
                            column(12,
                                   align = 'center',
                                   h2("Percent of all youth age 15-29 within each census location"),
                                   splitLayout(
                                     leafletOutput('the_map', width = 900, height = 700))
                                   
                            )
                          
                          )
                          
                                 )
                          
                          ),
                        
                        # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                        
                        shinydashboard::box(
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = FALSE,
                          collapsed = FALSE,
                          
                        fluidRow(
                          shinydashboard::box(
                            title = 'Demographics',
                            status = 'primary',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            
                            fluidRow(column(12,
                                            h3('Examine demographic variables by the location and years selected above'))),
                            br(), br(),
                            fluidRow(column(6,
                                            plotlyOutput('demo_plot_pie'),
                                            DT::dataTableOutput('pie_table')),
                                     column(6,
                                            selectInput('demo_variable',
                                                        'Examine by ',
                                                        choices =  c('Sex', 
                                                                     'Place of Birth', 
                                                                     'Visible minority',
                                                                     'Aboriginal identity'),
                                                        selected = 'Sex',
                                                        multiple = FALSE),
                                            br(),
                                            uiOutput('demo_chart_avg'),
                                            uiOutput('demo_chart_table'),
                                            br(),
                                            tabsetPanel(tabPanel('Plot', 
                                                                 br(), br(),
                                                                 plotlyOutput('demo_charts')),
                                                        tabPanel('Table',
                                                                 DT::dataTableOutput('demo_tables')))))
                            
                          )
                          
                        ),

                      
                      # (2) family status
                      # - spouses and common law vs lone parents (25-29)
                      # - spouses and common law vs lone parents by vis_min (25-29)
                      # - children in couple families vs children in lone parent (15-19)
                      fluidRow(
                        shinydashboard::box(
                          title = 'Family status',
                          status = 'success',
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          fluidRow(column(12,
                                          h2('Family structure of youth as both children and parents'))),
                          br(), br(),
                          fluidRow(column(6,
                                          shinydashboard::box(
                                            title = 'Youth (25-29) as parents',
                                            status = 'success',
                                            solidHeader = TRUE,
                                            width = 12, 
                                            collapsible = TRUE,
                                            collapsed = TRUE,
                                          radioButtons('fam_type',
                                                       '',
                                                       choices = c('Lone parents', 'Spouses and common law partners'),
                                                       selected = 'Lone parents', 
                                                       inline = FALSE),
                                          uiOutput('fam_plot_parents'))),
                                   column(6,
                                          shinydashboard:: box(
                                            title = 'Youth (15-19) as children',
                                            status = 'success',
                                            solidHeader = TRUE,
                                            width = 12,
                                            collapsible = TRUE,
                                            collapsed = TRUE,
                                          plotlyOutput('fam_plot_kids')))),
                          br(), 
                          fluidRow(column(6,
                                          selectInput('which_fam_type',
                                                      'Parental type by visible minority',
                                                      choices = c('Spouses and common law partners', 'Lone parents'),
                                                      selected = 'Lone parents',
                                                      multiple = FALSE)),
                                   column(6,
                                          checkboxInput('fam_chart_table',
                                                        'View as table',
                                                        value = FALSE),
                                          checkboxInput('fam_chart_table_all_or_vm',
                                                        'Compare with non visible minority population',
                                                        value = FALSE))
                          ),
                          fluidRow(column(12,
                                          uiOutput('fam_plot_table_vismin')) 
                          )
                          
                        )
                      ),
                      
                      fluidRow(
                        shinydashboard::box(
                          title = 'Education',
                          status = 'info',
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          
                          fluidRow(
                            column(4,
                                   box(
                                     title = 'Highschool',
                                     background = 'light-blue',                                  
                                     solidHeader = FALSE,
                                     width = 12,
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     textOutput('ed_text_highschool'))
                            ),
                            column(4,
                                   box(
                                     title = 'College certificate/diploma',
                                     solidHeader = FALSE,
                                     background = 'light-blue',                                  
                                     width = 12,
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     textOutput('ed_text_college'))
                            ),
                            
                            column(4,
                                   box(
                                     title = "Univeristy degree/diploma",
                                     solidHeader = FALSE,
                                     background = 'light-blue',
                                     width = 12,
                                     collapsible =TRUE,
                                     collapsed = TRUE,
                                     textOutput('ed_text_university'))
                            )
                            
                          ),
                          fluidRow(
                            column(12,
                                   h3('Highschool or equivalent', align = 'center'))),
                        fluidRow(
                          column(6,
                                 align = 'center',
                                 checkboxInput('ed_plot_or_table_age',
                                               'View as table', 
                                               value = FALSE),
                                 uiOutput('ed_plot_table_age')),
                          column(6,
                                 align = 'center',
                                 checkboxInput('ed_plot_or_table_sex',
                                               'View as table', 
                                               value = FALSE),
                                 uiOutput('ed_plot_table_sex')))
                            
                          )  
                          
                          
                        ),
                      
                      fluidRow(
                        shinydashboard::box(
                          title = 'Employment',
                          status = 'warning',
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          fluidRow(
                            br(), br(),
                            column(12, 
                                   align = 'center',
                                   textOutput('emp_header'),
                                   tags$head(tags$style("#emp_header{color: #424744;
                                                        font-size: 20px;
                                                        font-weight: bold;
                                                        }"
                                     )
                                   ))
                                   ),
                          
                          br(),br(), br(),
                          fluidRow(
                            column(6,
                                   align = 'center',
                                   tags$head(tags$style("
                                                        #emp_age_group ~ .selectize-control.single .selectize-input {
                                                        border: 1px solid #000000; background-color: #D2DAE3;
                                                        
                                                        }")),
                                     selectInput('emp_age_group',
                                                 'Age group',
                                                 choices = unique(census$`Age group`)[!grepl('Total', unique(census$`Age group`))],
                                                 selected = "25 to 29 years"),
                                   checkboxInput('emp_plot_or_table_age',
                                                 'View as table', 
                                                 value = FALSE),
                                   uiOutput('emp_plot_table_age')),
                            
                            column(6,
                                   align = 'center',
                                   tags$head(tags$style("
                                                        #emp_gen ~ .selectize-control.single .selectize-input {
                                                        border: 1px solid #000000; background-color: #D2DAE3;
                                                        
                                                        }")),
                                     
                                   selectInput('emp_gen',
                                               'Sex',
                                               choices = c('Male', 'Female'),
                                               selected = "Female"),
                                   checkboxInput('emp_plot_or_table_gen',
                                                 'View as table', 
                                                 value = FALSE),
                                   uiOutput('emp_plot_table_gen'))
                                   ),
                          br(), br(),
                          fluidRow(
                            column(6,
                                   box(
                                     title = 'Aboriginal unemployment rate: ',
                                     solidHeader = FALSE,
                                     background = "aqua",
                                     width = 12,
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     textOutput('emp_ab_text'))
                            ),
                            
                            column(6,
                                   box(
                                     title = "Non-Aboriginal unemployment rate: ",
                                     solidHeader = FALSE,
                                     background = "yellow",
                                     width = 12,
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     textOutput('emp_non_text'))
                            )
                            
                          )
                            )
                          ),
                      
                      fluidRow(
                        shinydashboard::box(
                          title = 'Housing',
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          fluidRow(column(12,
                                          align = 'center',
                                          h2('Housing for youth aged 15-29'))),
                          br(),
                          column(6,
                                 box(solidHeader = TRUE,
                                     width = 12,
                                     title = 'Owner occupied vs rented 
                                     (Youth 15-29)',
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     tabsetPanel(tabPanel('Plot', 
                                                          selectInput('house_demo_variable',
                                                                      'Examine by:', 
                                                                      choices = c('All youth', 
                                                                                  'Sex', 
                                                                                  'Place of Birth', 
                                                                                  'Visible minority',
                                                                                  'Aboriginal identity'),
                                                                      selected = 'All youth',
                                                                      multiple = FALSE),
                                                          uiOutput('owner_plot_vm_filter'),
                                                          plotlyOutput('owner_plot')),
                                                 tabPanel('Table',
                                                          DT::dataTableOutput('owner_table')))
                                     
                                 )
                      ),
                      column(6,
                             box(solidHeader = TRUE,
                                 width = 12,
                                 title = 'Subsidized housing (youth 15-29)',
                                 collapsible = TRUE,
                                 collapsed = TRUE,
                                 tabsetPanel(tabPanel('Plot', 
                                                      selectInput('sub_demo_variable',
                                                                  'Examine by:', 
                                                                  choices = c('All youth', 
                                                                              'Sex', 
                                                                              'Place of Birth', 
                                                                              'Visible minority',
                                                                              'Aboriginal identity'),
                                                                  selected = 'All youth',
                                                                  multiple = FALSE),
                                                      uiOutput('sub_plot_vm_filter'),
                                                      plotlyOutput('sub_plot')),
                                             tabPanel('Table',
                                                      dataTableOutput('sub_table')))
                             )
                      )
                      
                      )
                      ),
                      
                      fluidRow(
                        shinydashboard::box(
                          title = 'Income',
                          status = 'danger',
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = TRUE,
                          collapsed = TRUE,
                          align = 'center',
                          h2('Average household income before tax (Youth 15-29)'), 
                          fluidRow(column(6,
                                          selectInput('income_add_location',
                                                      'Select additional locations',
                                                      choices = unique(census$Geography),
                                                      selected = 'Ontario',
                                                      multiple = TRUE),
                                          plotlyOutput('income_plot_all_geo')),
                                   column(6,
                                          selectInput('income_vm_filter',
                                                      'Select visible minority',
                                                      choices = unique(census$`Visible minority`)[!grepl('Total', unique(census$`Visible minority`))],
                                                      selected = 'All visible minorities',
                                                      multiple = TRUE),
                                          plotlyOutput('income_plot_vm'))),

                          br(), br(),
                          h2('Percent of youth qualifying as low income status by Sex'),
                          br(),
                          br(),
                          fluidRow(column(4,
                                          selectInput('income_status_map_demo',
                                                      'Choose a demographic variable',
                                                      choices = colnames(census)[4:8],
                                                      selected = colnames(census)[4:8][1],
                                                      multiple = FALSE)),
                                   column(4,
                                          radioButtons('income_map_year',
                                                      'Choose a year to view map',
                                                      choices = c('2001', '2006', '2011', '2016'),
                                                      selected = '2016',
                                                      inline = TRUE)),
                                   column(4,
                                          uiOutput('income_status_map_demo_filter_ui'))),
                          fluidRow(column(6,
                                          br(), br(), 
                                          leafletOutput('income_status_map_all_geo', height = 450)),
                                   column(6,
                                          DT::dataTableOutput('income_status_table')))
                          
                        )
                      )
                      )
                    )
                    )         
)




# Define server 
server <- function(input, output) {
  
  # -----------------------------------------------------------------------------
  # reactive objects for map popper
  
  
  # observeEvent(input$locations,{
  #   updateSelectInput(session, "location", "Location", 
  #                     choices = unique(census$Geography),
  #                     selected = input$locations)
  # })
  # 
  # observeEvent(input$map_marker_click, {
  #   click <- input$map_marker_click
  #   location <- ont2@data[which(ont2@data$lat == click$lat & ont2@data$lat$long == click$lng), ]$Name_2
  #   updateSelectInput(session, "location", "Location", 
  #                     choices = unique(census$Geography),
  #                     selected = c(location))
  # })
  # 
  
  
  # -----------------------------------------------------------------------------
  # Map
  
  # map of ontario
  output$the_map <- renderLeaflet({
    if (is.null(input$years)) {
      return(NULL) 
    } else {
      
      # subset data by inputs 

      years <- input$years
      message('YEARS are ')
      print(input$years)

      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                     "Place of Birth","Visible minority", "Aboriginal identity", 'Population')
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(!Geography %in% 'Ontario') %>% 
        filter(year %in% years) 
      
      temp <- temp %>% filter(grepl('Total',`Age group`)) %>%
        filter(grepl('Total',`Sex`)) %>% filter(grepl('Total',`Place of Birth`)) %>%
        filter(grepl('Total',`Visible minority`)) %>% filter(grepl('Total',`Aboriginal identity`)) 
      
      # keep only age group, year, and population
      temp <- temp[, c('Geography', 'geo_code','year','Population')]
      
      temp$year <- as.character(temp$year)
      temp$Geography <- NULL
      census_pop$year <- as.character(census_pop$year)
      
      temp <- inner_join(temp, census_pop, by = c('year', 'geo_code'))
      # make percentage youth variable 
      temp$per_youth <- round((temp$Population/temp$`Total population`)*100, 2)
      
      # keep only geo code and per_youth
      temp <- temp[, c('geo_code', 'per_youth')]
      
      if(length(years) > 1){
        temp <- temp %>%
          group_by(geo_code) %>%
          summarise(per_youth = mean(per_youth, na.rm = T))
      }
      
      leaf(temp, years = years)
      
    }
  })
  
  
  # -----------------------------------------------------------------------------
  # demo variables
  output$demo_chart_avg <- renderUI({
    if (is.null(input$years) | length(input$years) == 1) {
      return(NULL) 
    } else {
      checkboxInput('demo_chart_avg',
                    'Average all years selected',
                    value = FALSE)
    }
  })
  

  
  # demo_plot_pie
  output$demo_plot_pie <- renderPlotly({
    
    
    if (is.null(input$years)) {
      return(NULL) 
    } else {
      location <- input$location
      years <- input$years
      # create vector of demographic variables with population
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                     "Place of Birth","Visible minority", "Aboriginal identity", 'Population')
      new_census <- census[ , demo_vars]
      
      # subset data so that we have total in every variable except the variable of interest
      # Age group in this cases
      temp <- new_census %>% filter(Geography %in% location) %>% 
        filter(year %in% years) %>% filter(!grepl('Total',`Age group`)) %>%
        filter(grepl('Total',`Sex`)) %>% filter(grepl('Total',`Place of Birth`)) %>%
        filter(grepl('Total',`Visible minority`)) %>% filter(grepl('Total',`Aboriginal identity`)) 
      
      # keep only age group, year, and population
      temp <- temp[, c('year','Age group','Population')]
      
      # use functon for plotly pie, with a dougnut hole
      pie_plotly_demo(temp, hole_value = 0.5)
    }
    
  })
  
  
  # demo_plot_pie
  output$pie_table <- renderDataTable({
    
    
    if (is.null(input$years)) {
      return(NULL)
    } else {
      # same idea as above.
      location <- input$location
      years <- input$years
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", 'Population')
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(!grepl('Total',`Age group`)) %>%
        filter(grepl('Total',`Sex`)) %>% filter(grepl('Total',`Place of Birth`)) %>%
        filter(grepl('Total',`Visible minority`)) %>% filter(grepl('Total',`Aboriginal identity`))
      
      
      # keep only age group, year, and population
      temp <- temp[, c('year','Age group','Population')]
      temp <- spread(temp, key = year, value = Population)
      
      
      datatable(temp, fillContainer = F, rownames = FALSE, options = list(dom = 't', ordering = FALSE)) %>%
        formatStyle(
          'Age group',
          target = 'row',
          backgroundColor = styleEqual(c('15 to 19 years', '20 to 24 years', '25 to 29 years'),
                                       c('white', 'white', 'white')))
 
    }
    
    
  })
  # 
  # 'Sex', 'Place of Birth', 'Visible minority',
  # 'Aboriginal identity'
  output$demo_charts <- renderPlotly({
    
    if ((is.null(input$years) | is.null(input$location) | is.null(input$demo_variable) | is.null(input$demo_chart_avg))) {
      return(NULL)
    } else {
      
      location <- 'Ontario'
      years <- 2016
      demo_variable <- 'Sex'
      
      
      location <- input$location
      years <- input$years
      
      # variable to examine
      demo_variable <- input$demo_variable
      
      # choice to avg all years into piechart
      avg_years <- input$demo_chart_avg
      
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", 'Population')
      new_census <- census[ , demo_vars]
      
      new_census <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl('Total',`Age group`))
      
      
      if(demo_variable == 'Sex') {
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
        
      }
      
      if(demo_variable == 'Place of Birth') {
        
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Aboriginal identity` <- temp$`Sex` <- temp$`Visible minority` <-   NULL
        
        
      }
      
      if(demo_variable == 'Visible minority') {
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Place of Birth` <- temp$Sex <- temp$`Aboriginal identity` <-   NULL
        
        # remove Arab/West Asian
        temp <- temp[temp$`Visible minority` != 'Arab/West Asian',]
        temp <- temp[temp$`Visible minority` != 'All visible minorities',]
        
      }
      
      if(demo_variable == 'Aboriginal identity') {
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Aboriginal identity`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Visible minority`))
        
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Place of Birth` <- temp$Sex <- temp$`Visible minority` <-   NULL
      
      }
      
      
      if(length(years) == 1){
        plotly_plot <- pie_plotly_demo(temp, hole_value = 0.5)
      } else {
        if(avg_years){
          plotly_plot <- pie_plotly_demo(temp, hole_value = 0.5)
        } else {
          plotly_plot <- bar_plotly_demo(temp, no_legend = F)
        }
      }
      
      
      return(plotly_plot)
    }
    
  })
  # 
  # 
  
  # 'Aboriginal identity'
  output$demo_tables <- renderDataTable({
    
    if (is.null(input$years) | is.null(input$location) | is.null(input$demo_variable) | is.null(input$demo_chart_avg)) {
      return(NULL)
    } else {
      # subset data by inputs
      # location <- 'Ontario'
      # years <- c(2001, 2006, 2011, 2016)
      # demo_variable <- 'Place of Birth'
      # avg_years <- TRUE
      location <- input$location
      years <- input$years
      demo_variable <- input$demo_variable
      avg_years <- input$demo_chart_avg
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", 'Population')
      new_census <- census[ , demo_vars]
      
      new_census <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl('Total',`Age group`))
      
      if(demo_variable == 'Sex') {
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
        
        return(prettify(temp, comma_numbers = TRUE, round_digits = TRUE, remove_line_breaks = TRUE, 
                        download_options = TRUE))
        
      }
      
      if(demo_variable == 'Place of Birth') {
        
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Aboriginal identity` <- temp$`Sex` <- temp$`Visible minority` <-   NULL
        
        return(prettify(temp, download_options = TRUE))
        
      }
      
      if(demo_variable == 'Visible minority') {
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Place of Birth` <- temp$Sex <- temp$`Aboriginal identity` <-   NULL
        
        # remove Arab/West Asian
        temp <- temp[temp$`Visible minority` != 'Arab/West Asian',]
        temp <- temp[temp$`Visible minority` != 'All visible minorities',]
        
        return(prettify(temp, download_options = TRUE))        
      }
      
      if(demo_variable == 'Aboriginal identity') {
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Aboriginal identity`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Visible minority`))
        
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Place of Birth` <- temp$Sex <- temp$`Visible minority` <-   NULL
        
        return(prettify(temp, download_options = TRUE))        
      }
      
    }
    
  })
  # 
  # 
  # ---------------------------------------------------------------------------------------------
  # family variables = 'fam_plot_parents, fam_plot_kids, fam_plot_vismin, fam_table_vismin
  # 
  output$fam_plot_parents_1 <- renderPlotly({
    
    if (is.null(input$years) | is.null(input$location) | is.null(input$fam_type)) {
      return(NULL)
    } else {
      # # subset data by inputs
      # location <- 'Ontario'
      # years <- c(2001)
      # fam_type <- 'Spouses and common law partners'
      
      # get family input 
      fam_type <- input$fam_type
      location <- input$location
      years <- input$years
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", fam_type,'Population')
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl("25 to 29 years",`Age group`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
        temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
      
      
      temp_melt <- melt(temp, id.vars = 'year')
      

      f <- list(
        family = "Ubuntu",
        size = 20,
        color = "white"
      )
      
      year_value <- unique(temp_melt$year)
      
      if(length(year_value) == 1){
        title_name <- year_value
        title_f <- list(
          family = "Ubuntu",
          size = 12,
          color = "#1F2023"
        )
        
        
      } else {
        year_value <- paste0(year_value, collapse = ', ')
        title_name <- paste0('Average for years:  ', year_value)
        
        title_f <- list(
          family = "Ubuntu",
          size = 11,
          color = "#1F2023"
        )
        
      }
     
      cols <- c("#9999CC",'#28B463')
      p1 <-  plot_ly(temp_melt,labels = ~variable, values = ~value ,
                     type ='pie',
                     hole = 0.5,
                     textposition = 'inside',
                     textinfo = 'percent',
                     insidetextfont = f,
                     marker = list(colors = cols,
                                   line = list(color = '#FFFFFF', width = 1)),
                     hoverinfo = 'label+value') %>%
        config(displayModeBar = F) %>%
        
        layout(title = title_name, font = title_f, showlegend = F,
               annotations = list(
                 showarrow = FALSE,
                 text = '',
                 font = list(color = '#1F2023',
                             family = 'sans serif')), 
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      p1
    }
    
  })
  
  
  output$fam_plot_parents_2 <- renderGvis({
    
    if (is.null(input$years) | is.null(input$location) | is.null(input$fam_type)) {
      return(NULL)
    } else {
      
      # subset data by inputs
      location <- 'Ontario'
      years <- c(2001, 2006, 2011, 2016)
      fam_type <- 'Spouses and common law partners'
      location <- input$location
      years <- input$years
      fam_type <- input$fam_type
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", fam_type,'Population')
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl("25 to 29 years",`Age group`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
        temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
      
      
      # # get percentages
      temp <- as.data.frame(temp)
      colnames(temp)[2] <- paste0('Total ', fam_type)
      
      temp$Percent <- round((temp[,2]/temp$Population)*100, 2)
      colnames(temp)[4] <- 'Percent'
      temp$year <- as.factor(temp$year)
      line <- 
        gvisLineChart(temp, xvar="year", yvar=c(paste0('Total ', fam_type), 'Percent'),
                      options=list(title="",
                                   titleTextStyle="{color:'#1F2023',
                                   fontName:'Ubuntu',
                                   fontSize:18}",
                                   curveType="function", 
                                   pointSize=15,
                                   series="[{targetAxisIndex:0, 
                                   color:'#28B463'}, 
                                   {targetAxisIndex:1,
                                   color:'#9999CC'}]",
                                   vAxes="[{title:'Population',
                                   format:'##,###',
                                   titleTextStyle: {color: 'green'},
                                   textStyle:{color: 'green'},
                                   textPosition: 'out'}, 
                                   {title:'Percent',
                                   format:'#.##',
                                   titleTextStyle: {color: '#9999CC'},  
                                   textStyle:{color: '#9999CC'},
                                   textPosition: 'out'}]",
                                   hAxes="[{title:'Year',
                                   textPosition: 'out'}]",
                                   height=320
                      ),
                      chartid="twoaxislinechart_fam"
        )
      line
  }
    
    
    
    })
  
  output$fam_plot_parents <- renderUI({
    if (is.null(input$years) | is.null(input$location) | is.null(input$fam_type)) {
      return(NULL)
    } else {
      if(length(input$years) == 1){
        plotlyOutput('fam_plot_parents_1')
      } else {
        htmlOutput('fam_plot_parents_2')
      }
    }
    
    # ------------------------------------------------
    # for kids - children in couple families vs children in lone parent families 
    
  })
  
  output$fam_plot_kids <- renderPlotly({
    if (is.null(input$years) | is.null(input$location)) {
      return(NULL)
    } else {
      # subset data by inputs
      location <- 'Ontario'
      years <- ( 2016)
      location <- input$location
      years <- input$years
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", 
                     "Children in lone parent families","Children in couple families",'Population')
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl("15 to 19 years",`Age group`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
        temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
      
      # make an "Other" column that makes up the rest between the addtion of our two variables and total population for 
      # that age group.
      temp$other <- temp$Population - (temp$`Children in lone parent families` + temp$`Children in couple families`)
      
      # remove population since other variables account for it 
      temp$Population <- NULL
      
      # get in long format 
      temp_long <- melt(temp, id.vars = 'year')
      
      temp_dat <- temp_long %>%
        group_by(year) %>%
        mutate(tot_pop = sum(value))  %>%
        group_by(year, variable) %>%
        mutate(pop_per =round(value/tot_pop,  2))
      temp_dat$year <- as.factor(temp_dat$year)
      
      year_value <- unique(temp_dat$year)
      
      f <- list(
        family = "Ubuntu",
        size = 20,
        color = "white"
      )
      
      
      
      
      if(length(years) == 1){
        
        
          title_name <- year_value
          title_f <- list(
            family = "Ubuntu",
            size = 12,
            color = "#1F2023"
          )
          
       
        cols <- c('#28B463',"#9999CC")
        p1 <-  plot_ly(temp_dat, labels = ~variable, values = ~value ,
                       type ='pie',
                       hole = 0.5,
                       textposition = 'inside',
                       textinfo = 'percent',
                       insidetextfont = f,
                       marker = list(colors = cols,
                                     line = list(color = '#FFFFFF', width = 1)),
                       hoverinfo = 'label+value') %>%
          config(displayModeBar = F) %>%
          
          layout(title = title_name, font = title_f, showlegend = F,
                 annotations = list(
                   showarrow = FALSE,
                   text = '',
                   font = list(color = '#1F2023',
                               family = 'sans serif')), 
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        p1
      } else {
        
        year_value <- paste0(year_value, collapse = ', ')
        title_name <- paste0('Average for years:  ', year_value)
        
        title_f <- list(
          family = "Ubuntu",
          size = 11,
          color = "#1F2023"
        )
        # plot dataseries="[{targetAxisIndex:0, 
        cols <- c('#28B463',"#9999CC", "grey" )
        g <- ggplot(data = temp_dat,
                    aes(x = year,
                        y = pop_per,
                        fill = variable, 
                        text = paste('Total population 15-19 year old: ', tot_pop,
                                     '<br>', pop_per , '%', as.factor(variable)))) +
          geom_bar(position = 'dodge', stat = 'identity', colour = 'black', alpha = 0.8) +
          scale_y_continuous(labels = scales::percent) +
          # geom_text(aes(label = pop_per), position = position_dodge(width = 1), vjust = -0.5) +
          labs(x = '', y = ' ') + scale_fill_manual(name = '',
                                                    values = cols)
        g <- g  + theme_bw(base_size = 12, base_family = 'Ubuntu') 
        
        plotly::ggplotly(g, tooltip = 'text') %>%
          config(displayModeBar = F) %>%
          layout( 
            legend = list(
              orientation = "l",
              x = 0,
              y = -0.6))
      }
     
      
    }
    
    
  })
  
  
  output$fam_plot_table_vismin <- renderUI({
    if(input$fam_chart_table){
      dataTableOutput('fam_table_vismin')
    } else {
      plotlyOutput('fam_plot_vismin')
    }
    
  })
  
  
  output$fam_plot_vismin <- renderPlotly({
    if(is.null(input$years) | is.null(input$location) | is.null(input$fam_chart_table_all_or_vm)) {
      return(NULL)
    } else {
      # subset data by inputs
      location <- 'Ontario'
      years <- c(2016)
      which_fam_type <- 'Lone parents'
      fam_chart_table_all_or_vm <- FALSE
      which_fam_type <- input$which_fam_type
      location <- input$location
      years <- input$years
      fam_chart_table_all_or_vm <- input$fam_chart_table_all_or_vm
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", 
                     which_fam_type,'Population')
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl("25 to 29 years",`Age group`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(!grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
        temp$`Aboriginal identity` <- temp$`Place of Birth`  <-   NULL
      
      # make an "Other" column that makes up the rest between the addtion of our two variables and total population for 
      # that age group.
      
      # remove Arab/West Asian from data
      temp <- temp %>% filter(!`Visible minority` %in% 'Arab/West Asian')
      
      # get two datasets: all other vs all vm, and only within vm.
      temp_all <- temp %>% filter(grepl('All', temp$`Visible minority`))
      temp_vm <- temp %>% filter(!grepl('All', temp$`Visible minority`))
      
      # make data frmae
      temp_all <- as.data.frame(temp_all)
      temp_vm <- as.data.frame(temp_vm)
      
      # get percentage 
      colnames(temp_all)[3] <- 'V2'
      colnames(temp_vm)[3] <- 'V2'
      
      temp_all$per <- round(temp_all$V2/temp_all$Population,2 )
      temp_vm$per <- round(temp_vm$V2/temp_vm$Population,2 )
      
      
      if(length(years)==1){
        if(fam_chart_table_all_or_vm){
          # plot all vm
          cols <- c('darkgreen', "#9999CC")
          g <- ggplot(data = temp_all,
                      aes(x = `Visible minority`,
                          y = per,
                          text = paste('Total population of', `Visible minority`, ': ', Population,
                                       '<br>', (per)*100, '%'))) +
            geom_bar(stat = 'identity', alpha = 0.7,fill = 'darkgreen') + theme_bw(base_size = 16, base_family = 'Ubuntu')  +
            labs(x = '', y = ' ') + ggtitle(paste0('Youth (25 to 29) ', which_fam_type)) + 
            scale_y_continuous(labels = scales::percent)
          
          g <- g  + theme_bw(base_size = 14, base_family = 'Ubuntu') 
          
          p1 <- plotly::ggplotly(g, tooltip = 'text') %>% config(displayModeBar = F) 
        } else {
          temp_vm$`Visible minority` <- gsub('Visible minority, n.i.e.', 'VM N.I.E', temp_vm$`Visible minority`)
          temp_vm$`Visible minority`<- gsub('Multiple visible minority', 'Multiple VM', temp_vm$`Visible minority`)
          
          cols <- colorRampPalette(brewer.pal(9, 'Set1'))(length(unique(temp_vm$`Visible minority`)))
          g <- ggplot(data = temp_vm,
                      aes(x = as.factor(`Visible minority`),
                          y = per,
                          text = paste('Total population of', `Visible minority`, ': ', Population,
                                       '<br>', (per)*100 , '%'))) +  theme_bw(base_size = 8, base_family = 'Ubuntu') +
            geom_bar(stat = 'identity', alpha = 0.7,fill = cols) + 
            labs(x = '', y = ' ') + ggtitle(paste0('Youth (25 to 29) ', which_fam_type)) + 
            scale_y_continuous(labels = scales::percent)
            
          
          p1 <- plotly::ggplotly(g, tooltip = 'text') %>% config(displayModeBar = F)
        }
        
      } else {
        if(fam_chart_table_all_or_vm){
          # plot all vm
          cols <- c('darkgreen', "#9999CC")
          g <- ggplot(data = temp_all,
                      aes(x = year,
                          y = per,
                          group = `Visible minority`,
                          colour = `Visible minority`,
                          text = paste('Total population of', `Visible minority`, ': ', Population,
                                       '<br>', (per)*100 , '%'))) +
            geom_point(size = 6) + geom_line(size = 2, alpha = 0.6) + scale_color_manual(name = '',
                                                                                         values = cols) + theme_bw(base_size = 16, base_family = 'Ubuntu')  +
            labs(x = '', y = ' ') + ggtitle(paste0('Youth (25 to 29) ', which_fam_type)) + 
            scale_y_continuous(labels = scales::percent)
          g <- g  + theme_bw(base_size = 14, base_family = 'Ubuntu') 
          
          p1 <- plotly::ggplotly(g, tooltip = 'text') %>% config(displayModeBar = F) 
        } else {
          # plot data
          cols <- colorRampPalette(brewer.pal(9, 'Set1'))(length(unique(temp_vm$`Visible minority`)))
          g <- ggplot(data = temp_vm,
                      aes(x = year,
                          y = per,
                          group = `Visible minority`,
                          colour = `Visible minority`,
                          text = paste('Total population of', `Visible minority`, ': ', Population,
                                       '<br>', (per)*100 , '%'))) + 
            geom_point(size = 6) + geom_line(size = 2, alpha = 0.6) + scale_color_manual(name = '',
                                                                                         values = cols) + theme_bw(base_size = 14, base_family = 'Ubuntu')  +
            scale_y_continuous(labels = scales::percent) +
            labs(x = '', y = ' ') + ggtitle(paste0('Youth (25 to 29) ', which_fam_type))
          
          p1 <- plotly::ggplotly(g, tooltip = 'text')  %>% config(displayModeBar = F)
        }
      }
      
      
      return(p1)
      
    }
    
  })
  
  
  output$fam_table_vismin <- DT::renderDataTable({
    if(is.null(input$years) | is.null(input$location) | is.null(input$fam_chart_table_all_or_vm)) {
      return(NULL)
    } else {
      # subset data by inputs
      location <- 'Ontario'
      years <- c(2001, 2006, 2011, 2016)
      which_fam_type <- 'Lone parents'
      fam_chart_table_all_or_vm <- FALSE
      which_fam_type <- input$which_fam_type
      location <- input$location
      years <- input$years
      fam_chart_table_all_or_vm <- input$fam_chart_table_all_or_vm
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                     "Place of Birth","Visible minority", "Aboriginal identity", 
                     which_fam_type,'Population')
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl("25 to 29 years",`Age group`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(!grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
        temp$`Aboriginal identity` <- temp$`Place of Birth`  <-   NULL
      
      # make an "Other" column that makes up the rest between the addtion of our two variables and total population for 
      # that age group.
      
      # remove Arab/West Asian from data
      temp <- temp %>% filter(!`Visible minority` %in% 'Arab/West Asian')
      
      # get two datasets: all other vs all vm, and only within vm.
      temp_all <- temp %>% filter(grepl('All', temp$`Visible minority`))
      temp_vm <- temp %>% filter(!grepl('All', temp$`Visible minority`))
      
      # get percentage 
      colnames(temp_all)[3] <- 'V2'
      colnames(temp_vm)[3] <- 'V2'
      
      temp_all$per <- round((temp_all$V2/temp_all$Population)*100, 2)
      temp_vm$per <- round((temp_vm$V2/temp_vm$Population)*100, 2)
      
      # get percentage 
      colnames(temp_all)[3] <- which_fam_type
      colnames(temp_vm)[3] <- which_fam_type
      
      if(fam_chart_table_all_or_vm){
        prettify(temp_all)
      } else {
        prettify(temp_vm)
      }
      
    }
    
  })
  
  
  #----------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------
  # education 
  # textOuput: ed_text_college (25-29), ed_text_highschool (20-24), ed_text_all (by gender) (20-29)
  
  
  output$ed_text_highschool <- renderText({
    # subset data by inputs
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "High school or equivalent",'Population')
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(grepl("20 to 24 years",`Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    temp$per <- round((temp$`High school or equivalent`/temp$Population)*100, 2)
    
    if(length(years) == 1){
      paste0('in ', years,' , ' , temp$per, '% of ', 'Youth aged 20-24 in ', location ,' had earned a highschool degree or equivalent')
    } else {
      first_year_per <- temp$per[temp$year == min(temp$year)]
      last_year_per <- temp$per[temp$year == max(temp$year)]
      
      first_year <- temp$year[temp$year == min(temp$year)]
      last_year <- temp$year[temp$year == max(temp$year)]
      
      if(first_year_per > last_year_per) {
        increase_decrease  <- 'decreased'
      } else if(first_year_per < last_year_per) {
        increase_decrease  <- 'increased'
      } else {
        increase_decrease <- 'stayed the same'
      }
      
      paste0('The % of youth aged 20-24, earning a HS degree or equivalent in ',location , ' has ', increase_decrease,' from ',first_year_per, '% in ', first_year,' to ', last_year_per, '% in ', last_year)
      
    }
  })
  
  output$ed_text_college <- renderText({
    # subset data by inputs
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "College, CEGEP or other non-university certificate or diploma" ,'Population')
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(grepl("25 to 29 years",`Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    temp$per <- round((temp$`College, CEGEP or other non-university certificate or diploma`/temp$Population)*100, 2)
    
    if(length(years) == 1){
      paste0('in ', years,' , ' , temp$per, '% of ', 'Youth aged 25-29 in ', location ,' had earned a College, CEGEP or other non-university certificate or diploma')
    } else {
      first_year_per <- temp$per[temp$year == min(temp$year)]
      last_year_per <- temp$per[temp$year == max(temp$year)]
      
      first_year <- temp$year[temp$year == min(temp$year)]
      last_year <- temp$year[temp$year == max(temp$year)]
      
      if(first_year_per > last_year_per) {
        increase_decrease  <- 'decreased'
      } else if(first_year_per < last_year_per) {
        increase_decrease  <- 'increased'
      } else {
        increase_decrease <- 'stayed the same'
      }
      
      paste0('The % of youth aged 25-29, earning a College, CEGEP or other non-university certificate or diploma in ',location , ' has ', increase_decrease,' from ',first_year_per, '% in ', first_year,' to ', last_year_per, '% in ', last_year)
      
    }
    
  })
  
  output$ed_text_university <- renderText({
    # subset data by inputs
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "University certificate, diploma or degree" ,'Population')
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(grepl("25 to 29 years",`Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    temp$per <- round((temp$`University certificate, diploma or degree`/temp$Population)*100, 2)
    
    if(length(years) == 1){
      paste0('in ', years,' , ' , temp$per, '% of ', 'Youth aged 25-29 in ', location ,' had earned a University certificate, diploma or degree')
    } else {
      first_year_per <- temp$per[temp$year == min(temp$year)]
      last_year_per <- temp$per[temp$year == max(temp$year)]
      
      first_year <- temp$year[temp$year == min(temp$year)]
      last_year <- temp$year[temp$year == max(temp$year)]
      
      if(first_year_per > last_year_per) {
        increase_decrease  <- 'decreased'
      } else if(first_year_per < last_year_per) {
        increase_decrease  <- 'increased'
      } else {
        increase_decrease <- 'stayed the same'
      }
      
      paste0('The % of youth aged 25-29, earning a University certificate, diploma or degree in ',location , ' has ', increase_decrease,' from ',first_year_per, '% in ', first_year,' to ', last_year_per, '% in ', last_year)
      
    }
  })
  
  
  output$ed_plot_table_age <- renderUI({
    if(!input$ed_plot_or_table_age){
      plotlyOutput('ed_plot_age')
      
    } else {
      DT::dataTableOutput('ed_table_age')
    }
    
  })
  
  output$ed_plot_table_sex <- renderUI({
    if(!input$ed_plot_or_table_sex){
      plotlyOutput('ed_plot_sex')
      
    } else {
      DT::dataTableOutput('ed_table_sex')
    }
    
  })
  
  # plot of highschool 20-29, gender
  output$ed_plot_age <- renderPlotly({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011,2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "High school or equivalent" ,'Population')
    new_census <- census[ , demo_vars]
    
    
    # get age data
    temp_age <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% 
      filter(!grepl("Total|15",`Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp_age$Geography <- temp_age$geo_code  <- temp_age$Sex <- 
      temp_age$`Aboriginal identity` <- temp_age$`Place of Birth`  <-
      temp_age$`Visible minority` <- NULL
    
    temp_age$per <- round((temp_age$`High school or equivalent`/temp_age$Population),2)
    
    
    if(length(years) == 1){
      year_value <- unique(temp_age$year)
      title_name <- paste0('By age:  ', year_value)
      
      # plot1 - by age group (20-24) vs (25-29) (bar)
      # plot data
      cols <- c('green3', 'deepskyblue')
      g <- ggplot(data = temp_age,
                  aes(x = as.factor(year),
                      y = per,
                      fill = `Age group`,
                      text = paste('Total population for age group: ', Population,
                                   '<br>', (per)*100 , '%', as.factor(`Age group`)))) +
        scale_fill_manual(name = '',
                          values = cols) + scale_y_continuous(labels = scales::percent) +
        geom_bar(position = 'dodge', stat = 'identity', colour = 'darkgrey',alpha = 0.8) + 
        theme_bw(base_size = 14, base_family = 'Ubuntu') + labs(x = '', y = ' ', title = title_name)
      
      
      age_plot <-  plotly::ggplotly(g, tooltip = 'text') %>%
        config(displayModeBar = F) %>%
        layout( 
          legend = list(
            orientation = "l",
            x = 0,
            y = -0.3))
      
     
    } else {
      year_value <- unique(temp_age$year)
      year_value <- paste0(year_value, collapse = ', ')
      title_name <- paste0('By age:  ', year_value)
      # plot1 - by age group (20-24) vs (25-29)
      # bar plot with year by percent, grouped by age and gender
      cols <- c('green3', 'deepskyblue')
      # plot data
      g <- ggplot(data = temp_age,
                  aes(x = year,
                      y =per,
                      group = `Age group`,
                      colour = `Age group`,
                      text = paste('<br>', as.factor(`Age group`),': ', (per)*100, '%',
                                   '<br> With highschool degree of equivalent'))) +
        geom_point(size = 4) +
        geom_line(size = 1, alpha = 0.8,linetype = 'dashed') +
        geom_smooth(alpha = 0.4, size = 1) +     theme_bw(base_size = 13, base_family = 'Ubuntu') +
        scale_color_manual(name = '', 
                           values = cols) + theme(legend.position="none") +
        labs(x = '', y = ' ', title = title_name) + scale_y_continuous(labels=scales::percent) 
      
      age_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
        config(displayModeBar = F)  
  
    }
    age_plot
    
    
  })
  
  
  
  
  # plot of highschool 20-29, gender
  output$ed_plot_sex <- renderPlotly({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "High school or equivalent" ,'Population')
    new_census <- census[ , demo_vars]
    
    
    # get sex data
    temp_sex <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% 
      filter(grepl("Total",`Age group`)) %>%
      filter(!grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp_sex$Geography <- temp_sex$geo_code  <-temp_sex$`Age group` <- 
      temp_sex$`Aboriginal identity` <- temp_sex$`Place of Birth`  <-
      temp_sex$`Visible minority` <- NULL
    
    temp_sex$per <- round((temp_sex$`High school or equivalent`/temp_sex$Population),2)
    
    # if one year, both charts become bar charts 
    
    if(length(years) == 1){
      year_value <- unique(temp_sex$year)
      title_name <- paste0('By sex:  ', year_value)
      # plot 2 - by sex bar
      cols <- c('green3', 'deepskyblue')
      g <- ggplot(data = temp_sex,
                  aes(x = as.factor(year),
                      y = per,
                      fill = Sex,
                      text = paste('Total population for age group: ', Population,
                                   '<br>', (per)*100 , '%', as.factor(Sex)))) +
        scale_fill_manual(name = '',
                          values = cols) + scale_y_continuous(labels = scales::percent) +
        geom_bar(position = 'dodge', stat = 'identity', colour = 'darkgrey',alpha = 0.8) + 
        theme_bw(base_size = 14, base_family = 'Ubuntu') + labs(x = '', y = ' ', title = title_name)
      
      
      sex_plot <-  plotly::ggplotly(g, tooltip = 'text') %>%
        config(displayModeBar = F) %>%
        layout( 
          legend = list(
            orientation = "l",
            x = 0,
            y = -0.3))
       

    } else {
      year_value <- unique(temp_sex$year)
      
      year_value <- paste0(year_value, collapse = ', ')
      title_name <- paste0('By Sex :  ', year_value)
      
      cols <- c('green3', 'deepskyblue')
      # plot data
      g <- ggplot(data = temp_sex,
                  aes(x = year,
                      y =per,
                      group = Sex,
                      colour = Sex,
                      text = paste('<br>', as.factor(Sex),': ', (per)*100, '%',
                                   '<br> With highschool degree of equivalent'))) +
        geom_point(size = 4) +
        geom_line(size = 1, alpha = 0.8,linetype = 'dashed') +
        geom_smooth(alpha = 0.4, size = 1) +   theme_bw(base_size = 13, base_family = 'Ubuntu') +
        scale_color_manual(name = '', 
                           values = cols) + theme(legend.position="none") +
        labs(x = '', y = ' ', title = title_name) + scale_y_continuous(labels=scales::percent) 
      
      sex_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
        config(displayModeBar = F)  
      
    }
    
    sex_plot
    
  })
  
  # table of highschool 20-29, gender
  output$ed_table_age <- renderDataTable({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011,2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "High school or equivalent" ,'Population')
    new_census <- census[ , demo_vars]
    
    
    # get age data
    temp_age <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% 
      filter(!grepl("Total|15",`Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp_age$Geography <- temp_age$geo_code  <- temp_age$Sex <- 
      temp_age$`Aboriginal identity` <- temp_age$`Place of Birth`  <-
      temp_age$`Visible minority` <- NULL
    
    temp_age$per <- round((temp_age$`High school or equivalent`/temp_age$Population),2)
    
    prettify(temp_age, download_options = TRUE, round_digits = TRUE)
  })
  
  
  # table of highschool 20-29, gender
  output$ed_table_sex <- renderDataTable({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "High school or equivalent" ,'Population')
    new_census <- census[ , demo_vars]
    
    
    # get sex data
    temp_sex <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% 
      filter(grepl("Total",`Age group`)) %>%
      filter(!grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp_sex$Geography <- temp_sex$geo_code  <-temp_sex$`Age group` <- 
      temp_sex$`Aboriginal identity` <- temp_sex$`Place of Birth`  <-
      temp_sex$`Visible minority` <- NULL
    
    temp_sex$per <- round((temp_sex$`High school or equivalent`/temp_sex$Population),2)
    
    prettify(temp_sex, download_options = TRUE, round_digits = TRUE)
  })
  
  
  
  
  #------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------
  #employment - em_plot_or_table_age - unemployment rate
  output$emp_header <- renderText({
    location <- input$location
    years <- input$years
    if(length(years) == 2) {
      years_final <- paste0(years, collapse = ' & ')
    } else if(length(years) == 3) {
      years_2 <- paste0(years[1:2], collapse = ', ')
      years_final <- paste0(years_2, ' & ', years[3])
    } else if(length(years) == 4) {
      years_3 <- paste0(years[1:3], collapse = ', ')
      years_final <- paste0(years_3, ' & ', years[4])
    } else {
      years_final <- years
    }
    
    paste0('Examine unemployment across age groups and gender in ', location, ' for ', years_final)
  })
  
  ##########
  # age 
  output$emp_plot_table_age <- renderUI({
    if(!input$emp_plot_or_table_age){
      htmlOutput('emp_plot_age')
      
    } else {
      DT::dataTableOutput('emp_table_age')
    }
    
  })
  
  
  output$emp_plot_age <- renderGvis({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    age_group <- '25 to 29 years'
    age_group <- input$emp_age_group
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "Unemployment rate %" ,"Unemployed")
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(!grepl("Total",`Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp$Geography <- temp$geo_code  <- temp$Sex <- 
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    
    temp <- temp %>% filter(`Age group` %in% age_group)
    temp$year <- as.factor(temp$year)
    # double axis chart
    
    if(length(temp$year) == 1){
      point_size <- 40
    } else {
      point_size <- 15
    }
    line <-
      gvisLineChart(temp, xvar="year", yvar=c('Unemployed', 'Unemployment rate %'),
                    options=list(title=paste0(age_group, ' years old'),
                                 titleTextStyle="{color:'black',
                                 fontName:'Ubuntu',
                                 fontSize:18}",
                                 curveType="function",
                                 pointSize=point_size,
                                 series="[{targetAxisIndex:0,
                                 color:'#FFA500'},
                                 {targetAxisIndex:1,
                                 color:'#000080'}]",
                                 vAxes="[{title:'Total unemployed',
                                 format:'##,###',
                                 titleTextStyle: {color: '#FFA500'},
                                 textStyle:{color: '#FFA500'},
                                 textPosition: 'out'},
                                 {title:'Unemployment rate',
                                 format:'#.##',
                                 titleTextStyle: {color: '#000080'},
                                 textStyle:{color: '#000080'},
                                 textPosition: 'out'}]",
                                 hAxes="[{title:'Year',
                                 textPosition: 'out'}]",
                                 width=475, height=550
                    ),
                    chartid="twoaxislinechart"
      )
    line
    # 
})
  
  output$emp_table_age <- renderDataTable({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    age_group <- '25 to 29 years'
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "Unemployment rate %" ,"Unemployed")
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(!grepl("Total",`Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp$Geography <- temp$geo_code  <- temp$Sex <- 
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    temp <- temp %>% filter(`Age group` %in% age_group)
    
    prettify(temp, download_options = TRUE)
  })
  
  #############
  # gender
  ##########
  output$emp_plot_table_gen <- renderUI({
    if(!input$emp_plot_or_table_gen){
      htmlOutput('emp_plot_gen')
    } else {
      DT::dataTableOutput('emp_table_gen')
    }
    
  })
  
  output$emp_plot_gen <- renderGvis({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    gen_group <- 'Male'
    gen_group <- input$emp_gen
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "Unemployment rate %" ,"Unemployed")
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(grepl("Total",`Age group`)) %>%
      filter(!grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp$Geography <- temp$geo_code   <- temp$`Age group` <-
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    
    temp <- temp %>% filter(Sex %in% gen_group)
    temp$`Unemployment rate %` <- round(temp$`Unemployment rate %`, 2)
    temp$year <- as.factor(temp$year)
    
    if(length(temp$year) == 1){
      point_size <- 40
    } else {
      point_size <- 15
    }# double axis chart
    
    
    line_gen <-
      gvisLineChart(temp, xvar="year", yvar=c('Unemployed', 'Unemployment rate %'),
                    options=list(title=paste0(gen_group),
                                 titleTextStyle="{color:'black',
                                 fontName:'Ubuntu',
                                 fontSize:18}",
                                 curveType="function",
                                 pointSize=point_size,
                                 series="[{targetAxisIndex:0,
                                 color:'#1799B5'},
                                 {targetAxisIndex:1,
                                 color:'#A9A9A9'}]",
                                 vAxes="[{title:'Total unemployed',
                                 format:'##,###',
                                 titleTextStyle: {color: '#1799B5'},
                                 textStyle:{color: '#1799B5'},
                                 textPosition: 'out'},
                                 {title:'Unemployment rate',
                                 format:'#.##',
                                 titleTextStyle: {color: '#A9A9A9'},
                                 textStyle:{color: '#A9A9A9'},
                                 textPosition: 'out'}]",
                                 hAxes="[{title:'Year',
                                 textPosition: 'out'}]",
                                 width=475, height=550
                    ),
                    chartid="twoaxislinechart_gen"
      )
    line_gen
    # 
  })
  
  output$emp_table_gen <- renderDataTable({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    gen_group <- 'Male'
    gen_group <- input$emp_gen
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "Unemployment rate %" ,"Unemployed")
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(grepl("Total",`Age group`)) %>%
      filter(!grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp$Geography <- temp$geo_code   <- temp$`Age group` <-
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    
    temp <- temp %>% filter(Sex %in% gen_group)
    temp$`Unemployment rate %` <- round(temp$`Unemployment rate %`, 2)
    
    prettify(temp, download_options = TRUE, no_scroll = FALSE)
  })
  
  
  output$emp_ab_text <- renderText({
    # subset data by inputs
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "Unemployment rate %")
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(grepl("Total",`Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(!grepl('Total|Non', `Aboriginal identity`))
    temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
      temp$`Place of Birth`  <- temp$`Visible minority` <- NULL
    
    temp$`Unemployment rate %` <- round(temp$`Unemployment rate %`,2)
    
    if(length(years) == 1){
      paste0('Unemployment rate = ', temp$`Unemployment rate %`, '% in ', location, ' for ', years)
    } else {
      first_year_rate <- temp$`Unemployment rate %`[temp$year == min(temp$year)]
      last_year_rate <- temp$`Unemployment rate %`[temp$year == max(temp$year)]
      
      first_year <- temp$year[temp$year == min(temp$year)]
      last_year <- temp$year[temp$year == max(temp$year)]
      
      if(first_year_rate > last_year_rate) {
        increase_decrease  <- 'decreased'
      } else if(first_year_rate < last_year_rate) {
        increase_decrease  <- 'increased'
      } else {
        increase_decrease <- 'stayed the same'
      }
      
      paste0(increase_decrease,' in ',location, ' from ',first_year_rate, '% in ', first_year, 
             ' to ', last_year_rate, '% in ', last_year)
      
    }
    
  })
  
  output$emp_non_text <- renderText({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "Unemployment rate %")
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(grepl("Total",`Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Non', `Aboriginal identity`))
    temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
      temp$`Place of Birth`  <- temp$`Visible minority` <- NULL
    
    temp$`Unemployment rate %` <- round(temp$`Unemployment rate %`,2)
    
    if(length(years) == 1){
      paste0('Unemployment rate = ', temp$`Unemployment rate %`, '% in ', location, ' for ', years)
    } else {
      first_year_rate <- temp$`Unemployment rate %`[temp$year == min(temp$year)]
      last_year_rate <- temp$`Unemployment rate %`[temp$year == max(temp$year)]
      
      first_year <- temp$year[temp$year == min(temp$year)]
      last_year <- temp$year[temp$year == max(temp$year)]
      
      if(first_year_rate > last_year_rate) {
        increase_decrease  <- 'decreased'
      } else if(first_year_rate < last_year_rate) {
        increase_decrease  <- 'increased'
      } else {
        increase_decrease <- 'stayed the same'
      }
      
      paste0(increase_decrease,' in ',location, ' from ',first_year_rate, '% in ', first_year, 
             ' to ', last_year_rate, '% in ', last_year)
      
    }
    
  })
  
  # -----------------------------------------------------------------------------------------------------------------
  # -----------------------------------------------------------------------------------------------------------------
  # housing - owner occupied vs reneted, subsidized housing 
  
  # ownder occupied vs rented 
  # owner_rented_table_plot, sub_table_plot
  
  output$owner_plot_vm_filter <- renderUI({
    
    if(input$house_demo_variable !='Visible minority' | is.null(input$house_demo_variable)){
      NULL
    } else {
      choice_vm <- unique(census$`Visible minority`)
      choice_vm <- choice_vm[!grepl('Arab/West|Total', choice_vm)]
      selectInput('owner_plot_vm_filter',
                  'Take a closer look',
                  choices = choice_vm,
                  selected = 'All visible minorities',
                  multiple = TRUE)
    }
  })
  
  # by gender 
  output$owner_plot <- renderPlotly({
    
    
    # subset data by inputs
    location <- 'Ontario'
    years <- c(2016)
    house_demo_variable <- 'Sex'
    # avg_years <- TRUE
    
    location <- input$location
    years <- input$years
    house_demo_variable <- input$house_demo_variable
    
    if(is.null(input$house_demo_variable)){
      NULL
    } else {
      # avg_years <- input$demo_chart_avg
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex",
                     "Place of Birth","Visible minority", "Aboriginal identity",
                     "Living in owner occupied dwelling", "Living in rented dwelling", "Population")
      new_census <- census[ , demo_vars]
      
      new_census <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl('Total',`Age group`))
      
      # 
      if(house_demo_variable == 'All youth'){
        # get data
        temp <- new_census %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
          temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
        
        temp$Other <- temp$Population - (temp$`Living in owner occupied dwelling` + temp$`Living in rented dwelling`)
        temp$Population <- NULL
        
        
        temp_melt <- melt(temp, id.vars = 'year')
        
        temp_melt$year <- as.factor(temp_melt$year)
        temp_melt$value <- as.numeric(temp_melt$value)
        
        # group by and get population and percent for each year
        temp_dat <- temp_melt %>%
          group_by(year) %>%
          mutate(tot_pop = sum(value))  %>%
          group_by(year, variable) %>%
          mutate(pop_per = round(value/tot_pop ,2))
        
        # plot data
        cols <- colorRampPalette(brewer.pal(9, 'Reds'))(length(unique(temp_dat$variable)))
        g <- ggplot(data = temp_dat,
                    aes(x = year,
                        y = pop_per,
                        fill = variable,
                        text = paste('Total population 15-29 year old: ', tot_pop,
                                     '<br>', pop_per , '%', as.factor(variable)))) +
          scale_fill_manual(name = '',
                            values = cols) + scale_y_continuous(labels = scales::percent) +
          geom_bar(position = 'dodge', stat = 'identity', colour = 'black', alpha = 0.8) + 
          theme_bw(base_size = 14, base_family = 'Ubuntu') + labs(x = '', y = ' ')
        
        
        final_plot <-  plotly::ggplotly(g, tooltip = 'text') %>%
          config(displayModeBar = F) %>% 
          layout( 
            legend = list(
              orientation = "l",
              x = 0,
              y = -0.6))
      }
      
      if(house_demo_variable == 'Sex') {
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <-
          temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
        temp$Other <- temp$Population - (temp$`Living in owner occupied dwelling` + temp$`Living in rented dwelling`)
        temp$Population <- NULL
        
        temp_melt <- melt(temp, id.vars = c('year', house_demo_variable))
        
        
        temp_melt$year <- as.factor(temp_melt$year)
        temp_melt$value <- as.numeric(temp_melt$value)
        
        
        temp_demo_lab <- colnames(temp_melt)[2]
        colnames(temp_melt)[2] <- 'V2'
        # group by and get population and percent for each year
        temp_dat <- temp_melt %>%
          group_by(year, V2) %>%
          mutate(tot_pop = sum(value))  %>%
          group_by(year, V2, variable) %>%
          mutate(pop_per = round(value/tot_pop,2))
        
        
        # set condition for if only 1 year chosen, then set x lab to that year, otherwise blank, and add the extra theme, otherwise dont
        if(length(unique(temp_dat$year)) == 1){
          x_label <- as.character(unique(temp_dat$year))
          new_theme <- theme(axis.text.x=element_blank(),
                             axis.ticks.x=element_blank()) 
        } else {
          x_label <- ''
          new_theme <- NULL
        }
        
        cols <- colorRampPalette(brewer.pal(9, 'Reds'))(length(unique(temp_dat$variable)))
        g <- ggplot(data = temp_dat,
                    aes(x = year,
                        y = pop_per,
                        fill = variable,
                        text = paste('Total population 15-29 year old: ', tot_pop,
                                     '<br>', pop_per , '%', as.factor(variable)))) +
          scale_fill_manual(name = '',
                            values = cols) + scale_y_continuous(labels = scales::percent) +
          geom_bar(position = 'dodge', stat = 'identity', colour = 'black', alpha = 0.8) + 
          theme_bw(base_size = 14, base_family = 'Ubuntu') + labs(x = x_label, y = ' ') + 
          new_theme + facet_wrap(~V2) 
        
        
        final_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
          config(displayModeBar = F) %>% 
          layout( 
            legend = list(
              orientation = "l",
              x = 0,
              y = -0.6))
      }
      
      if(house_demo_variable == 'Place of Birth') {
        
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <-
          temp$`Aboriginal identity` <- temp$`Sex` <- temp$`Visible minority` <-   NULL
        
        temp$Other <- temp$Population - (temp$`Living in owner occupied dwelling` + temp$`Living in rented dwelling`)
        temp$Population <- NULL
        
        temp_melt <- melt(temp, id.vars = c('year', house_demo_variable))
        
        
        temp_melt$year <- as.factor(temp_melt$year)
        temp_melt$value <- as.numeric(temp_melt$value)
        
        
        temp_demo_lab <- colnames(temp_melt)[2]
        colnames(temp_melt)[2] <- 'V2'
        # group by and get population and percent for each year
        temp_dat <- temp_melt %>%
          group_by(year, V2) %>%
          mutate(tot_pop = sum(value))  %>%
          group_by(year, V2, variable) %>%
          mutate(pop_per = round(value/tot_pop, 2))
        
        
        # set condition for if only 1 year chosen, then set x lab to that year, otherwise blank, and add the extra theme, otherwise dont
        if(length(unique(temp_dat$year)) == 1){
          x_label <- as.character(unique(temp_dat$year))
          new_theme <- theme(axis.text.x=element_blank(),
                             axis.ticks.x=element_blank()) 
        } else {
          x_label <- ''
          new_theme <- NULL
        }
        
        
        # plot data
        cols <- colorRampPalette(brewer.pal(9, 'Reds'))(length(unique(temp_dat$variable)))
        g <- ggplot(data = temp_dat,
                    aes(x = year,
                        y = pop_per,
                        fill = variable,
                        text = paste('Total population 15-29 year old: ', tot_pop,
                                     '<br>', pop_per , '%', as.factor(variable)))) +
          scale_fill_manual(name = '',
                            values = cols) + scale_y_continuous(labels = scales::percent) +
          geom_bar(position = 'dodge', stat = 'identity', colour = 'black', alpha = 0.8) + 
          theme_bw(base_size = 14, base_family = 'Ubuntu') + labs(x = x_label, y = ' ') +
          new_theme
        
        g <- g  + facet_wrap(~V2)
        
        
        final_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
          config(displayModeBar = F) %>% 
          layout( 
            legend = list(
              orientation = "l",
              x = 0,
              y = -0.6))
        
      }
      
      if(house_demo_variable == 'Visible minority') {
        
        if(is.null(input$owner_plot_vm_filter)) {
          return(NULL)
        } else {
          owner_plot_vm_filter <- input$owner_plot_vm_filter
          # get data
          temp <- new_census %>%
            filter(!grepl('Total', `Visible minority`)) %>%
            filter(grepl('Total', `Place of Birth`)) %>%
            filter(grepl('Total', `Sex`)) %>%
            filter(grepl('Total', `Aboriginal identity`))
          
          temp$`Age group` <- temp$Geography <- temp$geo_code <-
            temp$`Place of Birth` <- temp$Sex <- temp$`Aboriginal identity` <-   NULL
          
          # remove Arab/West Asian
          temp <- temp[temp$`Visible minority` != 'Arab/West Asian',]
          temp$Other <- temp$Population - (temp$`Living in owner occupied dwelling` + temp$`Living in rented dwelling`)
          temp$Population <- NULL
          
          temp_melt <- melt(temp, id.vars = c('year', house_demo_variable))
          
          
          temp_melt$year <- as.factor(temp_melt$year)
          temp_melt$value <- as.numeric(temp_melt$value)
          
          
          temp_demo_lab <- colnames(temp_melt)[2]
          colnames(temp_melt)[2] <- 'V2'
          # group by and get population and percent for each year
          temp_dat <- temp_melt %>%
            group_by(year, V2) %>%
            mutate(tot_pop = sum(value))  %>%
            group_by(year, V2, variable) %>%
            mutate(pop_per = round(value/tot_pop,2))
          
          temp_dat <- temp_dat %>% filter(V2 %in% owner_plot_vm_filter)
          
          
          # set condition for if only 1 year chosen, then set x lab to that year, otherwise blank, and add the extra theme, otherwise dont
          if(length(unique(temp_dat$year)) == 1){
            x_label <- as.character(unique(temp_dat$year))
            new_theme <- theme(axis.text.x=element_blank(),
                               axis.ticks.x=element_blank()) 
          } else {
            x_label <- ''
            new_theme <- NULL
          }
          # plot data
          cols <- colorRampPalette(brewer.pal(9, 'Reds'))(length(unique(temp_dat$variable)))
          g <- ggplot(data = temp_dat,
                      aes(x = year,
                          y = pop_per,
                          fill = variable,
                          text = paste('Total population 15-29 year old: ', tot_pop,
                                       '<br>', pop_per , '%', as.factor(variable)))) +
            scale_fill_manual(name = '',
                              values = cols) + scale_y_continuous(labels = scales::percent)+ 
            geom_bar(position = 'dodge', stat = 'identity', colour = 'black', alpha = 0.8) +
            theme_bw(base_size = 14, base_family = 'Ubuntu') + labs(x = '', y = ' ')
          
          if(length(owner_plot_vm_filter) > 1){
            g <- g  + facet_wrap(~V2) + 
              theme_bw(base_size = 14, base_family = 'Ubuntu') + labs(x = x_label, y = '') +
              new_theme
            
          }
          final_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
            config(displayModeBar = F) %>% 
            layout( 
              legend = list(
                orientation = "l",
                x = 0,
                y = -0.6))
        }
        
        
        
      }
      
    }
    
    if(house_demo_variable == 'Aboriginal identity') {
      # get data
      temp <- new_census %>%
        filter(!grepl('Total', `Aboriginal identity`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Visible minority`))
      
      temp$`Age group` <- temp$Geography <- temp$geo_code <-
        temp$`Place of Birth` <- temp$Sex <- temp$`Visible minority` <-   NULL
      
      temp$Other <- temp$Population - (temp$`Living in owner occupied dwelling` + temp$`Living in rented dwelling`)
      temp$Population <- NULL
      
      temp_melt <- melt(temp, id.vars = c('year', house_demo_variable))
      
      
      temp_melt$year <- as.factor(temp_melt$year)
      temp_melt$value <- as.numeric(temp_melt$value)
      
      
      temp_demo_lab <- colnames(temp_melt)[2]
      colnames(temp_melt)[2] <- 'V2'
      # group by and get population and percent for each year
      temp_dat <- temp_melt %>%
        group_by(year, V2) %>%
        mutate(tot_pop = sum(value))  %>%
        group_by(year, V2, variable) %>%
        mutate(pop_per = round(value/tot_pop,2))
      
      
      
      # set condition for if only 1 year chosen, then set x lab to that year, otherwise blank, and add the extra theme, otherwise dont
      if(length(unique(temp_dat$year)) == 1){
        x_label <- as.character(unique(temp_dat$year))
        new_theme <- theme(axis.text.x=element_blank(),
                           axis.ticks.x=element_blank()) 
      } else {
        x_label <- ''
        new_theme <- NULL
      }
      
      # plot data
      cols <- colorRampPalette(brewer.pal(9, 'Reds'))(length(unique(temp_dat$variable)))
      g <- ggplot(data = temp_dat,
                  aes(x = year,
                      y = pop_per,
                      fill = variable,
                      text = paste('Total population 15-29 year old: ', tot_pop,
                                   '<br>', pop_per , '%', as.factor(variable)))) +
        scale_fill_manual(name = '',
                          values = cols) + scale_y_continuous(labels = scales::percent) +
        geom_bar(position = 'dodge', stat = 'identity', colour = 'black', alpha = 0.8) + 
        theme_bw(base_size = 14, base_family = 'Ubuntu') + labs(x = x_label, y = ' ') +
        new_theme
      
      g <- g  + facet_wrap(~V2)
      
      final_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
        config(displayModeBar = F) %>% 
        layout( 
          legend = list(
            orientation = "l",
            x = 0,
            y = -0.6))
      
    }
    
    final_plot
    
  })
  
  #
  output$owner_table <- renderDataTable({
    
    
    # subset data by inputs
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    house_demo_variable <- 'Sex'
    # avg_years <- TRUE
    
    location <- input$location
    years <- input$years
    house_demo_variable <- input$house_demo_variable
    # avg_years <- input$demo_chart_avg
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex",
                   "Place of Birth","Visible minority", "Aboriginal identity",
                   "Living in owner occupied dwelling", "Living in rented dwelling", "Population")
    new_census <- census[ , demo_vars]
    
    new_census <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(grepl('Total',`Age group`))
    # 
    if(house_demo_variable == 'All youth'){
      # get data
      temp <- new_census %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
        temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
      
      temp$Other <- temp$Population - (temp$`Living in owner occupied dwelling` + temp$`Living in rented dwelling`)
      temp$Population <- NULL
      
      
      temp_melt <- melt(temp, id.vars = 'year')
      
      temp_melt$year <- as.factor(temp_melt$year)
      temp_melt$value <- as.numeric(temp_melt$value)
      
      # group by and get population and percent for each year
      temp_dat <- temp_melt %>%
        group_by(year) %>%
        mutate(tot_pop = sum(value))  %>%
        group_by(year, variable) %>%
        mutate(pop_per = round((value/tot_pop)*100,2))
      
      temp_dat$tot_pop <- NULL
      
      # recod for table 
      colnames(temp_dat) <- c('Year', 'Housing status', 'Total','Percent')
      
    }
    
    if(house_demo_variable == 'Sex') {
      # get data
      temp <- new_census %>%
        filter(!grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$Geography <- temp$geo_code <-
        temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
      temp$Other <- temp$Population - (temp$`Living in owner occupied dwelling` + temp$`Living in rented dwelling`)
      temp$Population <- NULL
      
      temp_melt <- melt(temp, id.vars = c('year', house_demo_variable))
      
      
      temp_melt$year <- as.factor(temp_melt$year)
      temp_melt$value <- as.numeric(temp_melt$value)
      
      
      temp_demo_lab <- colnames(temp_melt)[2]
      colnames(temp_melt)[2] <- 'V2'
      # group by and get population and percent for each year
      temp_dat <- temp_melt %>%
        group_by(year, V2) %>%
        mutate(tot_pop = sum(value))  %>%
        group_by(year, V2, variable) %>%
        mutate(pop_per = round((value/tot_pop)*100,2))
      
      temp_dat$tot_pop <- NULL
      
      # recod for table 
      colnames(temp_dat) <- c('Year', 'Sex', 'Housing status', 'Total','Percent')
      
      
    }
    
    if(house_demo_variable == 'Place of Birth') {
      
      # get data
      temp <- new_census %>%
        filter(!grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$Geography <- temp$geo_code <-
        temp$`Aboriginal identity` <- temp$`Sex` <- temp$`Visible minority` <-   NULL
      
      temp$Other <- temp$Population - (temp$`Living in owner occupied dwelling` + temp$`Living in rented dwelling`)
      temp$Population <- NULL
      
      temp_melt <- melt(temp, id.vars = c('year', house_demo_variable))
      
      
      temp_melt$year <- as.factor(temp_melt$year)
      temp_melt$value <- as.numeric(temp_melt$value)
      
      
      temp_demo_lab <- colnames(temp_melt)[2]
      colnames(temp_melt)[2] <- 'V2'
      # group by and get population and percent for each year
      temp_dat <- temp_melt %>%
        group_by(year, V2) %>%
        mutate(tot_pop = sum(value))  %>%
        group_by(year, V2, variable) %>%
        mutate(pop_per = round((value/tot_pop)*100,2))
      
      temp_dat$tot_pop <- NULL
      
      # recod for table 
      colnames(temp_dat) <- c('Year', 'Place of birth', 'Housing status', 'Total','Percent')
      
    }
    
    if(house_demo_variable == 'Visible minority') {
      # get data
      temp <- new_census %>%
        filter(!grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      
      temp$`Age group` <- temp$Geography <- temp$geo_code <-
        temp$`Place of Birth` <- temp$Sex <- temp$`Aboriginal identity` <-   NULL
      
      # remove Arab/West Asian
      temp <- temp[temp$`Visible minority` != 'Arab/West Asian',]
      temp <- temp[temp$`Visible minority` != 'All visible minorities',]
      temp$Other <- temp$Population - (temp$`Living in owner occupied dwelling` + temp$`Living in rented dwelling`)
      temp$Population <- NULL
      
      temp_melt <- melt(temp, id.vars = c('year', house_demo_variable))
      
      
      temp_melt$year <- as.factor(temp_melt$year)
      temp_melt$value <- as.numeric(temp_melt$value)
      
      
      temp_demo_lab <- colnames(temp_melt)[2]
      colnames(temp_melt)[2] <- 'V2'
      # group by and get population and percent for each year
      temp_dat <- temp_melt %>%
        group_by(year, V2) %>%
        mutate(tot_pop = sum(value))  %>%
        group_by(year, V2, variable) %>%
        mutate(pop_per = round((value/tot_pop)*100,2))
      
      temp_dat$tot_pop <- NULL
      
      # recod for table 
      colnames(temp_dat) <- c('Year', 'Visible minority', 'Housing status', 'Total','Percent')
      
      
    }
    
    if(house_demo_variable == 'Aboriginal identity') {
      # get data
      temp <- new_census %>%
        filter(!grepl('Total', `Aboriginal identity`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Visible minority`))
      
      temp$`Age group` <- temp$Geography <- temp$geo_code <-
        temp$`Place of Birth` <- temp$Sex <- temp$`Visible minority` <-   NULL
      
      temp$Other <- temp$Population - (temp$`Living in owner occupied dwelling` + temp$`Living in rented dwelling`)
      temp$Population <- NULL
      
      temp_melt <- melt(temp, id.vars = c('year', house_demo_variable))
      
      
      temp_melt$year <- as.factor(temp_melt$year)
      temp_melt$value <- as.numeric(temp_melt$value)
      
      
      temp_demo_lab <- colnames(temp_melt)[2]
      colnames(temp_melt)[2] <- 'V2'
      # group by and get population and percent for each year
      temp_dat <- temp_melt %>%
        group_by(year, V2) %>%
        mutate(tot_pop = sum(value))  %>%
        group_by(year, V2, variable) %>%
        mutate(pop_per = round((value/tot_pop)*100,2))
      temp_dat$tot_pop <- NULL
      
      # recod for table 
      colnames(temp_dat) <- c('Year', 'Aboriginal identity', 'Housing status', 'Total','Percent')
      
    }
    
    prettify(temp_dat, cap_columns = TRUE, comma_numbers = TRUE, nrows = 5, download_options = TRUE)
    
  })
  #   
  # 
  #####
  # subsidized housing
  
  # ownder occupied vs rented 
  # owner_rented_table_plot, sub_table_plot
  
  output$sub_plot_vm_filter <- renderUI({
    
    if(input$sub_demo_variable !='Visible minority' | is.null(input$sub_demo_variable)){
      NULL
    } else {
      choice_vm <- unique(census$`Visible minority`)
      choice_vm <- choice_vm[!grepl('Arab/West|Total', choice_vm)]
      selectInput('owner_plot_vm_filter',
                  'Take a closer look',
                  choices = choice_vm,
                  selected = 'All visible minorities',
                  multiple = TRUE)
    }
  })
  
  # by gender 
  output$sub_plot <- renderPlotly({
    
    
    # subset data by inputs
    location <- 'Ontario'
    years <- c(2016)
    sub_demo_variable <- 'Sex'
    # avg_years <- TRUE
    
    location <- input$location
    years <- input$years
    sub_demo_variable <- input$sub_demo_variable
    
    if(is.null(input$sub_demo_variable)){
      NULL
    } else {
      # avg_years <- input$demo_chart_avg
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex",
                     "Place of Birth","Visible minority", "Aboriginal identity",
                     "Subsidized housing", "Non-subsidized housing",
                     "Living in rented dwelling")
      new_census <- census[ , demo_vars]
      
      new_census <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl('Total',`Age group`))
      
      # remove na 
      new_census <- new_census[complete.cases(new_census$`Subsidized housing`),]
      
      if(sub_demo_variable == 'All youth'){
        # get data
        temp <- new_census %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
          temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
        
        # get percentages 
        temp$`Subsidized` <- round(temp$`Subsidized housing`/temp$`Living in rented dwelling`,2)
        temp$`Not subisdized` <- round(temp$`Non-subsidized housing`/temp$`Living in rented dwelling`,2)
        temp$`Subsidized housing` <- temp$`Non-subsidized housing` <- temp$`Living in rented dwelling` <- NULL
        temp_melt <- melt(temp, id.vars = 'year')
        
        temp_melt$year <- as.factor(temp_melt$year)
        
        temp_plot <- emp_line(temp_melt)
        g <- temp_plot 
        
        sub_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
          config(displayModeBar = F) %>% 
          layout( 
            legend = list(
              orientation = "l",
              x = 0,
              y = -0.4))
        
        
      }
      
      if(sub_demo_variable == 'Sex') {
        # get data
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
        
        # get percentages 
        temp$`Subsidized` <- round(temp$`Subsidized housing`/temp$`Living in rented dwelling`,2)
        temp$`Not subisdized` <- round(temp$`Non-subsidized housing`/temp$`Living in rented dwelling`,2)
        temp$`Subsidized housing` <- temp$`Non-subsidized housing` <- temp$`Living in rented dwelling` <- NULL
        temp_melt <- melt(temp, id.vars =c('year', 'Sex'))
        
        temp_melt$year <- as.factor(temp_melt$year)
        
        
        # set condition for if only 1 year chosen, then set x lab to that year, otherwise blank, and add the extra theme, otherwise dont
        
        temp_plot <- emp_line(temp_melt)
        g <- temp_plot + facet_wrap(~Sex)
        
        sub_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
          config(displayModeBar = F) %>% 
          layout( 
            legend = list(
              orientation = "l",
              x = 0,
              y = -0.4))
        
      }
      
      if(sub_demo_variable == 'Place of Birth') {
        # get data
        # get data
        temp <- new_census %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(!grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Aboriginal identity` <- temp$`Sex` <- temp$`Visible minority` <-   NULL
        
        # get percentages 
        temp$`Subsidized` <- round(temp$`Subsidized housing`/temp$`Living in rented dwelling`,2)
        temp$`Not subisdized` <- round(temp$`Non-subsidized housing`/temp$`Living in rented dwelling`,2)
        temp$`Subsidized housing` <- temp$`Non-subsidized housing` <- temp$`Living in rented dwelling` <- NULL
        temp_melt <- melt(temp, id.vars =c('year', 'Place of Birth'))
        
        temp_melt$year <- as.factor(temp_melt$year)
        
        temp_plot <- emp_line(temp_melt)
        g <- temp_plot + facet_wrap(~`Place of Birth`)
        
        sub_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
          config(displayModeBar = F) %>% 
          layout( 
            legend = list(
              orientation = "l",
              x = 0,
              y = -0.4))
      }
      
      if(sub_demo_variable == 'Visible minority') {
        
        if(is.null(input$owner_plot_vm_filter)) {
          return(NULL)
        } else {
          owner_plot_vm_filter <- input$owner_plot_vm_filter
          # get data
          # get data
          temp <- new_census %>%
            filter(grepl('Total', `Sex`)) %>%
            filter(grepl('Total', `Place of Birth`)) %>%
            filter(!grepl('Total', `Visible minority`)) %>%
            filter(grepl('Total', `Aboriginal identity`))
          temp$`Age group` <- temp$Geography <- temp$geo_code <- 
            temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Sex` <-   NULL
          
          # get percentages 
          temp$`Subsidized` <- round(temp$`Subsidized housing`/temp$`Living in rented dwelling`,2)
          temp$`Not subisdized` <- round(temp$`Non-subsidized housing`/temp$`Living in rented dwelling` ,2)
          temp$`Subsidized housing` <- temp$`Non-subsidized housing` <- temp$`Living in rented dwelling` <- NULL
          # remove Arab/West Asian
          temp <- temp[temp$`Visible minority` != 'Arab/West Asian',]
          temp_melt <- melt(temp, id.vars =c('year', 'Visible minority'))
          temp_melt <- temp_melt %>% filter(`Visible minority`%in% owner_plot_vm_filter)
          
          
          temp_melt$year <- as.factor(temp_melt$year)
          
          temp_plot <- emp_line(temp_melt)
          g <- temp_plot +
            facet_wrap(~`Visible minority`)
          
          sub_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
            config(displayModeBar = F) %>% 
            layout( 
              legend = list(
                orientation = "l",
                x = 0,
                y = -0.4))
          
        }
        
      }
      
    }
    
    if(sub_demo_variable == 'Aboriginal identity') {
      # get data
      
      # get data
      temp <- new_census %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(!grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$Geography <- temp$geo_code <- 
        temp$`Sex` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
      
      # get percentages 
      temp$`% Subsidized` <- round(temp$`Subsidized housing`/temp$`Living in rented dwelling`,2)
      temp$`% Not subisdized` <- round(temp$`Non-subsidized housing`/temp$`Living in rented dwelling`,2)
      temp$`Subsidized housing` <- temp$`Non-subsidized housing` <- temp$`Living in rented dwelling` <- NULL
      temp_melt <- melt(temp, id.vars =c('year', 'Aboriginal identity'))
      
      temp_melt$year <- as.factor(temp_melt$year)
      
      temp_plot <- emp_line(temp_melt)
      g <- temp_plot + facet_wrap(~`Aboriginal identity`)
      sub_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
        config(displayModeBar = F) %>% 
        layout( 
          legend = list(
            orientation = "l",
            x = 0,
            y = -0.4))
    }
 
    sub_plot
    
  })
  
  # #
  output$sub_table <- DT::renderDataTable({
    
    # subset data by inputs
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    sub_demo_variable <- 'All youth'
    # avg_years <- TRUE
    
    location <- input$location
    years <- input$years
    sub_demo_variable <- input$sub_demo_variable
    
    if(is.null(input$sub_demo_variable)){
      NULL
    } else {
      # avg_years <- input$demo_chart_avg
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex",
                     "Place of Birth","Visible minority", "Aboriginal identity",
                     "Subsidized housing", "Non-subsidized housing",
                     "Living in rented dwelling")
      new_census <- census[ , demo_vars]
      
      new_census <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl('Total',`Age group`))
      
      # remove na 
      new_census <- new_census[complete.cases(new_census$`Subsidized housing`),]
      
      if(sub_demo_variable == 'All youth'){
        # get data
        temp <- new_census %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- temp$Sex <-
          temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
        
        # get percentages 
        temp$`% Subsidized` <- round((temp$`Subsidized housing`/temp$`Living in rented dwelling`)*100,2)
        temp$`% Not subisdized` <- round((temp$`Non-subsidized housing`/temp$`Living in rented dwelling`)*100,2)
        temp$`Subsidized housing` <- temp$`Non-subsidized housing` <- temp$`Living in rented dwelling` <- NULL
        
      }
      
      if(sub_demo_variable == 'Sex') {
        # get data
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
        
        # get percentages 
        temp$`% Subsidized` <- round((temp$`Subsidized housing`/temp$`Living in rented dwelling`)*100,2)
        temp$`% Not subisdized` <- round((temp$`Non-subsidized housing`/temp$`Living in rented dwelling`)*100,2)
        temp$`Subsidized housing` <- temp$`Non-subsidized housing` <- temp$`Living in rented dwelling` <- NULL
      }
      
      if(sub_demo_variable == 'Place of Birth') {
        # get data
        # get data
        temp <- new_census %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(!grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$Geography <- temp$geo_code <- 
          temp$`Aboriginal identity` <- temp$`Sex` <- temp$`Visible minority` <-   NULL
        
        # get percentages 
        temp$`% Subsidized` <- round((temp$`Subsidized housing`/temp$`Living in rented dwelling`)*100,2)
        temp$`% Not subisdized` <- round((temp$`Non-subsidized housing`/temp$`Living in rented dwelling`)*100,2)
        temp$`Subsidized housing` <- temp$`Non-subsidized housing` <- temp$`Living in rented dwelling` <- NULL
        
      }
      
      if(sub_demo_variable == 'Visible minority') {
        
        if(is.null(input$owner_plot_vm_filter)) {
          return(NULL)
        } else {
          owner_plot_vm_filter <- input$owner_plot_vm_filter
          # get data
          # get data
          temp <- new_census %>%
            filter(grepl('Total', `Sex`)) %>%
            filter(grepl('Total', `Place of Birth`)) %>%
            filter(!grepl('Total', `Visible minority`)) %>%
            filter(grepl('Total', `Aboriginal identity`))
          temp$`Age group` <- temp$Geography <- temp$geo_code <- 
            temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Sex` <-   NULL
          
          # get percentages 
          temp$`% Subsidized` <- round((temp$`Subsidized housing`/temp$`Living in rented dwelling`)*100,2)
          temp$`% Not subisdized` <- round((temp$`Non-subsidized housing`/temp$`Living in rented dwelling`)*100,2)
          temp$`Subsidized housing` <- temp$`Non-subsidized housing` <- temp$`Living in rented dwelling` <- NULL
          # remove Arab/West Asian
          temp <- temp[temp$`Visible minority` != 'Arab/West Asian',]
          
        }
        
      }
      
    }
    
    if(sub_demo_variable == 'Aboriginal identity') {
      # get data
      
      # get data
      temp <- new_census %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(!grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$Geography <- temp$geo_code <- 
        temp$`Sex` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
      
      # get percentages 
      temp$`% Subsidized` <- round((temp$`Subsidized housing`/temp$`Living in rented dwelling`)*100,2)
      temp$`% Not subisdized` <- round((temp$`Non-subsidized housing`/temp$`Living in rented dwelling`)*100,2)
      temp$`Subsidized housing` <- temp$`Non-subsidized housing` <- temp$`Living in rented dwelling` <- NULL
      
      
      
      
    }
    
    prettify(temp, cap_columns = TRUE, comma_numbers = TRUE, nrows = 5, download_options = TRUE)
    
  })
  # 
  
  
  #------------------------------------------------------------------------------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------------------------------------------------------------------------------
  # income_plot_all_geo - line plot of all provinces by median income, can filter by province
  # income_plot_vm - line plot for census track chosen, income by vm
  # income_status_map_all_geo - map of percent low income status of ontario
  # income_status_bar - bar plot for census track chosen, percent low income status by gender.
  
  # output$income_add_location <- renderUI({
  #   current_location <- input$location
  #   other_locations <- unique(census$Geography)
  #   add_location_choice <- other_locations[other_locations != current_location]
  #   
  #   selectInput('income_add_location',
  #               'Select additional locations',
  #               choices = c('Ontario', add_location_choice),
  #               selected = 'Ontario',
  #               multiple = TRUE)
  #   
  # })
  
  output$income_plot_all_geo <- renderPlotly({
    

      location <- input$location
      years <- input$years
      add_location <- input$income_add_location
      
      # avg_years <- input$demo_chart_avg
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex",
                     "Place of Birth","Visible minority", "Aboriginal identity",
                     "Average household income before tax $")
      new_census <- census[ , demo_vars]
      
      # combine location and add_locaiton
      location <- c(location, add_location)
      
      temp <- new_census %>%
        filter(year %in% years) %>% 
        filter(Geography %in% location) %>%
        filter(grepl('Total',`Age group`)) %>% 
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      temp$`Age group`  <- temp$geo_code <- temp$Sex <- temp$`Aboriginal identity` <- 
        temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
      temp$`Average household income before tax $` <- round(temp$`Average household income before tax $`)
      
      temp$year <- as.factor(as.character(temp$year))
      temp$`Average household income before tax $` <- round(temp$`Average household income before tax $`)
      cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$Geography)))
      
      # set condition for if only 1 year is chosent 
      if(length(unique(temp$year)) == 1) {
        point_size <- 13
      } else {
        point_size <- 4
      }
      
      # plot data
      g <- ggplot(data = temp,
                  aes(x = year,
                      y = round(`Average household income before tax $`, 2),
                      group = Geography,
                      colour = Geography,
                      text = paste('<br>', `Average household income before tax $` , as.factor(Geography)))) +
        geom_point(size = point_size) +
        geom_line(size = 1, alpha = 0.8,linetype = 'dashed') +
        geom_smooth(alpha = 0.4, size = 1) +     theme_bw(base_size = 13, base_family = 'Ubuntu') +
        scale_color_manual(name = '', 
                           values = cols) + theme(legend.position="none") +
        labs(x = '', y = '', title ='') + scale_y_continuous(labels=scales::comma) 
      
      sub_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
        config(displayModeBar = F) %>% 
        layout( 
          legend = list(
            orientation = "l",
            x = 0,
            y = -0.4))
      
      sub_plot
    
    
  })
  
  # plot avg household income across vm status over the years
  # income_vm_filter = 'All visible minorites' as default
  output$income_plot_vm <- renderPlotly({
    # subset data by inputs
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    income_vm_filter = 'All visible minorities' 
    
    location <- input$location
    years <- input$years
    income_vm_filter <- input$income_vm_filter
    
    # avg_years <- input$demo_chart_avg
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex",
                   "Place of Birth","Visible minority", "Aboriginal identity",
                   "Average household income before tax $")
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>%
      filter(year %in% years) %>% 
      filter(Geography %in% location) %>%
      filter(grepl('Total',`Age group`)) %>% 
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(!grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp$`Age group`  <- temp$geo_code <- temp$Sex <- temp$`Aboriginal identity` <- 
      temp$`Place of Birth` <-    NULL
    temp$`Average household income before tax $` <- round(temp$`Average household income before tax $`)
    
    # subet by income_vm_filter to get at least one line 
    temp <- temp %>% filter(`Visible minority` %in% income_vm_filter)
    
    temp$year <- as.factor(as.character(temp$year))
    temp$`Average household income before tax $` <- round(temp$`Average household income before tax $`)
    cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$`Visible minority`)))
    
    # set condition for if only 1 year is chosent 
    if(length(unique(temp$year)) == 1) {
      point_size <- 13
    } else {
      point_size <- 4
    }
    
    # plot data
    g <- ggplot(data = temp,
                aes(x = year,
                    y = round(`Average household income before tax $`, 2),
                    group = `Visible minority`,
                    colour = `Visible minority`,
                    text = paste('<br>', `Average household income before tax $` , as.factor(`Visible minority`)))) +
      geom_point(size = point_size) +
      geom_line(size = 1, alpha = 0.8,linetype = 'dashed') +
      geom_smooth(alpha = 0.4, size = 1) + theme_bw(base_size = 13, base_family = 'Ubuntu') +
      scale_color_manual(name = '', 
                         values = c('darkblue',cols)) + theme(legend.position="none") +
      labs(x = '', y = '', title ='') + scale_y_continuous(labels=scales::comma) 
    
    sub_plot <- plotly::ggplotly(g, tooltip = 'text') %>%
      config(displayModeBar = F) %>% 
      layout( 
        legend = list(
          orientation = "l",
          x = 0,
          y = -0.4))
    
    sub_plot
  })
  
  
  ######## 
  # percent low income status map 
  # income_status_map_demo : radiobuttons = c('Age group', 'Sex', 'Visible minority', 'Aboriginal identity')
  # income_status_map_demo_filter: uiOutput: selecInput: levels of income_status_map_demo
  # map: income_status_map_all_geo
  # table: income_status_table
  
 
  output$income_status_map_demo_filter_ui <- renderUI({

    if(is.null(input$income_status_map_demo)){
      return(NULL)
    } else {
      income_status_map_demo <-input$income_status_map_demo
      map_demo_levels <- as.data.frame(unique(census[, income_status_map_demo]))
      map_demo_levels <- as.character(map_demo_levels[,1])
      if(income_status_map_demo == 'Age group') {
        selectInput('income_status_map_demo_filter',
                    'Pick a sub group to map',
                    choices = map_demo_levels,
                    selected = "Total - 15 to 29 years",
                    multiple = FALSE)
      } else {
        map_demo_levels <- map_demo_levels[!grepl('Total', map_demo_levels)]
        selectInput('income_status_map_demo_filter',
                    'Pick a sub group to map',
                    choices = map_demo_levels,
                    selected = map_demo_levels[1],
                    multiple = FALSE)
      }
    }

  })

 
  
  
  output$income_status_map_all_geo <- renderLeaflet({
    # # subset data by inputs
    # income_map_year <- c(2016)
    # income_status_map_demo <- 'Sex'
    # income_status_map_demo_filter <- "Male"
    income_status_map_demo <- input$income_status_map_demo
    income_map_year <- input$income_map_year
    income_status_map_demo_filter <- input$income_status_map_demo_filter
    
      if( is.null(income_status_map_demo) | is.null(income_map_year) | is.null(income_status_map_demo_filter)){
        return(NULL)
      } else {

        # avg_years <- input$demo_chart_avg
        demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex",
                       "Place of Birth","Visible minority", "Aboriginal identity",
                       "Low income (LICO before tax)", 'Population')
        new_census <- census[ , demo_vars]

        new_census <- new_census %>% filter(!grepl('Ontario', Geography)) %>%
          filter(year %in% income_map_year)

        if(income_status_map_demo == 'Age group') {
          temp <- new_census %>%
            filter(grepl(income_status_map_demo_filter,`Age group`)) %>%
            filter(grepl('Total', `Sex`)) %>%
            filter(grepl('Total', `Place of Birth`)) %>%
            filter(grepl('Total', `Visible minority`)) %>%
            filter(grepl('Total', `Aboriginal identity`))
          temp$Sex <- temp$`Aboriginal identity` <- temp$`Visible minority` <- temp$`Place of Birth` <-    NULL
        }

        if(income_status_map_demo == 'Sex') {
          temp <- new_census %>%
            filter(grepl(income_status_map_demo_filter,`Sex`)) %>%
            filter(grepl('Total', `Age group`)) %>%
            filter(grepl('Total', `Place of Birth`)) %>%
            filter(grepl('Total', `Visible minority`)) %>%
            filter(grepl('Total', `Aboriginal identity`))
          temp$`Age group` <- temp$`Aboriginal identity` <- temp$`Visible minority` <- temp$`Place of Birth` <-    NULL
        }

        if(income_status_map_demo == 'Place of Birth') {
          temp <- new_census %>%
            filter(grepl(income_status_map_demo_filter,`Place of Birth`)) %>%
            filter(grepl('Total', `Sex`)) %>%
            filter(grepl('Total', `Age group`)) %>%
            filter(grepl('Total', `Visible minority`)) %>%
            filter(grepl('Total', `Aboriginal identity`))
          temp$Sex <- temp$`Aboriginal identity` <- temp$`Visible minority` <- temp$`Age group` <-    NULL
        }

        if(income_status_map_demo == 'Visible minority') {
          temp <- new_census %>%
            filter(grepl(income_status_map_demo_filter,`Visible minority`)) %>%
            filter(grepl('Total', `Sex`)) %>%
            filter(grepl('Total', `Place of Birth`)) %>%
            filter(grepl('Total', `Age group`)) %>%
            filter(grepl('Total', `Aboriginal identity`))
          temp$Sex <- temp$`Aboriginal identity` <- temp$`Age group` <- temp$`Place of Birth` <-    NULL
        }

        if(income_status_map_demo == 'Aboriginal identity') {
          temp <- new_census %>%
            filter(grepl(income_status_map_demo_filter,`Aboriginal identity`)) %>%
            filter(grepl('Total', `Sex`)) %>%
            filter(grepl('Total', `Place of Birth`)) %>%
            filter(grepl('Total', `Visible minority`)) %>%
            filter(grepl('Total', `Age group`))
          temp$Sex <- temp$`Age group` <- temp$`Visible minority` <- temp$`Place of Birth` <-    NULL
        }


        # get percentage
        temp_final <- as.data.frame(temp)
        temp_final$`Percent low income status` <-
          round((temp_final$`Low income (LICO before tax)`/temp_final$Population)*100,2)
        save(temp_final, file = 'x.RData')
        leaf_income(temp_final, income_status_map_demo_filter =  income_status_map_demo_filter )
      }



})
  
  
  output$income_status_table <- DT::renderDataTable({
    # # subset data by inputs
    income_map_year <- input$income_map_year
    income_status_map_demo <- input$income_status_map_demo
    income_status_map_demo_filter <- input$income_status_map_demo_filter
    if(is.null(income_status_map_demo_filter) |  is.null(income_status_map_demo) |is.null(income_map_year)){
      return(NULL)
    } else {


      # avg_years <- input$demo_chart_avg
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex",
                     "Place of Birth","Visible minority", "Aboriginal identity",
                     "Low income (LICO before tax)", 'Population')
      new_census <- census[ , demo_vars]

      new_census <- new_census %>% filter(!grepl('Ontario', Geography)) %>%
        filter(year %in% income_map_year)

      if(income_status_map_demo == 'Age group') {
        temp <- new_census %>%
          filter(grepl(income_status_map_demo_filter,`Age group`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$Sex <- temp$`Aboriginal identity` <- temp$`Visible minority` <- temp$`Place of Birth` <-    NULL
      }

      if(income_status_map_demo == 'Sex') {
        temp <- new_census %>%
          filter(grepl(income_status_map_demo_filter,`Sex`)) %>%
          filter(grepl('Total', `Age group`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$`Aboriginal identity` <- temp$`Visible minority` <- temp$`Place of Birth` <-    NULL
      }

      if(income_status_map_demo == 'Place of Birth') {
        temp <- new_census %>%
          filter(grepl(income_status_map_demo_filter,`Place of Birth`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Age group`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$Sex <- temp$`Aboriginal identity` <- temp$`Visible minority` <- temp$`Age group` <-    NULL
      }

      if(income_status_map_demo == 'Visible minority') {
        temp <- new_census %>%
          filter(grepl(income_status_map_demo_filter,`Visible minority`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Age group`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$Sex <- temp$`Aboriginal identity` <- temp$`Age group` <- temp$`Place of Birth` <-    NULL
      }

      if(income_status_map_demo == 'Aboriginal identity') {
        temp <- new_census %>%
          filter(grepl(income_status_map_demo_filter,`Aboriginal identity`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Age group`))
        temp$Sex <- temp$`Age group` <- temp$`Visible minority` <- temp$`Place of Birth` <-    NULL
      }
      #
    }
    # get percentage
    temp_final <- as.data.frame(temp)
    temp_final$`Percent low income status` <- round((temp_final$`Low income (LICO before tax)`/temp_final$Population)*100,2)
    temp_final$geo_code <- temp_final$year <- temp_final$Population <- NULL
    
    if(!is.null(temp_final)){
      if(nrow(temp_final) > 0){
        message('temp final is')
        print(head(temp_final))
        prettify(temp_final, comma_numbers = TRUE,download_options = TRUE)
      }
    }
    
    

  })


  
  }

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

