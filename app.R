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
                      
                      fluidPage(
                        fluidRow(shinydashboard::box(
                          title = '',
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = FALSE,
                          collapsed = FALSE,
                          column(3,
                                 selectInput('location',
                                             'Location',
                                             choices = unique(census$Geography),
                                             selected = 'Ontario')),
                          column(3,
                                 selectInput('years',
                                             'Years',
                                             choices = unique(census$year),
                                             selected = unique(census$year),
                                             multiple = TRUE)),
                          fluidRow(column(12,
                                          textOutput('map_text'),
                                          leafletOutput('the_map')))
                        )
                        
                        ),
                        
                        # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Demographics',
                            status = 'primary',
                            solidHeader = TRUE,
                            width = 13,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            
                            fluidRow(column(6,
                                            uiOutput('location_header'),
                                            htmlOutput('demo_plot_pie'),
                                     DT::dataTableOutput('pie_table')),
                                     br(),br(),
                                     column(6,
                                            selectInput('demo_variable',
                                                        'Exampine by ',
                                                        choices =  c('Sex', 
                                                                     'Place of Birth', 
                                                                     'Visible minority',
                                                                     'Aboriginal identity'),
                                                        selected = 'Sex',
                                                        multiple = FALSE),
                                            uiOutput('demo_chart_avg'),
                                            uiOutput('demo_chart_table'),
                                            tabsetPanel(tabPanel('Plot', 
                                                                 br(), br(),
                                                                 plotlyOutput('demo_charts', 
                                                         height = '130%', width = '100%')),
                                                        tabPanel('Table',
                                            DT::dataTableOutput('demo_tables')))))

                            )

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
                            fluidRow(column(6,
                                            radioButtons('fam_type',
                                                         'Parental type',
                                                         choices = c('Lone parents', 'Spouses and common law partners'),
                                                         selected = 'Lone parents', 
                                                         inline = TRUE),
                                            uiOutput('fam_plot_parents')),
                                     column(6,
                                            br(), br(), 
                                            plotlyOutput('fam_plot_kids'))),
                            br(), 
                            fluidRow(column(6,
                                            selectInput('which_fam_type',
                                                        'Parental structure',
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
                                       status = 'success',
                                       background = 'green',                                  
                                       solidHeader = FALSE,
                                       width = 12,
                                       collapsible = FALSE,
                                       collapsed = FALSE,
                                       textOutput('ed_text_highschool'))
                                     ),
                              column(4,
                                     box(
                                       title = 'College certificate/diploma',
                                       solidHeader = FALSE,
                                       background = 'light-blue',                                  
                                       width = 12,
                                       collapsible = FALSE,
                                       collapsed = FALSE,
                                       textOutput('ed_text_college'))
                                     ),
                              
                              column(4,
                                     box(
                                       title = "Univeristy degree/diploma",
                                       status = 'info',
                                       solidHeader = FALSE,
                                       background = 'aqua',
                                       width = 12,
                                       collapsible = FALSE,
                                       collapsed = FALSE,
                                       textOutput('ed_text_university'))
                              )
                              
                            ),
                          fluidRow(
                            column(12,
                                   h3('Highschool or equivalent', align = 'center'),
                                   align = 'center',
                                   checkboxInput('ed_plot_or_table',
                                                'View as table', 
                                                value = FALSE),
                                   uiOutput('ed_plot_table'))
                          )  
                           
                            
                          )
                        ),
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Employment',
                            status = 'warning',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            fluidRow(
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
                                     selectInput('emp_gen',
                                                 'Sex',
                                                 choices = c('Male', 'Female'),
                                                 selected = "Female"),
                                     checkboxInput('emp_plot_or_table_gen',
                                                   'View as table', 
                                                   value = FALSE),
                                     uiOutput('emp_plot_table_gen'))
                            ),
                            
                            fluidRow(
                              column(6,
                                     box(
                                       title = 'Aboriginal',
                                       solidHeader = FALSE,
                                       background = 'navy',                                  
                                       width = 12,
                                       collapsible = FALSE,
                                       collapsed = FALSE,
                                       textOutput('emp_ab'))
                              ),
                              
                              column(6,
                                     box(
                                       title = "Non-Aboriginal",
                                       solidHeader = FALSE,
                                       background = 'teal',
                                       width = 12,
                                       collapsible = FALSE,
                                       collapsed = FALSE,
                                       textOutput('emp_non'))
                              )
                             
                            )
                          )
                        ),
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Housing',
                            status = 'danger',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            
                            column(6,
                                   plotOutput('house_plot')),
                            column(6,
                                   DT::dataTableOutput('house_table'))
                            
                          )
                        ),
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Income',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            
                            column(6,
                                   plotOutput('income_plot')),
                            column(6,
                                   DT::dataTableOutput('income_table'))
                            
                          )
                        )
     )
                      
                      
)
                    



# Define server 
server <- function(input, output) {
  
  
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
  output$demo_plot_pie <- renderGvis({

    if (is.null(input$years)) {
      return(NULL) 
    } else {
      # subset data by inputs 
      location <- 'Ontario'
      years <- c(2001, 2006, 2011, 2016)
      location <- input$location
      years <- input$years
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                     "Place of Birth","Visible minority", "Aboriginal identity", 'Population')
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>% 
        filter(year %in% years) %>% filter(!grepl('Total',`Age group`)) %>%
        filter(grepl('Total',`Sex`)) %>% filter(grepl('Total',`Place of Birth`)) %>%
        filter(grepl('Total',`Visible minority`)) %>% filter(grepl('Total',`Aboriginal identity`)) 
      
      # keep only age group, year, and population
      temp <- temp[, c('year','Age group','Population')]
      
      year_value <- unique(temp$year)
      temp$year <- NULL
      
      temp <- temp %>% group_by(`Age group`) %>%
        summarise(`Population` = round(mean(Population)))
      # year condition 
      if(length(year_value) == 1) {
        chart_title <- paste0('Population for ',year_value)
        gvisPieChart(temp, 
                     options=list(title=chart_title,
                                  fontSize = 17,
                                  width=530,
                                  height=400,
                                  legend= 'yes',
                                  pieSliceText = 'value'))
      } else {
        if(length(year_value) == 2){
          year_value <- paste0(year_value, collapse = ' and ')
        }
        if(length(year_value) == 3){
          year_value2 <- paste0(year_value[1:2], collapse = ' , ')
          year_value <- paste(year_value2, ' and ', year_value[3])
        } 
        if(length(year_value) == 4){
          year_value3 <- paste0(year_value[1:3], collapse = ' , ')
          year_value <- 'All years'
        } 
        
        chart_title <- paste0('Youth population ',year_value)
        gvisPieChart(temp, 
                     options=list(title=chart_title,
                                  fontSize = 17,
                                  width=530,
                                  height=400,
                                  legend= 'yes',
                                  pieSliceText = 'value'))
      }
  
    }
      
  })
  
  
  # demo_plot_pie
  output$pie_table <- renderDataTable({


    if (is.null(input$years)) {
      return(NULL)
    } else {
      # subset data by inputs
      location <- 'Ontario'
      years <- c(2001, 2006, 2011, 2016)
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
      # subset data by inputs
      location <- 'Ontario'
      years <- c(2001, 2006, 2011, 2016)
      demo_variable <- 'Place of Birth'
      avg_years <- TRUE
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
          
          if(avg_years){
            plotly_plot <- pie_plotly_demo(temp)
          } else {
            plotly_plot <- bar_plotly_demo(temp, no_legend = F)
          }
  
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
          
          
          if(avg_years){
            plotly_plot <- pie_plotly_demo(temp)
          } else {
            plotly_plot <- bar_plotly_demo(temp, no_legend = F)
          }
          
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
          
          
          if(avg_years){
            plotly_plot <- pie_plotly_demo(temp)
          } else {
            plotly_plot <- bar_plotly_demo(temp, no_legend = F)
          }          
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
          
          if(avg_years){
            plotly_plot <- pie_plotly_demo(temp)
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
      # subset data by inputs
      location <- 'Ontario'
      years <- c(2001)
      fam_type <- 'Spouses and common law partners'
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
          size = 15,
          color = "white"
        )
        
        p1 <-  plot_ly(temp_melt,labels = ~variable, values = ~value ,
                       type ='pie',
                       hole = 0.5,
                       textposition = 'inside',
                       textinfo = 'percent',
                       insidetextfont = f,
                       hoverinfo = 'label+value')  %>%
          
          layout(title = paste0('Youth: 25 to 29: ', fam_type), showlegend = F,
                 annotations = list(
                   font = list(color = '264E86',
                               family = 'sans serif',
                               size = 20)), 
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
                      options=list(title=paste0(fam_type, " (25-29 years old)"),
                                   titleTextStyle="{color:'black',
                             fontName:'Ubuntu',
                             fontSize:18}",
                                   curveType="function", 
                                   pointSize=15,
                                   series="[{targetAxisIndex:0, 
                                       color:'#28B463'}, 
                                     {targetAxisIndex:1,
                                      color:'#FF4C4C'}]",
                                   vAxes="[{title:'Population',
                             format:'##,###',
                             titleTextStyle: {color: 'green'},
                             textStyle:{color: 'green'},
                             textPosition: 'out'}, 
                            {title:'Percent',
                             format:'#.##',
                             titleTextStyle: {color: '#FF4C4C'},  
                             textStyle:{color: '#FF4C4C'},
                             textPosition: 'out'}]",
                                   hAxes="[{title:'Year',
                             textPosition: 'out'}]",
                                   width=450, height=450
                      ),
                      chartid="twoaxislinechart"
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
      years <- c(2001, 2006, 2011, 2016)
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
        mutate(pop_per =round((value/tot_pop)*100,  2))
      temp_dat$year <- as.factor(temp_dat$year)
      
      # plot data
      cols <- colorRampPalette(brewer.pal(9, 'RdBu'))(length(unique(temp_dat$variable)))
      g <- ggplot(data = temp_dat,
                  aes(x = year,
                      y = pop_per,
                      fill = variable, 
                      text = paste('Total population 15-19 year old: ', tot_pop,
                                   '<br>', pop_per , '%', as.factor(variable)))) +
        geom_bar(position = 'dodge', stat = 'identity', colour = 'black', alpha = 0.8) +
        # geom_text(aes(label = pop_per), position = position_dodge(width = 1), vjust = -0.5) +
        labs(x = '', y = 'Percent') + ggtitle('Children (15-19) living with single or coupled parents')
        g <- g  + theme_bw(base_size = 10, base_family = 'Ubuntu') 
        
        plotly::ggplotly(g, tooltip = 'text') %>%
          layout(autosize = T, width = 400, height = 450, legend = list(x = -0.05, y = -0.4))
        

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
      years <- c(2001, 2006, 2011, 2016)
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
      
      temp_all$per <- round((temp_all$V2/temp_all$Population)*100,2 )
      temp_vm$per <- round((temp_vm$V2/temp_vm$Population)*100,2 )
      
      
      if(fam_chart_table_all_or_vm){
        # plot all vm
        cols <- c('red', 'blue')
        g <- ggplot(data = temp_all,
                    aes(x = year,
                        y = per,
                        group = `Visible minority`,
                        colour = `Visible minority`,
                        text = paste('Total population of', `Visible minority`, ': ', Population,
                                     '<br>', per , '%'))) +
          geom_point(size = 6) + geom_line(size = 2, alpha = 0.6) + scale_color_manual(name = '',
                                                                                       values = cols) + theme_bw(base_size = 16, base_family = 'Ubuntu')  +
          
          
          labs(x = '', y = 'Percent') + ggtitle(paste0('Youth (25 to 29) who are ', which_fam_type))
        g <- g  + theme_bw(base_size = 8, base_family = 'Ubuntu') 
        
        p1 <- plotly::ggplotly(g, tooltip = 'text') %>%
          layout(height = 400)
      } else {
        # plot data
        cols <- colorRampPalette(brewer.pal(9, 'RdBu'))(length(unique(temp_vm$`Visible minority`)))
        g <- ggplot(data = temp_vm,
                    aes(x = year,
                        y = per,
                        group = `Visible minority`,
                        colour = `Visible minority`,
                        text = paste('Total population of', `Visible minority`, ': ', Population,
                                     '<br>', per , '%'))) +
          geom_point(size = 6) + geom_line(size = 2, alpha = 0.6) + scale_color_manual(name = '',
                                                                                       values = cols) + theme_bw(base_size = 16, base_family = 'Ubuntu')  +
          
          
          labs(x = '', y = 'Percent') + ggtitle(paste0('Youth (25 to 29) who are ', which_fam_type))
        g <- g  + theme_bw(base_size = 8, base_family = 'Ubuntu') 
        
        p1 <- plotly::ggplotly(g, tooltip = 'text') %>%
          layout(height = 400)
        
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


  output$ed_plot_table <- renderUI({
    if(!input$ed_plot_or_table){
      plotlyOutput('ed_plot')
      
    } else {
      DT::dataTableOutput('ed_table')
    }
    
  })
  
  # plot of highschool 20-29, gender
  output$ed_plot <- renderPlotly({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "High school or equivalent" ,'Population')
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(!grepl("15 to 19 years|Total",`Age group`)) %>%
      filter(!grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
   temp$Geography <- temp$geo_code  <-
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    temp$per <- (temp$`High school or equivalent`/temp$Population)
    
    # bar plot with year by percent, grouped by age and gender
    # plot data
    cols <- colorRampPalette(brewer.pal(9, 'RdBu'))(length(unique(temp$Sex)))
    g <- ggplot(data = temp,
                aes(x = year,
                    y = per,
                    fill = Sex, 
                    text = paste('Total population 15-19 year old: ', Population,
                                 '<br>', round((per*100), 2) , '%', as.factor(Sex)))) +
      geom_bar(position = 'dodge', stat = 'identity', colour = 'black', alpha = 0.6) +
      scale_fill_manual(name ='',
                        values = c('green3', 'deepskyblue')) + scale_y_continuous(labels = scales::percent) +
      # geom_text(aes(label = pop_per), position = position_dodge(width = 1), vjust = -0.5) +
      labs(x = '', y = 'Percent') 
    g <- g  + theme_bw(base_size = 14, base_family = 'Ubuntu') + facet_wrap(~`Age group`)
    
    plotly::ggplotly(g, tooltip = 'text') %>%
      layout(width = 850, height = 400)
    
  })
  
  # table of highschool 20-29, gender
  output$ed_table <- renderDataTable({
    location <- 'Ontario'
    years <- c(2001, 2006, 2011, 2016)
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "High school or equivalent" ,'Population')
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(!grepl("15 to 19 years|Total",`Age group`)) %>%
      filter(!grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    temp$Geography <- temp$geo_code  <-
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    temp$per <- round((temp$`High school or equivalent`/temp$Population)*100,2)
    
    prettify(temp, download_options = TRUE, round_digits = TRUE)
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
    line <-
      gvisLineChart(temp, xvar="year", yvar=c('Unemployed', 'Unemployment rate %'),
                    options=list(title=paste0(age_group, ' years old'),
                                 titleTextStyle="{color:'black',
                             fontName:'Ubuntu',
                             fontSize:18}",
                                 curveType="function",
                                 pointSize=15,
                                 series="[{targetAxisIndex:0,
                                       color:'#FFA500'},
                                     {targetAxisIndex:1,
                                      color:'#000080'}]",
                                 vAxes="[{title:'Population',
                             format:'##,###',
                             titleTextStyle: {color: '#FFA500'},
                             textStyle:{color: '#FFA500'},
                             textPosition: 'out'},
                            {title:'Percent',
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
    # double axis chart
    line_gen <-
      gvisLineChart(temp, xvar="year", yvar=c('Unemployed', 'Unemployment rate %'),
                    options=list(title=paste0(gen_group, "'s"),
                                 titleTextStyle="{color:'black',
                                 fontName:'Ubuntu',
                                 fontSize:18}",
                                 curveType="function",
                                 pointSize=15,
                                 series="[{targetAxisIndex:0,
                                 color:'#008080'},
                                 {targetAxisIndex:1,
                                 color:'#A9A9A9'}]",
                                 vAxes="[{title:'Population',
                                 format:'##,###',
                                 titleTextStyle: {color: '#008080'},
                                 textStyle:{color: '#008080'},
                                 textPosition: 'out'},
                                 {title:'Percent',
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
  
  
  
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

