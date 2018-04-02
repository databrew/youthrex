library(shiny)
library(googleVis)
library(DT)
library(leaflet)
library(RColorBrewer)
library(networkD3)
options(gvis.plot.tag = 'chart')
library(shinyBS)
library(shinyLP)
library(ggplot2)
library(shinythemes)
library(plotly)


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
                            width = 12,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            
                            fluidRow(column(6,
                                            uiOutput("location_header"),
                                            htmlOutput('demo_plot_pie')),
                                     column(6,
                                            selectInput('demo_variable',
                                                        'Exampine by ',
                                                        choices =  c('Sex', 'Place of Birth', 'Visible minority',
                                                                     'Aboriginal identity'),
                                                        selected = 'Sex',
                                                        multiple = FALSE))),
                            fluidRow(
                              column(6,
                                     DT::dataTableOutput('pie_table')),
                              column(6,
                                     htmlOutput('demo_charts'))
                            )

                          )
                        ),
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Family status',
                            status = 'success',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            
                            column(6,
                                   plotOutput('fam_plot')),
                            column(6,
                                   DT::dataTableOutput('fam_table'))
                            
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
                            
                            column(6,
                                   plotOutput('ed_plot')),
                            column(6,
                                   DT::dataTableOutput('ed_table'))
                            
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
                            
                            column(6,
                                   plotOutput('emp_plot')),
                            column(6,
                                   DT::dataTableOutput('emp_table'))
                            
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
                    
)


# Define server 
server <- function(input, output) {
  
 

  output$pie_text <- renderText({

      if(input$location == 'Ontario') {
        paste0('All youth (defined 15-29) in Ontario for ',input$years )
      }

      years <- input$years
      location <- input$location
      if(length(years == 1)) {
        paste0('The youth (defined 15-29) in ', location, ' were a ', 'percent', '% of Onatrio')
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
      
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", 'Population')
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
        chart_title <- paste0('Avg population for ',year_value)
        gvisPieChart(temp,
                     options=list(title=year_value,
                                  legend='none'))
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
        backgroundColor = styleEqual(c('15 to 19 years', '20 to 24 years', '25 to 29 years'), c('white', 'white', 'white')))
      
     
      
    }
    
    
  })
  
  # 'Sex', 'Place of Birth', 'Visible minority',
  # 'Aboriginal identity'
  output$demo_charts <- renderGvis({

    if (is.null(input$years)) {
      return(NULL)
    } else {
      # subset data by inputs
      location <- 'Ontario'
      years <- c(2001, 2006, 2011, 2016)
      demo_variable <- 'Sex'
      location <- input$location
      years <- input$years
      demo_variable <- input$demo_variable
      
    
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
        }
      
        if(demo_variable == 'Place of Birth') {
          # get data
          temp <- new_census %>% 
            filter(!grepl('Total', `Place of Birth`)) %>% 
            filter(grepl('Total', `Sex`)) %>% 
            filter(grepl('Total', `Visible minority`)) %>%
            filter(grepl('Total', `Aboriginal identity`)) 
        }
      
        if(demo_variable == 'Visible minority') {
          # get data
          temp <- new_census %>% 
            filter(!grepl('Total', demo_variable)) %>% 
            filter(grepl('Total', `Place of Birth`)) %>% 
            filter(grepl('Total', `Sex`)) %>%
            filter(grepl('Total', `Aboriginal identity`)) 
        }
        
        if(demo_variable == 'Aboriginal identity') {
          # get data
          temp <- new_census %>% 
            filter(!grepl('Total', demo_variable)) %>% 
            filter(grepl('Total', `Place of Birth`)) %>% 
            filter(grepl('Total', `Sex`)) %>%
            filter(grepl('Total', `Visible minority`)) 
        }
      

      
      
      # remove age group
      temp$`Age group` <- temp$Geography <- temp$geo_code <- NULL
      
      colnames(temp)[2] <- 'V2'
  
      # group by year and demo_variable 
      temp <- temp %>% group_by(year, V2) %>%
        summarise(mean_pop )
      
    }

  })

  
  
  
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

