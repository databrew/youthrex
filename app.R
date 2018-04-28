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
library(feather)
library(memisc)
library(shiny)
library(googleVis)
library(DT)
library(leaflet)
library(RColorBrewer)
options(gvis.plot.tag = 'chart')
options(scipen = 999)
library(ggplot2)
library(ggthemes)
library(shinymaterial)

source('global.R')


# overall help here: https://ericrayanderson.github.io/shinymaterial/
# color universe here: http://materializecss.com/color.html
# icon universe here :http://materializecss.com/icons.html

ui <- material_page(
  tags$style('.leaflet-container {
             background: #FFF;
             }'),
  title = "Youthrex data app",
  nav_bar_color = "blue",
  
  # define all the tabs on top of page (map, demo, family, edu, emp, housing, income)
  
  material_tabs(c('Map' = 'map',
                  'Demographics' = 'demo' ,
                  'Family' = 'family',
                  'Education' = 'edu',
                  'Employment' = 'emp',
                  'Housing' = 'housing',
                  'Income' = 'income'), color = 'grey'),
  
  br(),br(),
  
  material_row(
    material_column(width = 3,
                    material_card(
                      title = 'Choose year and location(s)',
                      depth = 3,
                      
                      # years
                      material_radio_button(
                        "years",
                        label = "",
                        choices = sort(unique(census$year), decreasing = T)
                        
                      ),
                      
                      material_button(input_id = 'location_button_none',
                                      label = 'Select none',
                                      icon = icon('close')),
                      material_button(input_id = 'location_button_all',
                                      label = 'Select all',
                                      icon = icon('people')),
                      # checkboxes for location
                      uiOutput('location_ui')
                      
                    )
                    
    ),
    material_column(width = 8,
                    
                    # MAP SECTION
                    material_tab_content('map',
                                         height = 10,
                                         material_card(title = "Ontario",
                                                       depth = 4,
                                                       fluidPage(
                                                         fluidRow(helpText('Click a region (or regions) in the map below to filter the census tract choices at left.')),
                                                         fluidRow(
                                                           leafletOutput(outputId = 'the_map')
                                                         )
                                                       ))),
                    # DEMOGRAPHIC SECTION
                    material_tab_content('demo',
                                         align = 'center',
                                         h4('Youth demographics'),
                                         material_tabs(c('Plot' = 'plotter', 'Table'= 'tabler' ), color = 'blue'),
                                         material_tab_content('plotter',
                                                              material_card(
                                                                plotlyOutput(outputId = 'demo_plot_pie')
                                                              )),
                                         material_tab_content('tabler',
                                                              material_card(
                                                                DT::dataTableOutput('pie_table')
                                                              )),
                                         
                                         
                                         material_row(
                                           material_column(width = 9,
                                                           material_card(title = "Demographic breakdown",
                                                                         depth = 4,
                                                                         material_switch('demo_show_table',
                                                                                         'View as table',
                                                                                         off_label = '',
                                                                                         on_label = ''),
                                                                         uiOutput(outputId = 'demo_var'))
                                                           
                                           ),
                                           
                                           
                                           
                                           
                                           material_column(width = 3,
                                                           material_card(
                                                             title = 'Examine by: ',
                                                             material_radio_button('demo_variable',
                                                                                   '',
                                                                                   choices=  c('Sex',
                                                                                               'Place of Birth',
                                                                                               'Visible minority',
                                                                                               'Aboriginal identity'), color = 'blue')
                                                           )
                                                           
                                                           
                                           )
                                           
                                         )    
                                         
                                         
                                         
                    ),
                    # FAMILY SECTION
                    material_tab_content('family',
                                         material_tabs(c('Youth as children (15-19)' = 'kids', 'Youth as parents (25-29)'= 'parents' ), color = 'blue'),
                                      
                                         material_tab_content('parents',
                                                              title = '',
                                                              material_column(width = 12,
                                                                              material_card(title = '',
                                                                                            depth = 3,
                                                                                            material_radio_button('fam_type',
                                                                                                                  '',
                                                                                                                  choices = c('Lone parents', 'Spouses and common law partners'),
                                                                                                                  color = '#28B463' ),
                                                                                            plotlyOutput('fam_plot_parents')))
                                                              ),  
                                         material_tab_content('kids',
                                                              material_column(width = 12,
                                                                              material_card(title = '',
                                                                                            depth = 3,
                                                                                            material_radio_button('fam_type_kids',
                                                                                                                  '',
                                                                                                                  choices = c('Children in lone parent families', 
                                                                                                                              'Children in couple families'),
                                                                                                                  color = '#28B463' ),
                                                                                            plotlyOutput('fam_plot_kids')))
                                                              
                                         ), 
                                       
                                         material_row(material_column(width = 6,
                                                                      br(),br(), 
                                                                      
                                                                      selectInput('which_fam_type',
                                                                                  'Parental type by visible minority',
                                                                                  choices = c('Spouses and common law partners', 'Lone parents'),
                                                                                  selected = 'Lone parents')),
                                                      material_column(width = 6,
                                                                      br(), br(), 
                                                                      material_checkbox('fam_chart_table',
                                                                                        'View as table',
                                                                                        initial_value = FALSE,
                                                                                        color = '#01579b'),
                                                                      material_checkbox('fam_chart_table_all_or_vm',
                                                                                        'Compare with non visible minority population',
                                                                                        initial_value = FALSE,
                                                                                        color = '#ff6f00'))
                                                      
                                         ),
                                         material_row(column(width = 12,
                                                             align = 'center',
                                                             material_card(title = 'Family structure by Visible minority status',
                                                                           depth = 4,
                                                                           uiOutput('fam_plot_table_vismin'))))
                                         
                                         
                                         
                    ),
                    # EDUCATION SECTION: textOuput: ed_hs_sex, ed_hs_pob, ed_hs_vm (20-24) and ed_college_sex, ed_college_pob, ed_college_vm (25)

                    material_tab_content('edu',
                                         material_tabs(c('High school' = 'hs',
                                                         'College' = 'college')),
                                         material_tab_content('hs',
                                                              material_card(
                                                                title = 'High school degree or equivalent', 
                                                                helpText('For ages 20-24'),
                                                                material_row(material_column(width = 6,
                                                                                             uiOutput('ed_hs_sex')),
                                                                             material_column(width = 6,
                                                                                             uiOutput('ed_hs_pob')),
                                                                             material_column(width = 4, 
                                                                                             material_switch('ed_hs_as_table',
                                                                                                             label = 'View as table',
                                                                                                             off_label = '',
                                                                                                             on_label = ''))
                                                                ),
                                                                material_row(
                                                                  material_column(width = 12,
                                                                                  uiOutput('ed_hs_vm'))
                                                                  
                                                                )
                                                              )
                                                              ),
                                         material_tab_content('college',
                                                              material_card(
                                                                title = "University certificate, diploma or degree" , 
                                                                helpText('For ages 25-29'),
                                                                material_row(material_column(width = 6,
                                                                                             uiOutput('ed_college_sex')),
                                                                             material_column(width = 6,
                                                                                             uiOutput('ed_college_pob')),
                                                                             material_column(width = 4, 
                                                                                             material_switch('ed_college_as_table',
                                                                                                             label = 'View as table',
                                                                                                             off_label = '',
                                                                                                             on_label = ''))
                                                                ),
                                                                material_row(
                                                                  material_column(width = 12,
                                                                                  uiOutput('ed_college_vm'))
                                                                  
                                                                )
                                                              )
                                         )
                    
                    )
            
    )
  )
  
  
  #   
  
  #   
  #   
  #   
  #   # EMPLOYMENT SECTION
  #   material_tab_content('emp'
  #                        
  #   ),
  #   
  #   # HOUSING SECTION
  #   material_tab_content('housing'
  #                        
  #   ), 
  #   
  #   # INCOME SECTION
  #   material_tab_content('income'
  #                        
  #   )
  #   
  #   
  # 
  )


# Define server 
server <- function(input, output) {
  
  # 
  # output$test_plot <- renderPlot({
  #   plot(x=c(1:20), y=c(11:30))
  # })
  # -----------------------------------------------------------------------------
  # Map
  
  # map of ontario
  output$the_map <- renderLeaflet({
    
    # 
    # the_map
    
    cl <- clicky()
    message('clicky is')
    print(cl)
    the_index <- which(region_map$region %in% cl)
    leaf_region(region_map = region_map,
                index = the_index)
    
  })
  
  # Location drop down
  
  clicky <- reactiveVal(value=NULL)
  location_choices <- reactiveVal(value = sort(unique(census$Geography[census$Geography != 'Ontario'])))
  location_selected <- reactiveVal(value = sort(unique(census$Geography[census$Geography != 'Ontario']))[1:2])
  
  # observe the shape click and update the left choices and map
  observeEvent(input$the_map_shape_click,{
    old_val <- clicky()
    val <- input$the_map_shape_click
    val <- val$id
    message('Map click detected at:')
    print(val)
    message('--- Old values were')
    print(old_val)
    message('--- New value is')
    print(val)
    
    if(!is.null(val)){
      if(!is.null(old_val)){
        if(val %in% old_val){
          # Just remove
          message('-----removing.')
          val <- old_val[old_val != val]
        } else {
          message('-----adding.')
          val <- c(old_val, val)
        }
      }
    }
    val <- val[!is.na(val)]
    
    message('New values are')
    print(val)
    clicky(val)
    
    
    # Update the reactive location_choices object
    if(length(val) == 0){
      lc <- sort(unique(census$Geography[census$Geography != 'Ontario']))
      lc <- sort(lc)
      location_choices(lc)
      location_selected(lc[1:2])
    } else {
      new_choices <- geo_dict %>%
        filter(region %in% val) %>%
        .$Geography
      new_choices <- sort(new_choices)
      location_choices(new_choices)
      location_selected(new_choices[1:2])
    }
    
    
    # Update the map
    sub_shp <- ont2[ont2@data$region %in% val,]
    if(length(val) > 0){
      leafletProxy('the_map') %>%
        # clearShapes() %>%
        addPolylines(data = sub_shp,
                     color = 'black',
                     weight = 0.3)
    }
    
    
  }
  )
  
  output$location_ui <- renderUI({
    the_choices <- location_choices()
    the_choices <- sort(the_choices)
    the_selected <- location_selected()

    out_list <- list()
    for(i in 1:length(the_choices)){
      out_list[[i]] <- paste0("material_checkbox(input_id = 'location_",the_choices[i], "',
                              label = '", the_choices[i] ,"',
                              initial_value = ",the_choices[i] %in% the_selected,")")
    }
    out <- paste0('fluidPage(', paste0(out_list, collapse =',\n'),
                  ')', collapse = '')
    
    eval(parse(text = out))
    
    # Checkbox group input does not work with material design
    # checkboxGroupInput("location",
    #             label = "",
    #             choices = the_choices,
    #             selected = the_selected %in% the_choices, 
    #             inline = FALSE
    # )
    })
  
  # Reactive object containing the selected location(s)
  locations <- reactiveVal(value = sort(geo_dict$region[geo_dict$region != 'Ontario']))
  
  observeEvent(input$location_button_all, {
    lc <- location_choices()
    location_selected(lc)
    locations(lc)
  })
  observeEvent(input$location_button_none, {
    location_selected(c())
    locations(c())
  })
  
  # Observe ANY change to any of the checkboxes, and update accordingly
  observe({
    lc <- location_choices()
    ip <- input
    ip <- reactiveValuesToList(ip)
    # Keep only those values preceded by location_
    ip <- unlist(ip)
    ip <- ip[grepl('location_', names(ip))]
    ip <- ip[!grepl('switch', names(ip))]
    names(ip) <- gsub('location_', '', names(ip))
    df <- data.frame(key = names(ip),
                     value = ip)
    df <- df %>% filter(value == TRUE,
                        key %in% lc)
    out <- df$key
    out <- sort(as.character(out))
    locations(out)
  })
  
  census_reactive <- reactive({
    l <- locations()
    census %>%
      filter(Geography %in% l)
  })
  
  # -----------------------------------------------------------------------------
  # -----------------------------------------------------------------------------
  # # demo variables
  
  # demo_plot_pie
  output$demo_plot_pie <- renderPlotly({
    location <- NULL
    
    years <- input$years
    location <- locations()
    
    if(length(location) > 0){
      print(location)
      plot_age_demo(location, years)
    } else {
      
      g <-  ggplot() + 
        theme_base() +
        labs(title = 'You must select a location plot')
      
      ggplotly(g)
    }
    
    
  })
  
  
  # demo_plot_pie
  output$pie_table <- renderDataTable({
    
    
    # same idea as above.
    location <- locations()
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", 'Population')
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(!grepl('Total',`Age group`)) %>%
      filter(grepl('Total',`Sex`)) %>% filter(grepl('Total',`Place of Birth`)) %>%
      filter(grepl('Total',`Visible minority`)) %>% filter(grepl('Total',`Aboriginal identity`))
    
    
    # keep only age group, year, and population
    temp <- temp[, c('year','Geography','Age group','Population')]
    colnames(temp) <- Hmisc::capitalize(colnames(temp))
    
    prettify(temp, comma_numbers = TRUE, download_options = TRUE)
    
    # datatable(temp, fillContainer = F, rownames = FALSE) %>%
    #   formatStyle(
    #     'Age group',
    #     target = 'row',
    #     backgroundColor = styleEqual(c('15 to 19 years', '20 to 24 years', '25 to 29 years'),
    #                                  c('white', 'white', 'white')))
    # 
    
  })
  # 
  # 'Sex', 'Place of Birth', 'Visible minority',
  # 'Aboriginal identity'
  output$demo_chart <- renderPlotly({
    
    
    location <- locations()
    years <- input$years
    
    
    if(length(location) > 0){
      # variable to examine
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
        temp$`Age group` <- temp$geo_code <- 
          temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
        
      }
      
      if(demo_variable == 'Place of Birth') {
        
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$geo_code <- 
          temp$`Aboriginal identity` <- temp$`Sex` <- temp$`Visible minority` <-   NULL
        
        
      }
      
      if(demo_variable == 'Visible minority') {
        # get data
        temp <- new_census %>%
          filter(!grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        
        temp$`Age group` <- temp$geo_code <- 
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
        
        temp$`Age group` <- temp$geo_code <- 
          temp$`Place of Birth` <- temp$Sex <- temp$`Visible minority` <-   NULL
        
      }
      
      
      plotly_plot <- plotly_demo(temp_dat = temp, hole_value = 0.4, geo_names = unique(temp$Geography), var_lab = 'Sex', by_geo = FALSE)
      
      return(plotly_plot)
    } else {
      g <-  ggplot() + 
        theme_base() +
        labs(title = 'You must select a location plot')
      
      
      return(ggplotly(g))
    }
    
    
  })
  # 
  # 
  
  # 'Aboriginal identity'
  output$demo_table <- renderDataTable({
    
    location <- locations()
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
      temp$`Age group`  <- temp$geo_code <- 
        temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
      
      temp <- demo_table(temp,geo_names = unique(temp$Geography))
      colnames(temp)[colnames(temp) == 'V2'] <- 'Sex'
      
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
      temp$`Age group`  <- temp$geo_code <- 
        temp$`Aboriginal identity` <- temp$`Sex` <- temp$`Visible minority` <-   NULL
      
      temp <- demo_table(temp,geo_names = unique(temp$Geography))
      colnames(temp)[colnames(temp) == 'V2'] <- 'Place of birth'
      
      return(prettify(temp, download_options = TRUE))
      
    }
    
    if(demo_variable == 'Visible minority') {
      # get data
      temp <- new_census %>%
        filter(!grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      
      temp$`Age group` <- temp$geo_code <- 
        temp$`Place of Birth` <- temp$Sex <- temp$`Aboriginal identity` <-   NULL
      
      # remove Arab/West Asian
      temp <- temp[temp$`Visible minority` != 'Arab/West Asian',]
      temp <- temp[temp$`Visible minority` != 'All visible minorities',]
      
      temp <- demo_table(temp,geo_names = unique(temp$Geography))
      colnames(temp)[colnames(temp) == 'V2'] <- 'Visible minority'
      
      return(prettify(temp, download_options = TRUE))        
    }
    
    if(demo_variable == 'Aboriginal identity') {
      # get data
      temp <- new_census %>%
        filter(!grepl('Total', `Aboriginal identity`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Visible minority`))
      
      temp$`Age group`  <- temp$geo_code <- 
        temp$`Place of Birth` <- temp$Sex <- temp$`Visible minority` <-   NULL
      
      temp <- demo_table(temp,geo_names = unique(temp$Geography))
      colnames(temp)[colnames(temp) == 'V2'] <- 'Aboriginal identity'
      return(prettify(temp, download_options = TRUE))        
    }
    
    
    
  })
  
  
  output$demo_var <- renderUI({
    make_table <- input$demo_show_table
    if(make_table) {
      dataTableOutput('demo_table')
    } else {
      plotlyOutput('demo_chart')
    }
  })
  
  # 
  # 
  # ---------------------------------------------------------------------------------------------
  # family variables = 'fam_plot_parents, fam_plot_kids, fam_plot_vismin, fam_table_vismin
  
  output$fam_plot_parents <- renderPlotly({
  
    location <- locations()
    years <- c('2001', '2006', '2011', '2016')
    
    # years <- input$years
    fam_type <- input$fam_type
    
    if(length(location) > 0){
      
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", fam_type,'Population')
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% filter(grepl("25 to 29 years",`Age group`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      temp$`Age group` <- temp$geo_code <- temp$Sex <-
        temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
      
      
      # # get percentages
      temp <- as.data.frame(temp)
      colnames(temp)[3] <- paste0('Total ', fam_type)
      
      temp$Percent <- round(temp[,3]/temp$Population, 2)
      
      temp$year <- as.factor(temp$year)
      # plot all vm
      cols <- colorRampPalette(brewer.pal(9, 'Set1'))(length(unique(temp$Geography)))
      g <- ggplot(data = temp,
                  aes(x = year,
                      y = Percent,
                      group = Geography,
                      colour = Geography,
                      text = paste0('Location: ', Geography,
                                    '<br>', (Percent)*100 , '% ', fam_type))) + ggtitle(fam_type) +
        geom_point(size = 2, alpha = 0.6) + geom_line(size = 1, alpha = 0.8, linetype = 'dotdash') + scale_color_manual(name = '',
                                                                                                                        values = cols) + theme_bw(base_size = 16, base_family = 'Ubuntu')  +
        labs(x = '', y = ' ') + 
        scale_y_continuous(labels = scales::percent)
      g <- g  + theme_bw(base_size = 14, base_family = 'Ubuntu')
      
      plotly_plot <- plotly::ggplotly(g, tooltip = 'text') %>% config(displayModeBar = F) %>%
        layout(showlegend = F)
    } else {
      plotly_plot <-  ggplot() + 
        theme_base() +
        labs(title = 'You must select a location plot')
    }
      
    
     return(plotly_plot)
     

    })
  
  
  output$fam_plot_kids <- renderPlotly({
    
      location <- locations()
      years <- input$years
      fam_type_kids <- input$fam_type_kids
      
      
      if(length(location) > 0){
        demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", 
                       fam_type_kids,'Population')
        new_census <- census[ , demo_vars]
        
        if(length(location) == 1) {
          location <- c('Ontario', location)
          
          f <- list(
            family = "Ubuntu",
            size = 20,
            color = "white"
          )
        }
        
        temp <- new_census %>% filter(Geography %in% location) %>%
          filter(year %in% years) %>% filter(grepl("15 to 19 years",`Age group`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group` <- temp$geo_code <- temp$Sex <-
          temp$`Aboriginal identity` <- temp$`Place of Birth` <- temp$`Visible minority` <-   NULL
        
      
         
         if(length(unique(temp$Geography)) == 2){

           colnames(temp)[3] <- 'V3'
           temp$Percent <- as.numeric(round(temp$V3/temp$Population, 2))
           temp$Geography <- gsub('Ontario', 'Average in Ontario', temp$Geography)
           # plot data
           g <- ggplot(data = temp,
                       aes(x = as.factor(Geography),
                           y = Percent,
                           text = paste('Total population for age group: ', Population,
                                        '<br>', (Percent)*100 , '%', as.factor(Geography)))) + 
             scale_y_continuous(labels = scales::percent) +
             geom_bar(position = 'dodge', stat = 'identity', colour = 'darkgrey', fill = 'rgba(50, 171, 96, 0.6)', alpha = 0.8) + 
             theme_bw(base_size = 14, base_family = 'Ubuntu') + labs(x = '', y = ' ', title = fam_type_kids)
           
           
           g <-  plotly::ggplotly(g, tooltip = 'text') %>%
             config(displayModeBar = F) %>%
             layout( 
               legend = list(
                 orientation = "l",
                 x = 0,
                 y = -0.3))
         } else {
           colnames(temp)[3] <- 'V3'
           temp$Percent <- as.numeric(round((temp$V3/temp$Population)*100, 2))
           g  <- plot_ly(temp, x = ~Percent, y = ~reorder(Geography, Percent), 
                         type = 'bar', orientation = 'h',
                         marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                       line = list(color = 'black', width = 1))) %>%
             layout(title = fam_type_kids,
                    yaxis = list(title = '', showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                    xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
             add_annotations(xref = 'x1', yref = 'y',
                             x = temp$Percent + 2.5,  y = temp$Geography,
                             text = paste(round(temp$Percent, 2), '%'),
                             font = list(family = 'Ubuntu', size = 12, color = 'black'),
                             showarrow = FALSE)
         }
        
      } else {
        g <-  ggplot() + 
          theme_base() +
          labs(title = 'You must select a location plot')
      }

     return(g)
  
  })
  
  

  
  output$fam_plot_vismin <- renderPlotly({
    
      # # subset data by inputs
      location <- c('Toronto', 'Ottawa')
      years <- c(2016)
      which_fam_type <- 'Lone parents'
      fam_chart_table_all_or_vm <- FALSE
      which_fam_type <- input$which_fam_type
      location <- locations()
      years <- input$years
      fam_chart_table_all_or_vm <- input$fam_chart_table_all_or_vm
      
      if(length(location) > 0){
        demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", 
                       which_fam_type,'Population')
        new_census <- census[ , demo_vars]
        
        temp <- new_census %>% filter(Geography %in% location) %>%
          filter(year %in% years) %>% filter(grepl("25 to 29 years",`Age group`)) %>%
          filter(grepl('Total', `Sex`)) %>%
          filter(grepl('Total', `Place of Birth`)) %>%
          filter(!grepl('Total', `Visible minority`)) %>%
          filter(grepl('Total', `Aboriginal identity`))
        temp$`Age group`  <- temp$geo_code <- temp$Sex <-
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
        colnames(temp_all)[4] <- 'V2'
        colnames(temp_vm)[4] <- 'V2'
        
        temp_all$per <- round(temp_all$V2/temp_all$Population,2 )
        temp_vm$per <- round(temp_vm$V2/temp_vm$Population,2 )
        
        
        if(fam_chart_table_all_or_vm){
          # plot all vm
          print(temp_all)
          cols <- c('darkgreen', "#9999CC")
          g <- ggplot(data = temp_all,
                      aes(x = Geography,
                          y = per,
                          fill = `Visible minority`,
                          text = paste('Total population of', `Visible minority`, ': ', Population,
                                       '<br>', (per)*100 , '% ', which_fam_type))) +
           
            geom_jitter(aes(size = Population), width = 0.2, height = 0, colour ='black') +
            scale_fill_manual(name = '', 
                               values = cols) + 
            scale_size_continuous(name = '') +
            labs(x = '', y = ' ') +  
            scale_y_continuous(labels = scales::percent)
          g <- g  + theme_bw(base_size = 14, base_family = 'Ubuntu') 
          
          p1 <- plotly::ggplotly(g, tooltip = 'text') %>% config(displayModeBar = F) 
        } else {
          
          # plot data
          cols <- colorRampPalette(brewer.pal(9, 'Set1'))(length(unique(temp_vm$`Visible minority`)))
          g <- ggplot(data = temp_vm,
                      aes(x = Geography,
                          y = per,
                          fill = `Visible minority`,
                          text = paste('Total population of', `Visible minority`, ': ', Population,
                                       '<br>', (per)*100 , '% ', which_fam_type ))) + 
            geom_jitter(aes(size = Population), width = 0.2, height = 0, colour = 'black') +
            scale_fill_manual(name = '', 
                              values = cols) + 
            scale_size_continuous(name = '') + theme_bw(base_size = 14, base_family = 'Ubuntu')  +
            scale_y_continuous(labels = scales::percent) +
            labs(x = '', y = ' ') 
          
          p1 <- plotly::ggplotly(g, tooltip = 'text')  %>% config(displayModeBar = F)
        }
        
      } else {
       p1 <-  ggplot() + 
          theme_base() +
          labs(title = 'You must select a location plot')
      }
        
    
      return(p1)
   
  })
  
  
  output$fam_table_vismin <- DT::renderDataTable({
    
      # # subset data by inputs
      # location <- 'Ontario'
      # years <- c(2001, 2006, 2011, 2016)
      # which_fam_type <- 'Lone parents'
      # fam_chart_table_all_or_vm <- FALSE
      which_fam_type <- input$which_fam_type
      location <- locations()
      years <- input$years
      fam_chart_table_all_or_vm <- input$fam_chart_table_all_or_vm
      
      if(length(location) > 0){
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
        temp$`Age group` <- temp$geo_code <- temp$Sex <-
          temp$`Aboriginal identity` <- temp$`Place of Birth`  <-   NULL
        
        # make an "Other" column that makes up the rest between the addtion of our two variables and total population for 
        # that age group.
        
        # remove Arab/West Asian from data
        temp <- temp %>% filter(!`Visible minority` %in% 'Arab/West Asian')
        
        # get two datasets: all other vs all vm, and only within vm.
        temp_all <- temp %>% filter(grepl('All', temp$`Visible minority`))
        temp_vm <- temp %>% filter(!grepl('All', temp$`Visible minority`))
        
        # get percentage 
        colnames(temp_all)[4] <- 'V2'
        colnames(temp_vm)[4] <- 'V2'
        
        temp_all$Percent <- round((temp_all$V2/temp_all$Population)*100, 2)
        temp_vm$Percent <- round((temp_vm$V2/temp_vm$Population)*100, 2)
        
        # get percentage 
        colnames(temp_all)[4] <- which_fam_type
        colnames(temp_vm)[4] <- which_fam_type
        
        if(fam_chart_table_all_or_vm){
          return(prettify(temp_all))
        } else {
          return(prettify(temp_vm))
        }
        
      } else {
        NULL
      }
        
      
     
    
    
  })
  
  output$fam_plot_table_vismin <- renderUI({
    if(input$fam_chart_table){
      dataTableOutput('fam_table_vismin')
    } else {
      plotlyOutput('fam_plot_vismin')
    }
    
  })
  
  
  
  #----------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------
  # education 

  ###########################################
  # HIGH SCHOOL
  # EDUCATION SECTION: textOuput: ed_hs_sex, ed_hs_pob, ed_hs_vm (20-24) and ed_college_sex, ed_college_pob, ed_college_vm (25-29)
  output$ed_hs_sex <- renderUI({
    if(!input$ed_hs_as_table){
      plotlyOutput('ed_hs_sex_plot')
      
    } else {
      DT::dataTableOutput('ed_hs_sex_table')
    }
    
  })
  
  
  # # ed_1 or ed_text_highschool
  output$ed_hs_sex_plot <- renderPlotly({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
    
    if(length(location) > 0){
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                     "Place of Birth","Visible minority", "Aboriginal identity", 
                     "High school or equivalent",'Population')
      
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% 
        filter(grepl('20 to 24', `Age group`)) %>%
        filter(!grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      
      temp$`Age group` <- temp$geo_code  <-
        temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
        temp$`Visible minority` <- NULL
      temp$per <- round(temp$`High school or equivalent`/temp$Population, 2)
      
      names(temp)[3] <- 'V2'
      
      color_palette <- colorRampPalette(brewer.pal(9, 'Set2'))(length(unique(temp$V2)))
      
      g_plot <- ed_candle_plot(temp_dat = temp,
                               ed_var = "High school or equivalent" ,
                               title = "",
                               color_palette = color_palette,
                               location = location,
                               years = years)
      
    } else {
      g_plot <-  ggplot() + 
        theme_base() +
        labs(title = 'You must select a location plot')
    }
    return(g_plot)
    
  })
  
  
  # # ed_1 or ed_text_highschool
  output$ed_hs_sex_table <- renderDataTable({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
  
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "High school or equivalent",'Population')
    
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% 
      filter(grepl('20 to 24', `Age group`)) %>%
      filter(!grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    
    temp$`Age group` <- temp$geo_code  <-
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    temp$Percent <- round(temp$`High school or equivalent`/temp$Population, 2)
    
   return(prettify(temp, comma_numbers = TRUE, download_options = TRUE))
  })
  
  
  #########################3
  output$ed_hs_pob <- renderUI({
    if(!input$ed_hs_as_table){
      plotlyOutput('ed_hs_pob_plot')
      
    } else {
      DT::dataTableOutput('ed_hs_pob_table')
    }
    
  })
  
  
  # # ed_1 or ed_text_highschool
  output$ed_hs_pob_plot <- renderPlotly({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
    
    if(length(location) > 0){
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                     "Place of Birth","Visible minority", "Aboriginal identity", 
                     "High school or equivalent",'Population')
      
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% 
        filter(grepl('20 to 24', `Age group`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(!grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      
      temp$`Age group` <- temp$geo_code  <-
        temp$`Aboriginal identity` <- temp$Sex  <-
        temp$`Visible minority` <- NULL
      temp$per <- round(temp$`High school or equivalent`/temp$Population, 2)
      
     names(temp)[3] <- 'V2'
      
     color_palette <- colorRampPalette(brewer.pal(9, 'Set1'))(length(unique(temp$V2)))
     
      g_plot <- ed_candle_plot(temp_dat = temp,
                               ed_var = "High school or equivalent" ,
                               title = "",
                               color_palette = color_palette,
                               location = location,
                               years = years)
      
    } else {
      g_plot <-  ggplot() + 
        theme_base() +
        labs(title = 'You must select a location plot')
    }
    return(g_plot)
    

  })
  
  
  
  
  # # ed_1 or ed_text_highschool
  output$ed_hs_pob_table <- renderDataTable({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
  
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "High school or equivalent",'Population')
    
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% 
      filter(grepl('20 to 24', `Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(!grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    
    temp$`Age group` <- temp$geo_code  <-
      temp$`Aboriginal identity` <- temp$Sex  <-
      temp$`Visible minority` <- NULL
    temp$Percent <- round(temp$`High school or equivalent`/temp$Population, 2)

  return(prettify(temp, comma_numbers = TRUE, download_options = TRUE))
  
    
  })
  
  
  #########################3
  output$ed_hs_vm <- renderUI({
    if(!input$ed_hs_as_table){
      plotlyOutput('ed_hs_vm_plot')
      
    } else {
      DT::dataTableOutput('ed_hs_vm_table')
    }
    
  })
  
  # # ed_1 or ed_text_highschool
  output$ed_hs_vm_plot <- renderPlotly({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
    
    if(length(location) > 0){
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                     "Place of Birth","Visible minority", "Aboriginal identity", 
                     "High school or equivalent",'Population')
      
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% 
        filter(grepl('20 to 24', `Age group`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(!grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      
      temp$`Age group` <- temp$geo_code  <-
        temp$`Aboriginal identity` <- temp$Sex  <-
        temp$`Place of Birth` <- NULL
      temp$per <- round(temp$`High school or equivalent`/temp$Population, 2)
      
      names(temp)[3] <- 'V2'
      
      color_palette <- colorRampPalette(brewer.pal(9, 'Dark2'))(length(unique(temp$V2)))
      
      g_plot <- ed_candle_plot(temp_dat = temp,
                               ed_var = "High school or equivalent" ,
                               title = "",
                               color_palette = color_palette,
                               location = location,
                               years = years)
      
    } else {
      g_plot <-  ggplot() + 
        theme_base() +
        labs(title = 'You must select a location plot')
    }
    return(g_plot)
    
    
  })
  

  # # ed_1 or ed_text_highschool
  output$ed_hs_vm_table <- renderDataTable({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
  
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "High school or equivalent",'Population')
    
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% 
      filter(grepl('20 to 24', `Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(!grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    
    temp$`Age group` <- temp$geo_code  <-
      temp$`Aboriginal identity` <- temp$Sex  <-
      temp$`Place of Birth` <- NULL
    temp$Percent <- round(temp$`High school or equivalent`/temp$Population, 2)
   
  return(prettify(temp, comma_numbers = TRUE, download_options = TRUE))
  
    
  })
  
  
  
  ###########################################
  # COLLLEGE
  # EDUCATION SECTION: textOuput: ed_hs_sex, ed_hs_pob, ed_hs_vm (20-24) and ed_college_sex, ed_college_pob, ed_college_vm (25-29)
  output$ed_college_sex <- renderUI({
    if(!input$ed_college_as_table){
      plotlyOutput('ed_college_sex_plot')
      
    } else {
      DT::dataTableOutput('ed_college_sex_table')
    }
    
  })
  
  
  # # ed_1 or ed_text_highschool
  output$ed_college_sex_plot <- renderPlotly({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
    
    if(length(location) > 0){
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                     "Place of Birth","Visible minority", "Aboriginal identity", 
                     "University certificate, diploma or degree" ,'Population')
      
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% 
        filter(grepl('25 to 29', `Age group`)) %>%
        filter(!grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      
      temp$`Age group` <- temp$geo_code  <-
        temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
        temp$`Visible minority` <- NULL
      temp$per <- round(temp$`University certificate, diploma or degree`/temp$Population, 2)
      
      names(temp)[3] <- 'V2'
      
      color_palette <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$V2)))
      
      g_plot <- ed_candle_plot(temp_dat = temp,
                               ed_var = "University certificate, diploma or degree"  ,
                               title = "",
                               color_palette = color_palette,
                               location = location,
                               years = years)
      
    } else {
      g_plot <-  ggplot() + 
        theme_base() +
        labs(title = 'You must select a location plot')
    }
    return(g_plot)
    
  })
  
  
  # # ed_1 or ed_text_highschool
  output$ed_college_sex_table <- renderDataTable({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "University certificate, diploma or degree" ,'Population')
    
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% 
      filter(grepl('25 to 29', `Age group`)) %>%
      filter(!grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    
    temp$`Age group` <- temp$geo_code  <-
      temp$`Aboriginal identity` <- temp$`Place of Birth`  <-
      temp$`Visible minority` <- NULL
    temp$Percent <- round(temp$`University certificate, diploma or degree` /temp$Population, 2)
    
    return(prettify(temp, comma_numbers = TRUE, download_options = TRUE))
  })
  
  
  #########################3
  output$ed_college_pob <- renderUI({
    if(!input$ed_college_as_table){
      plotlyOutput('ed_college_pob_plot')
      
    } else {
      DT::dataTableOutput('ed_college_pob_table')
    }
    
  })
  
  
  # # ed_1 or ed_text_highschool
  output$ed_college_pob_plot <- renderPlotly({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
    
    if(length(location) > 0){
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                     "Place of Birth","Visible minority", "Aboriginal identity", 
                     "University certificate, diploma or degree" ,'Population')
      
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% 
        filter(grepl('25 to 29', `Age group`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(!grepl('Total', `Place of Birth`)) %>%
        filter(grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      
      temp$`Age group` <- temp$geo_code  <-
        temp$`Aboriginal identity` <- temp$Sex  <-
        temp$`Visible minority` <- NULL
      temp$per <- round(temp$`University certificate, diploma or degree`/temp$Population, 2)
      
      names(temp)[3] <- 'V2'
      
      color_palette <- colorRampPalette(brewer.pal(9, 'Paired'))(length(unique(temp$V2)))
      
      g_plot <- ed_candle_plot(temp_dat = temp,
                               ed_var = "University certificate, diploma or degree"  ,
                               title = "",
                               color_palette = color_palette,
                               location = location,
                               years = years)
      
    } else {
      g_plot <-  ggplot() + 
        theme_base() +
        labs(title = 'You must select a location plot')
    }
    return(g_plot)
    
    
  })
  
  
  
  
  # # ed_1 or ed_text_highschool
  output$ed_college_pob_table <- renderDataTable({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "University certificate, diploma or degree" ,'Population')
    
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% 
      filter(grepl('25 to 29', `Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(!grepl('Total', `Place of Birth`)) %>%
      filter(grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    
    temp$`Age group` <- temp$geo_code  <-
      temp$`Aboriginal identity` <- temp$Sex  <-
      temp$`Visible minority` <- NULL
    temp$Percent <- round(temp$`University certificate, diploma or degree`/temp$Population, 2)
    
    return(prettify(temp, comma_numbers = TRUE, download_options = TRUE))
    
    
  })
  
  
  #########################3
  output$ed_college_vm <- renderUI({
    if(!input$ed_college_as_table){
      plotlyOutput('ed_college_vm_plot')
      
    } else {
      DT::dataTableOutput('ed_college_vm_table')
    }
    
  })
  
  # # ed_1 or ed_text_highschool
  output$ed_college_vm_plot <- renderPlotly({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
    
    if(length(location) > 0){
      demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                     "Place of Birth","Visible minority", "Aboriginal identity", 
                     "University certificate, diploma or degree" ,'Population')
      
      new_census <- census[ , demo_vars]
      
      temp <- new_census %>% filter(Geography %in% location) %>%
        filter(year %in% years) %>% 
        filter(grepl('25 to 29', `Age group`)) %>%
        filter(grepl('Total', `Sex`)) %>%
        filter(grepl('Total', `Place of Birth`)) %>%
        filter(!grepl('Total', `Visible minority`)) %>%
        filter(grepl('Total', `Aboriginal identity`))
      
      temp$`Age group` <- temp$geo_code  <-
        temp$`Aboriginal identity` <- temp$Sex  <-
        temp$`Place of Birth` <- NULL
      temp$per <- round(temp$`University certificate, diploma or degree`/temp$Population, 2)
      
      names(temp)[3] <- 'V2'
      
      color_palette <- colorRampPalette(brewer.pal(9, 'Set1'))(length(unique(temp$V2)))
      
      g_plot <- ed_candle_plot(temp_dat = temp,
                               ed_var = "University certificate, diploma or degree"  ,
                               title = "",
                               color_palette = color_palette,
                               location = location,
                               years = years)
      
    } else {
      g_plot <-  ggplot() + 
        theme_base() +
        labs(title = 'You must select a location plot')
    }
    return(g_plot)
    
    
  })
  
  
  # # ed_1 or ed_text_highschool
  output$ed_college_vm_table <- renderDataTable({
    # subset data by inputs
    location <- c('Toronto', 'Ottawa', 'Hamilton')
    years <- c(2016)
    location <- locations()
    years <- input$years
    print(location)
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", 
                   "Place of Birth","Visible minority", "Aboriginal identity", 
                   "University certificate, diploma or degree",'Population')
    
    new_census <- census[ , demo_vars]
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% 
      filter(grepl('25 to 29', `Age group`)) %>%
      filter(grepl('Total', `Sex`)) %>%
      filter(grepl('Total', `Place of Birth`)) %>%
      filter(!grepl('Total', `Visible minority`)) %>%
      filter(grepl('Total', `Aboriginal identity`))
    
    temp$`Age group` <- temp$geo_code  <-
      temp$`Aboriginal identity` <- temp$Sex  <-
      temp$`Place of Birth` <- NULL
    temp$Percent <- round(temp$`"University certificate, diploma or degree`/temp$Population, 2)
    
    return(prettify(temp, comma_numbers = TRUE, download_options = TRUE))
    
    
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
    years <- 2016
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
    
    temp$`Average household income before tax $` <- round(temp$`Average household income before tax $`)
    cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$Geography)))
    
    # set condition for if only 1 year is chosent 
    if(length(unique(temp$year)) == 1) {
      temp$year <- as.factor(as.character(temp$year))
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
      geom_smooth(alpha = 0.4, size = 1) + theme_bw(base_size = 13, base_family = 'Ubuntu') +
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
      temp$`Place of Birth` <- NULL
    temp$`Average household income before tax $` <- round(temp$`Average household income before tax $`)
    
    # subet by income_vm_filter to get at least one line 
    temp <- temp %>% filter(`Visible minority` %in% income_vm_filter)
    
    temp$`Average household income before tax $` <- round(temp$`Average household income before tax $`)
    cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$`Visible minority`)))
    
    # set condition for if only 1 year is chosent 
    if(length(unique(temp$year)) == 1) {
      temp$year <- as.factor(as.character(temp$year))
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
         server = server)

