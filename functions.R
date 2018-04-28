library(shinydashboard)
library(sp)
library(raster)
library(maptools)
library(googledrive)
library(yaml)
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(feather)
library(memisc)
library(stringdist)
library(reshape2)

leaf <- function(x, 
                 tile = 'OpenStreetMap', 
                 palette = 'Oranges',
                 show_legend = TRUE,
                 years,
                 title = NULL){
  
  # This function expects "x" to be a dataframe with a column named "geo_code" (a 4 character string)
  # and another named "value"
  # Keep only the numbered values
  right <- x %>%
    filter(!is.na(as.numeric(geo_code))) %>%
    mutate(geography = substr(geo_code, 3,4))
  # join to ont shapefile
  shp <- ont2
  shp@data <- shp@data %>%
    mutate(geography = CCA_2) %>%
    left_join(right,
              by = 'geography')
  
  # remove body of water on map (4 lakes, all currentl NA)
  
  
  # Create a color palette
  # pal <- colorQuantile("Blues", NULL, n = 9)
  # bins <- round(c(quantile(shp@data$value, na.rm = TRUE), Inf))
  # insure that the full range of numbers is captured by aplying floor and then adding one to the max.
  # bins <- unique(floor(c(quantile(shp@data$per_youth, 
  #                                 na.rm = TRUE, c(seq(0, 1, 0.15), 1)))), 2)
  # 
  # bins[length(bins)] <- bins[length(bins)] +1
  
  
  pal <- colorBin(palette, domain = shp@data$per_youth, 
                  # bins = bins,
                  bins = 7,
                  na.color = 'grey')
  
  
  # Create a popup
  popper <- paste0(shp@data$NAME_2, ': ',
                   round(shp@data$per_youth, 2), '% Youth in ', years)
  
  
  
  # Create map
  l <- leaf_basic(shp = shp, tile, palette = palette)
  # l <- leaflet(data = shp) %>%
  #   addProviderTiles(tile)
  if(show_legend){
    l <- l %>%
      addLegend(pal = pal, 
                values =~per_youth, 
                opacity = 0.7, 
                position = "bottomleft",
                title = '% youth (15-29)')
  }
  l <- l %>%
    addPolygons(data = shp, 
                fillColor = ~pal(per_youth),
                fillOpacity = 0.8,
                color = "white",
                weight = 1,
                # popup = popper,
                # highlight = highlightOptions(
                #   weight = 5,
                #   color = "white",
                #   dashArray = "",
                #   fillOpacity = 0.7,
                #   bringToFront = TRUE),
                label = popper,
                labelOptions = labelOptions(#noHide = T, 
                  direction = "auto",
                  style = list(
                    "color" = "#191A1C",
                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "12px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  ))) 
  # %>% 
  #   setView(lng = -84.3232, lat = 53.25, zoom = 6)
  return(l)
}
# Define function for generating a leaflet plot

# Create a basic leaflet with nothing else on it
leaf_basic <- function(shp = ont2, tile, palette){
  leaflet(data = shp) %>%
    addProviderTiles(provider = tile)
}


# Define function for printing nice html tables
prettify <- function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE,
                      cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y",
                      round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE,
                      data_table = TRUE, nrows = 5, download_options = FALSE, no_scroll = TRUE){
  column_names <- names(the_table)
  the_table <- data.frame(the_table)
  names(the_table) <- column_names
  classes <- lapply(the_table, function(x) {
    unlist(class(x))[1]
  })
  if (cap_columns) {
    names(the_table) <- Hmisc::capitalize(names(the_table))
  }
  if (remove_underscores_columns) {
    names(the_table) <- gsub("_", " ", names(the_table))
  }
  for (j in 1:ncol(the_table)) {
    the_column <- the_table[, j]
    the_class <- classes[j][1]
    if (the_class %in% c("character", "factor")) {
      if (cap_characters) {
        the_column <- as.character(the_column)
        the_column <- Hmisc::capitalize(the_column)
      }
      if (remove_line_breaks) {
        the_column <- gsub("\n", " ", the_column)
      }
    }
    else if (the_class %in% c("POSIXct", "Date")) {
      the_column <- format(the_column, format = date_format)
    }
    else if (the_class %in% c("numeric", "integer")) {
      the_column <- round(the_column, digits = round_digits)
      if (comma_numbers) {
        if(!grepl('year', tolower(names(the_table)[j]))){
          the_column <- scales::comma(the_column)
        }
      }
    }
    the_table[, j] <- the_column
  }
  if (remove_row_names) {
    row.names(the_table) <- NULL
  }
  if (data_table) {
    if (download_options) {
      if(no_scroll){
        the_table <- DT::datatable(the_table, fillContainer = F, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          dom = "Bfrtip", buttons = list("copy", 
                                         list(extend = "collection", sDom  = '<"top">lrt<"bottom">ip',buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      } else {
        the_table <- DT::datatable(the_table, fillContainer = F,options = list(pageLength = nrows,
                                                                               # scrollY = '300px', paging = FALSE,
                                                                               dom = "Bfrtip", 
                                                                               buttons = list("copy",
                                                                                              list(extend = "collection",buttons = "csv",   text = "Download", sDom  = '<"top">lrt<"bottom">ip'))), rownames = FALSE, extensions = "Buttons")
      }
      
    }  
    
    else {
      if(no_scroll){
        the_table <- DT::datatable(the_table, fillContainer = F,options = list(sDom  = '<"top">lrt<"bottom">ip',#pageLength = nrows,
                                                                               scrollY = '300px', paging = FALSE,
                                                                               columnDefs = list(list(className = "dt-right",
                                                                                                      targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      } else {
        the_table <- DT::datatable(the_table, fillContainer = F,options = list(pageLength = nrows,
                                                                               columnDefs = list(list(className = "dt-right",sDom  = '<"top">lrt<"bottom">ip',
                                                                                                      targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      }
    }
  }
  return(the_table)
}


# Define function for printing nice html tables
prettify_scroll <- function (the_table, remove_underscores_columns = TRUE, cap_columns = TRUE,
                             cap_characters = TRUE, comma_numbers = TRUE, date_format = "%B %d, %Y",
                             round_digits = 2, remove_row_names = TRUE, remove_line_breaks = TRUE,
                             data_table = TRUE, nrows = 5, download_options = FALSE, no_scroll = TRUE,
                             scroll_x = TRUE){
  column_names <- names(the_table)
  the_table <- data.frame(the_table)
  names(the_table) <- column_names
  classes <- lapply(the_table, function(x) {
    unlist(class(x))[1]
  })
  if (cap_columns) {
    names(the_table) <- Hmisc::capitalize(names(the_table))
  }
  if (remove_underscores_columns) {
    names(the_table) <- gsub("_", " ", names(the_table))
  }
  for (j in 1:ncol(the_table)) {
    the_column <- the_table[, j]
    the_class <- classes[j][1]
    if (the_class %in% c("character", "factor")) {
      if (cap_characters) {
        the_column <- as.character(the_column)
        the_column <- Hmisc::capitalize(the_column)
      }
      if (remove_line_breaks) {
        the_column <- gsub("\n", " ", the_column)
      }
    }
    else if (the_class %in% c("POSIXct", "Date")) {
      the_column <- format(the_column, format = date_format)
    }
    else if (the_class %in% c("numeric", "integer")) {
      the_column <- round(the_column, digits = round_digits)
      if (comma_numbers) {
        if(!grepl('year', tolower(names(the_table)[j]))){
          the_column <- scales::comma(the_column)
        }
      }
    }
    the_table[, j] <- the_column
  }
  if (remove_row_names) {
    row.names(the_table) <- NULL
  }
  if (data_table) {
    if (download_options) {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          scrollX = scroll_x,
          dom = "Bfrtip", buttons = list("copy", 
                                         list(extend = "collection", buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             scrollX = scroll_x,
                                                             # scrollY = '300px', paging = FALSE,
                                                             dom = "Bfrtip", buttons = list("copy",
                                                                                            list(extend = "collection", buttons = "csv",
                                                                                                 text = "Download"))), rownames = FALSE, extensions = "Buttons")
      }
      
    }
    else {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          scrollX = scroll_x,
          columnDefs = list(list(className = "dt-right",
                                 targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             scrollX = scroll_x,
                                                             columnDefs = list(list(className = "dt-right",
                                                                                    targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      }
    }
  }
  return(the_table)
}



# default colors for plotly in order
#1f77b4 or rgb(31, 119, 180)
#ff7f0e or rgb(255, 127, 14)
#2ca02c or rgb(44, 160, 44)
#d62728 or rgb(214, 39, 40)
#9467bd or rgb(148, 103, 189)
#8c564b or rgb(140, 86, 75)


# # # barplot with ggplotly
# temp_dat <- temp
# hole_value = 0.4
# geo_names <- unique(temp$Geography)
# var_lab = 'Sex'

plotly_demo <- function(temp_dat, 
                        hole_value, 
                        geo_names, 
                        var_lab,
                        by_geo){
  
  
  # get year and remove column
  year_value <- as.character(temp_dat$year)
  temp_dat$year <- NULL
  
  # # get font list
  # title_name <- paste0('% youth in ', year_value)
  # title_f <- list(
  #   family = "Ubuntu",
  #   size = 12,
  #   color = "#1F2023"
  # )
  # 
  title_name <- ''
 
  
  if(by_geo){
    temp_dat <- temp_dat %>%
      group_by(Geography) %>%
      mutate(tot_pop = sum(Population))  %>%
      group_by(Geography, V2) %>%
      mutate(pop_per =round((Population/tot_pop)*100,  2))    
    # reset geo_names so it is forced to my pie chart
    if(length(unique(temp_dat$Geography)))
    geo_names <- c('one')
    
    
  } else {
    # rename variable fo plotting 
    colnames(temp_dat)[2] <- 'V2'
    
    temp_dat <- temp_dat %>%
      group_by(Geography) %>%
      mutate(tot_pop = sum(Population))  %>%
      group_by(Geography, V2) %>%
      mutate(pop_per =round(Population/tot_pop,  2))
  }
 
  
  f <- list(
    family = "Ubuntu",
    size = 20,
    color = "white"
  )
  
  if(length(geo_names) == 1){
    plotly_plot <-  plot_ly(temp_dat,labels = ~V2, values = ~pop_per,
                            type ='pie',
                            hole = hole_value,
                            textposition = 'inside',
                            textinfo = 'percent',
                            insidetextfont = f,
                            hoverinfo = 'label+value')  %>%
      
      config(displayModeBar = F) %>%
      
      layout(title ='' , font = '', showlegend = T,
             annotations = list(
               showarrow = FALSE,
               text = '',
               font = list(color = '#1F2023',
                           family = 'sans serif')), 
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  } else {
    if(length(unique(temp_dat$V2)) == 2) {
      cols <- c('#1f77b4', '#2ca02c')
    } else {
      # plot data
      cols <- colorRampPalette(brewer.pal(9, 'Greens'))(length(unique(temp_dat$V2)))
    }
    
    # # get title based on var_lab
    # title_name <- paste0('% of youth population by ', var_lab)

    
    g <- ggplot(data = temp_dat,
                aes(x = Geography,
                    y = pop_per,
                    fill = V2, 
                    text = paste('Population', Population,
                                 '<br>', V2,
                                 '<br>Location: ', as.factor(Geography)))) +
      geom_bar(position = 'dodge', stat = 'identity', colour = 'black', alpha = 0.8) +
      theme(legend.position = 'bottom') + scale_y_continuous(labels = scales::percent) +
      # geom_text(aes(label = pop_per), position = position_dodge(width = 1), vjust = -0.5) +
      labs(x = '', y = ' ', title = '') +  theme(plot.title = element_text(size=12)) 
    
    g <- g + scale_fill_manual(name = '',
                               values = cols) + theme_bw(base_size = 13, base_family = 'Ubuntu') 
    
    
    
    plotly_plot <- plotly::ggplotly(g, tooltip = 'text') 
    
    if(var_lab == 'Visible minority'){
      plotly_plot <- plotly_plot %>% config(displayModeBar = F) %>%
        layout(showlegend = F)
      
    } else {
      plotly_plot <- plotly_plot %>% config(displayModeBar = F) %>%
        layout(legend = list(
          orientation = "h",
          y = -0.1))
    }
    
  }
  
  
  return(plotly_plot)
  
}


demo_table <- function(temp_dat, 
                       geo_names, 
                       var_lab,
                       by_geo){
  

  # get year and remove column
  year_value <- as.character(temp_dat$year)
  temp_dat$year <- NULL
  
  # get v2
  colnames(temp_dat)[2] <- 'V2'
  
  temp_dat <- temp_dat %>%
    group_by(Geography) %>%
    mutate(`Total population` = sum(Population))  %>%
    group_by(Geography, V2) %>%
    mutate(`Percent youth` =round((Population/`Total population`)*100,  2))    
 
    colnames(temp_dat)[colnames(temp_dat) == 'Population'] <- 'Youth population'
 
  return(temp_dat)
  
}



#1f77b4 or rgb(31, 119, 180)
#ff7f0e or rgb(255, 127, 14)
#2ca02c or rgb(44, 160, 44)
#d62728 or rgb(214, 39, 40)
#9467bd or rgb(148, 103, 189)
#8c564b or rgb(140, 86, 75)
# function for first plot - demo percent age by geography
plot_age_demo <- function(location, years){
  
  demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex",
                 "Place of Birth","Visible minority", "Aboriginal identity", 'Population')
  new_census <- census[ , demo_vars]
  
  if(length(location) == 1){
    # add ontario 
    all_locations <- unique(census$Geography)
    all_locations <- all_locations[all_locations != location]
    location <- c(location, all_locations)
    location <- location[location != 'Ontario']
    
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(year %in% years) %>% filter(grepl('Total',`Age group`)) %>%
      filter(grepl('Total',`Sex`)) %>% filter(grepl('Total',`Place of Birth`)) %>%
      filter(grepl('Total',`Visible minority`)) %>% filter(grepl('Total',`Aboriginal identity`))
    
    
    # grpup by groups and get mean percentage 
    # keep only age group, year, and population
    temp <- temp[, c('Geography', 'geo_code','year','Population')]
    # 
    temp$year <- as.character(temp$year)
    census_pop$year <- as.character(census_pop$year)
    
    
    temp <- inner_join(temp, census_pop, by = c('year', 'geo_code', 'Geography'))
    
    # get group by indicator
    temp$Geography <- ifelse(temp$Geography == location[1], location[1], 'Rest of Ontario')
    
    temp <- temp %>% 
      group_by(Geography) %>% 
      summarise(sum_youth = sum(Population))
    
    temp$total <- sum(temp$sum_youth)
    
    temp$percent_youth <- round((temp$sum_youth/temp$total)*100, 2)
    
    # get title based on var_lab 
    title_name <- paste0('% youth in ', years)
    
    
    f <- list(
      family = "Ubuntu",
      size = 20,
      color = "white"
    )
    
    g <-  plot_ly(temp,labels = ~Geography, values = ~percent_youth,
                   type ='pie',
                   hole = 0.4,
                   textposition = 'inside',
                   textinfo = 'percent',
                   insidetextfont = f,
                   hoverinfo = 'label+value')  %>%
      
      config(displayModeBar = F) %>%
      
      layout(title = '', font = '', showlegend = F,
             annotations = list(
               showarrow = FALSE,
               text = '',
               font = list(color = '#1F2023',
                           family = 'sans serif')), 
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
    
  } else {
    # 
    temp <- new_census %>% filter(Geography %in% location) %>%
      filter(!Geography %in% 'Ontario') %>%
      filter(year %in% years)
    
    
    # 
    temp <- temp %>% filter(grepl('Total',`Age group`)) %>%
      filter(grepl('Total',`Sex`)) %>% filter(grepl('Total',`Place of Birth`)) %>%
      filter(grepl('Total',`Visible minority`)) %>% filter(grepl('Total',`Aboriginal identity`))
    
    
    # keep only age group, year, and population
    temp <- temp[, c('Geography', 'geo_code','year','Population')]
    # 
    temp$year <- as.character(temp$year)
    census_pop$year <- as.character(census_pop$year)
    # 
    temp <- inner_join(temp, census_pop, by = c('year', 'geo_code'))
    # make percentage youth variable
    temp$Percent <- round((temp$Population/temp$`Total population`)*100, 2)
    
    temp$Geography.y <- NULL
    
    
    g  <- plot_ly(temp, x = ~Percent, y = ~reorder(Geography.x, Percent), 
                  type = 'bar', orientation = 'h',
                  marker = list(color = 'rgb(44, 119, 226, 0.6)',
                                line = list(color = 'black', width = 2))) %>%
      layout(title = '% youth aged 15 to 29',
             yaxis = list(title = '', showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
      add_annotations(xref = 'x1', yref = 'y',
                      x = temp$Percent + 2.5,  y = temp$Geography.x,
                      text = paste(round(temp$Percent, 2), '%'),
                      font = list(family = 'Ubuntu', size = 12, color = 'black'),
                      showarrow = FALSE)
    
    # # get title based on var_lab 
    # title_name <- ''
    # 
    # g <- ggplot(data = temp,
    #             aes(x = reorder(Geography.x, -per_youth),
    #                 y = per_youth,
    #                 text = paste('Population', `Total population`,
    #                              '<br>', paste0((per_youth)*100, ' %'),
    #                              '<br>Location: ', as.factor(Geography.x)))) +
    #   geom_bar(stat = 'identity', colour = 'black', fill = '#1f77b4', alpha = 0.7) +
    #   theme(legend.position = 'bottom') + scale_y_continuous(labels = scales::percent) +
    #   # geom_text(aes(label = pop_per), position = position_dodge(width = 1), vjust = -0.5) +
    #   labs(x = '', y = ' ', title = '') 
    # 
    # g <- g + scale_fill_manual(name = '',
    #                            values = cols) + theme_bw(base_size = 13, base_family = 'Ubuntu') + 
    #   theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.5, hjust=1))  
    # 
    # 
    # g <- plotly::ggplotly(g, tooltip = 'text')
    
    
  }
  return(g)
  
}


leaf_income <- function(x, 
                        tile = 'OpenStreetMap', 
                        palette = 'Oranges',
                        income_status_map_demo_filter,
                        show_legend = TRUE,
                        title = NULL){
  
  # This function expects "x" to be a dataframe with a column named "geo_code" (a 4 character string)
  # and another named "value"
  # Keep only the numbered values
  right <- x %>%
    filter(!is.na(as.numeric(geo_code))) %>%
    mutate(geography = substr(geo_code, 3,4))
  # join to ont shapefile
  shp <- ont2
  shp@data <- shp@data %>%
    mutate(geography = CCA_2) %>%
    left_join(right,
              by = 'geography')
  
  # insure that the full range of numbers is captured by aplying floor and then adding one to the max.
  bins <- unique(floor(c(quantile(shp@data$`Percent low income status`, 
                                  na.rm = TRUE, c(seq(0, 1, 0.15), 1)))), 2)
  
  bins[length(bins)] <- bins[length(bins)] +1
  
  
  pal <- colorBin(palette, domain = shp@data$`Percent low income status`, 
                  # bins = bins,
                  bins = 7,
                  na.color = NA)
  
  # Create a popup
  popper <- paste0(shp@data$NAME_2, ': ', round(shp@data$`Percent low income status`, 2),'%')
  
  # Create map
  l <- leaf_basic_income(shp = shp, tile, palette = palette)
  # l <- leaflet(data = shp) %>%
  #   addProviderTiles(tile)
  if(show_legend){
    l <- l %>%
      addLegend(pal = pal, 
                values =~`Percent low income status`, 
                opacity = 0.8, 
                position = "topright",
                title = paste0('% low income status ' ,income_status_map_demo_filter))
  }
  print('shp is')
  print(head(shp@data))
  l <- l %>%
    addPolygons(fillColor = ~pal(`Percent low income status`),
                fillOpacity = 0.8,
                color = "grey",
                weight = 1,
                # popup = popper,
                # highlight = highlightOptions(
                #   weight = 5,
                #   color = "white",
                #   dashArray = "",
                #   fillOpacity = 0.7,
                #   bringToFront = TRUE),
                label = popper,
                labelOptions = labelOptions(noHide = FALSE, direction = "auto",
                                            style = list(
                                              "color" = "#191A1C",
                                              "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                              "font-size" = "18px",
                                              "border-color" = "rgba(0,0,0,0.5)"
                                            ))) 
  # %>% 
  #   setView(lng = -84.3232, lat = 53.25, zoom = 6)
  return(l)
}

leaf_basic_income <- function(shp = ont2, tile, palette){
  
  l <- leaflet(data = shp) %>%
    addProviderTiles(tile) 
  return(l)
}


# function for line chart in employment
emp_line <- function(temp_dat) {
  # plot data
  
  if(length(unique(temp_dat$year)) == 1){
    x_label <- as.character(unique(temp_dat$year))
    new_theme <- theme(axis.text.x=element_blank(),
                       axis.ticks.x=element_blank()) 
  } else {
    x_label <- ''
    new_theme <- NULL
  }
  
  g <- ggplot(data = temp_dat,
              aes(x = year,
                  y = value,
                  group = variable,
                  colour = variable,
                  text = paste('<br>', value , as.factor(variable)))) +
    geom_point(size = 4) +
    geom_line(size = 2, alpha = 0.8) +
    scale_color_manual(name = '', 
                       values = c('grey', 'black')) + scale_y_continuous(labels = scales::percent) +
    labs(x = x_label, y = ' ', title ='')  + theme_bw(base_size = 14, base_family = 'Ubuntu') + new_theme
  
  return(g)
}


ed_candle_plot <- function(temp_dat, ed_var, title, color_palette, location, years) {
  
  
  # Plot
  cols <- color_palette
  cols <- adjustcolor(cols, alpha.f = 0.7)
  g <- ggplot(temp_dat, aes(x=Geography, 
                        y=per,
                        text = paste(Geography, ' ', V2,
                                     '<br>', (per)*100, '% ',ed_var))) + 
    geom_point(size=5, aes(color = V2)) + 
    geom_segment(aes(x=Geography, 
                     xend=Geography, 
                     y=0, 
                     yend=per)) + 
    theme_bw(base_size = 14, base_family = 'Ubuntu')  +
    scale_color_manual(name = '', 
                      values = cols) +
    scale_y_continuous(labels = scales::percent) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(title=title, x = '', y = ' ') 
  
  if(length(unique(temp_dat$V2)) > 5) {
    g_plot <- plotly::ggplotly(g, tooltip = 'text')  %>% config(displayModeBar = F) 
      
  } else {
    g_plot <- plotly::ggplotly(g, tooltip = 'text')  %>% config(displayModeBar = F) %>%
      layout( 
        legend = list(
          orientation = "l",
          x = 0,
          y = -0.3))
  }
 
  
  return(g_plot)
}


# # get data by year 
# temp_2011 <- temp_melt[temp_melt$year == '2011',]
# temp_2016 <- temp_melt[temp_melt$year == '2016',]
# 
# temp_2011$year <- NULL
# temp_2016$year <- NULL
# 
# 
# plot_2011 <- gvisPieChart(temp_2011, 
#              options=list(title='2011',
#                           titlePosition='out',
#                           fontSize = 15,
#                           width=275,
#                           height=400,
#                           pieSliceText = 'value',
#                           hAxis="{slantedText:'true',slantedTextAngle:45}",
#                           titleTextStyle="{color:'black',fontName:'Courier', fontSize:'25'}",
#                           legend="{position:'none',color:'black',fontName:'Courier'}",
#                           chartArea="{left:10,top:30,width:'100%',height:'100%'}"))
# 
# plot_2016 <- gvisPieChart(temp_2016, 
#                           options=list(title='2016',
#                                        titlePosition='out',
#                                        fontSize = 15,
#                                        width=275,
#                                        height=400,
#                                        pieSliceText = 'value',
#                                        titleTextStyle="{color:'black',fontName:'Courier', fontSize:'25'}",
#                                        legend="{position:'none',color:'black',fontName:'Courier'}",
#                                        chartArea="{left:10,top:30,width:'100%',height:'100%'}"))
# 
# 



# # get_plotly_pie <-
#   function(temp_dat,
#            var1,
#            var2,
#            location) {
# 
#   # get font list
#   f <- list(
#     family = "Ubuntu",
#     size = 15,
#     color = "white"
#   )
# 
#   # get year number and vector of all years
#   all_years <- as.character(sort(unique(temp_dat$year)))
#   num_years <- length(all_years)
# 
#   # year conditionals
#   if(num_years == 1) {
# 
#     p <-  plot_ly(labels = temp_dat[[V2]], values =temp_dat[[pop_per]] ,type ='pie',
#                   textposition = 'inside',
#                   textinfo = 'percent',
#                   insidetextfont = f,
#                   hoverinfo = 'label+value',
#                   marker = list(colors = colors,
#                                 line = list(color = '#FFFFFF', width = 1.5))) 
#      
#   }
# 
#   if(num_years == 2) {
# 
#     # get year data sets
#     temp_1 <- temp_dat[temp_dat$year == all_years[1], ]
#     temp_2 <- temp_dat[temp_dat$year == all_years[2], ]
# 
# 
#     p1 <-  plot_ly(temp_1,labels = ~V2, values = ~pop_per ,
#                    type ='pie',
#                    hole = 0.5,
#                   textposition = 'inside',
#                   textinfo = 'percent',
#                   insidetextfont = f,
#                   hoverinfo = 'label+value')  %>%
# 
#       layout(showlegend = F,
#              annotations = list(
#                list(text = all_years[1], showarrow = F)),
#              font = list(color = '#264E86',
#                          family = 'sans serif',
#                          size = 20))
#     
#     p2 <-  plot_ly(temp_2, labels = ~V2, values = ~pop_per ,
#                    type ='pie',
#                    hole = 0.5,
#                    textposition = 'inside',
#                    textinfo = 'percent',
#                    insidetextfont = f,
#                    hoverinfo = 'label+value')  %>%
#       
#       layout(showlegend = F,
#              annotations = list(
#                list(text = all_years[2], showarrow = F)),
#              font = list(color = '#264E86',
#                          family = 'sans serif',
#                          size = 20))
#     
#     subplot(p1,p2, nrows = 1, titleY = TRUE, shareX = TRUE)
# 
#   }
# 
#   if(num_years == 3) {
#     # get year data sets
#     temp_1 <- temp_dat[temp_dat$year == all_years[1], ]
#     temp_2 <- temp_dat[temp_dat$year == all_years[2], ]
#     temp_3 <- temp_dat[temp_dat$year == all_years[3], ]
# 
#     p <-  plot_ly(labels = temp_1[[var1]], values =temp_1[[var2]] ,type ='pie',
#                   domain = list(x = c(0, 0.4), y = c(0.4, 1)),
#                   textposition = 'inside',
#                   textinfo = 'percent',
#                   insidetextfont = f,
#                   hoverinfo = 'label+value',
#                   marker = list(colors = colors,
#                                 line = list(color = '#FFFFFF', width = 1.5))) %>%
#       add_pie(labels = temp_2[[var1]], values =temp_2[[var2]] ,
#               domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>%
#       add_pie(labels = temp_3[[var1]], values =temp_3[[var2]] ,
#               domain = list(x = c(0, 0.4), y = c(0, 0.4))) %>%
# 
#       layout(showlegend = F,
#              annotations = list(
#                list(x = 0.12 , y = 1, text = all_years[1], showarrow = F),
#                list(x = 0.88 , y = 1, text = all_years[2], showarrow = F),
#                list(x = 0.12 , y = 0.45, text = all_years[3], showarrow = F)),
#              font = list(color = '#264E86',
#                          family = 'sans serif',
#                          size = 20))
#   }
# 
#   if(num_years == 4) {
#     # get year data sets
#     temp_1 <- temp_dat[temp_dat$year == all_years[1], ]
#     temp_2 <- temp_dat[temp_dat$year == all_years[2], ]
#     temp_3 <- temp_dat[temp_dat$year == all_years[3], ]
#     temp_4 <- temp_dat[temp_dat$year == all_years[4], ]
# 
# 
#     p <-  plot_ly(labels = temp_1[[var1]], values =temp_1[[var2]] ,type ='pie',
#                   domain = list(x = c(0, 0.4), y = c(0.4, 1)),
#                   textposition = 'inside',
#                   textinfo = 'percent',
#                   insidetextfont = f,
#                   hoverinfo = 'label+value',
#                   marker = list(colors = colors,
#                                 line = list(color = '#FFFFFF', width = 1.5))) %>%
#       add_pie(labels = temp_2[[var1]], values =temp_2[[var2]] ,
#               domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>%
#       add_pie(labels = temp_3[[var1]], values =temp_3[[var2]] ,
#               domain = list(x = c(0, 0.4), y = c(0, 0.6))) %>%
#       add_pie(labels = temp_4[[var1]], values =temp_4[[var2]] ,
#               domain = list(x = c(0.6, 1), y = c(0, 0.6))) %>%
# 
#       layout(showlegend = F,
#              annotations = list(
#                list(x = 0.12 , y = 1, text = all_years[1], showarrow = F),
#                list(x = 0.88 , y = 1, text = all_years[2], showarrow = F),
#                list(x = 0.12 , y = 0.45, text = all_years[3], showarrow = F),
#                list(x = 0.88 , y = 0.45, text = all_years[4], showarrow = F)),
#              font = list(color = '#264E86',
#                          family = 'sans serif',
#                          size = 20))
#   }
#   return(p)
# 
# 
# }
#
# 
# 
# 
# # plotly_plot <-  plot_ly(temp, labels = ~`Age group`, values = ~`Population` ,type ='pie',
# #          textposition = 'outside',
# #          textinfo = 'percent',
# #          insidetextfont = f,
# #          hoverinfo = 'label+value',
# #          text = ~paste('Total population for age group'),
# #          marker = list(colors = colors,
# #                        line = list(color = '#FFFFFF', width = 1.5))) %>%
# #    
# #    layout(title = "",  
# #           showlegend = F,
# #           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
# #           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
# #           title = paste0('Population of youth in ', location)) %>%
# #   
# #  ggplotly(plotly_plot, width = 10) 
# #  
# 
# 

leaf_region <- function(region_map, index = 1:4){
  # cols <- brewer.pal(n = 4, name = 'Set3')
  the_colors <- c('red', 'blue', 'orange', 'purple')
  cols <- adjustcolor(the_colors, alpha.f = 0.1)
  if(length(index) > 0){
    for(i in index){
      cols[i] <- the_colors[i]
    }
  }
  
  x <- region_map
  leaflet(x, option=leafletOptions(zoomControl=FALSE)) %>%
    addTiles(options=tileOptions(minZoom=4, maxZoom=4,
                                 opacity = 0)) %>%
    addPolygons(fillColor = cols,
                fillOpacity = 1,
                opacity = 1,
                weight = 1,
                color = 'black',
                label = x@data$region,
                layerId = x@data$region,
                labelOptions = labelOptions(#noHide = T, 
                  direction = "auto",
                  style = list(
                    "color" = "#191A1C",
                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                    "font-size" = "12px",
                    "border-color" = "rgba(0,0,0,0.5)"
                  )))
  
}
