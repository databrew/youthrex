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
  
  if(length(years) > 1){
    # Create a popup
    popper <- paste0(shp@data$NAME_2, ': ',
                     round(shp@data$per_youth, 2),'% Youth - average for all years selected')
  } else {
    # Create a popup
    popper <- paste0(shp@data$NAME_2, ': ',
                     round(shp@data$per_youth, 2), '% Youth in ', years)
  }
  
  
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



# barplot with ggplotly
# temp_dat <- temp
bar_plotly_demo <- function(temp_dat, no_legend){
  # rename variable fo plotting 
  var_lab <- colnames(temp_dat)[2]
  
  colnames(temp_dat)[2] <- 'V2'
  
  temp_dat <- as.data.frame(temp_dat)
  
  temp_dat <- temp_dat %>%
    group_by(year) %>%
    mutate(tot_pop = sum(Population))  %>%
    group_by(year, V2) %>%
    mutate(pop_per =round((Population/tot_pop)*100,  2))
  temp_dat$year <- as.factor(temp_dat$year)
  
  if(length(unique(temp_dat$V2)) == 2) {
    cols <- c('blue', 'darkgreen')
  } else {
    # plot data
    cols <- colorRampPalette(brewer.pal(9, 'Greens'))(length(unique(temp_dat$V2)))
  }
  
  # get title based on var_lab 
  title_name <- paste0('% of youth population by ', var_lab)
  
  
  g <- ggplot(data = temp_dat,
              aes(x = year,
                  y = pop_per,
                  fill = V2, 
                  text = paste('Population', Population,
                               '<br>', V2,
                               '<br>Year: ', as.factor(year)))) +
    geom_bar(position = 'dodge', stat = 'identity', colour = 'black', alpha = 0.8) +
    theme(legend.position = 'bottom') +
    # geom_text(aes(label = pop_per), position = position_dodge(width = 1), vjust = -0.5) +
    labs(x = '', y = '%', title = title_name) +  theme(plot.title = element_text(size=12)) 
  
    g <- g + scale_fill_manual(name = '',
                               values = cols) + theme_bw(base_size = 10, base_family = 'Ubuntu') 
    
    
     
    g_plotly <- plotly::ggplotly(g, tooltip = 'text') 
    
    if(var_lab == 'Visible minority'){
      g_plotly <- g_plotly %>% config(displayModeBar = F) %>%
        layout(showlegend = F)
      
    } else {
      g_plotly <- g_plotly %>% config(displayModeBar = F) %>%
        layout(legend = list(
          orientation = "h",
          y = -0.1))
    }
  
    
  
  
  return(g_plotly)
  
}
pie_plotly_demo <- function(temp_dat, hole_value){
  # get font list
  
  year_value <- unique(temp_dat$year)
  
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

  # rename variable fo plotting 
  colnames(temp_dat)[2] <- 'V2'
  
   temp_dat <- temp_dat %>%
    group_by(year) %>%
    mutate(tot_pop = sum(Population))  %>%
    group_by(year, V2) %>%
    mutate(pop_per =round((Population/tot_pop)*100,  2))
   
   # get avg population and percentage
   temp_dat <- temp_dat %>%
     group_by(V2) %>%
     summarise(mean_pop = mean(Population, na.rm = T),
               mean_per = mean(pop_per, na.rm = T))

  f <- list(
    family = "Ubuntu",
    size = 20,
    color = "white"
  )
 
  p1 <-  plot_ly(temp_dat,labels = ~V2, values = ~mean_pop,
                 type ='pie',
                 hole = hole_value,
                 textposition = 'inside',
                 textinfo = 'percent',
                 insidetextfont = f,
                 hoverinfo = 'label+value')  %>%
    
    config(displayModeBar = F) %>%
    
    layout(title = title_name, font = title_f, showlegend = F,
           annotations = list(
             showarrow = FALSE,
             text = '',
             font = list(color = '#1F2023',
                         family = 'sans serif')), 
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(p1)

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
