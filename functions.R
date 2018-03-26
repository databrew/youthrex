library(shinydashboard)
library(sp)
library(raster)
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
library(foreign)
library(sas7bdat)
library(memisc)
library(stringdist)

# Function for map
ontario_map <- function(x){
  require(dplyr)
  require(ggplot2)
  # This function expects "x" to be a dataframe with a column named "geography"
  # and another named "value"
  # Keep only the numbered values
  right <- x %>%
    filter(!is.na(as.numeric(geography))) %>%
    mutate(geography_name = geography) %>%
    mutate(geography = substr(geography, 3,4))
  # join to ont fortified
  shp <- ont_fortified
  shp <- shp %>%
    left_join(right,
              by = 'geography')
  # Make a plot
  g <-
    ggplot(data = shp,
           aes(x = long,
               y = lat,
               group = group,
               fill = value)) +
    geom_polygon() +
    coord_map() +
    theme_databrew() +
    scale_fill_continuous(low = 'lightblue', high = 'darkorange', name = '', na.value = 'white') +
    theme(legend.text = element_text(size = 7),
          legend.position = 'right') +
    labs(x = '',
         y = '')
  return(g)
}
# Define function for generating a leaflet plot

# Create a basic leaflet with nothing else on it
leaf_basic <- function(shp = ont2){
  tile = 'OpenStreetMap.Mapnik'
  palette = 'Purples'
  l <- leaflet(data = shp) %>%
    addPolylines(color = NA, opacity = 0.5, weight = 0.2) %>%
    addProviderTiles(tile,
                     options = providerTileOptions(minZoom = 4, maxZoom = 10)) 
  return(l)
}

leaf <- function(x, 
                 tile = 'OpenStreetMap.Mapnik', 
                 palette = 'Purples',
                 show_legend = TRUE,
                 title = NULL){
  require(dplyr)
  require(leaflet)
  require(RColorBrewer)
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

  # Create a color palette
  # pal <- colorQuantile("Blues", NULL, n = 9)
  # bins <- round(c(quantile(shp@data$value, na.rm = TRUE), Inf))
  bins <- unique(round(c(quantile(shp@data$value, na.rm = TRUE, c(seq(0, 1, 0.15), 1)))))
  pal <- colorBin(palette, domain = shp@data$value, bins = bins,
                  na.color = NA)

  # Create a popup
  popper <- paste0(shp@data$NAME_2, ': ',
                   round(shp@data$value, 2))

  # Create map
  l <- leaf_basic(shp = shp)
  # l <- leaflet(data = shp) %>%
  #   addProviderTiles(tile)
  if(show_legend){
    l <- l %>%
      addLegend(pal = pal, values = ~value, opacity = 0.7, 
                position = "bottomleft",
                title = title)
  }
  l <- l %>%
    addPolygons(fillColor = ~pal(value),
                fillOpacity = 0.8,
                color = "#BDBDC3",
                weight = 1,
                # popup = popper,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label = popper,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", 
                               padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))
  return(l)
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
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
          dom = "Bfrtip", buttons = list("copy", "print",
                                         list(extend = "collection", buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
          # scrollY = '300px', paging = FALSE,
          dom = "Bfrtip", buttons = list("copy", "print",
                                         list(extend = "collection", buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      }
      
    }
    else {
      if(no_scroll){
        the_table <- DT::datatable(the_table, options = list(#pageLength = nrows,
          scrollY = '300px', paging = FALSE,
                                                             columnDefs = list(list(className = "dt-right",
                                                                                    targets = 0:(ncol(the_table) - 1)))), rownames = FALSE)
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             columnDefs = list(list(className = "dt-right",
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
          dom = "Bfrtip", buttons = list("copy", "print",
                                         list(extend = "collection", buttons = "csv",
                                              text = "Download"))), rownames = FALSE, extensions = "Buttons")
      } else {
        the_table <- DT::datatable(the_table, options = list(pageLength = nrows,
                                                             scrollX = scroll_x,
                                                             # scrollY = '300px', paging = FALSE,
                                                             dom = "Bfrtip", buttons = list("copy", "print",
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

# Define function for subsetting tables
subset_table <- function(data = census_all,
                        geo_code = NULL,
                        year = NULL,
                        age = NULL,
                        sex = NULL,
                        pob = NULL,
                        vm = NULL,
                        si = NULL){
  # Create data
  sub_data <- data

  # Modify param names
  the_geo_code = geo_code
  the_year = year
  the_age = age
  the_sex = sex
  the_pob = pob
  the_vm = vm
  the_si = si

  # Empty vector of groupers
  groupers <- c()

  if(!is.null(geo_code)){
    sub_data <- sub_data %>% dplyr::filter(geo_code == the_geo_code)
  } else {
    groupers <- c(groupers, 'geo_code')
  }
  if(!is.null(year)){
    sub_data <- sub_data %>% dplyr::filter(year == the_year)
  } else {
    groupers <- c(groupers, 'year')
  }
  if(!is.null(age)){
    sub_data <- sub_data %>% dplyr::filter(age == the_age)
  } else {
    groupers <- c(groupers, 'age')
  }
  if(!is.null(sex)){
    sub_data <- sub_data %>% dplyr::filter(sex == the_sex)
  } else {
    groupers <- c(groupers, 'sex')
  }
  if(!is.null(pob)){
    sub_data <- sub_data %>% dplyr::filter(pob == the_pob)
  } else {
    groupers <- c(groupers, 'pob')
  }
  if(!is.null(vm)){
    sub_data <- sub_data %>% dplyr::filter(vm == the_vm)
  } else {
    groupers <- c(groupers, 'vm')
  }
  if(!is.null(si)){
    sub_data <- sub_data %>% dplyr::filter(si == the_si)
  } else {
    groupers <- c(groupers, 'si')
  }

  # Apply groupers
  if(length(groupers) > 0){
    sub_data <-
      sub_data %>%
      group_by_(groupers) %>%
      summarise(value = sum(value))
  }

  return(sub_data)
}


# define function for filtering data by inputs
# year Numeric vector of length 1 or 3. For example, c(2001, 2006) to keep data from both years
censify <- function(df = census,
                    dict = census_dict,
                    age = FALSE, 
                    sex = FALSE,
                    pob = FALSE,
                    vm = FALSE, 
                    ai = FALSE,
                    geo_code = FALSE,
                    years = 2001,
                    sc = NULL,
                    percent = 'Percentage') { # one of Percentage, Raw Numbers, or Both
  # category
  if(!is.null(sc)) {
    if(!sc %in% unique(dict$sub_category)) {
      stop("sc must be one of ", paste0(unique(dict$sub_category), collapse = ", "))
    }
    # extract columns corresponding to category from census dict
    keep_columns <- dict %>%
      filter(sub_category %in% c('demographic', 'geo_code', 'year', sc)) %>%
      .$variable
    df <- df[, keep_columns]
  }
  
  # age 
  if(age) {
    df <- df %>%
      filter(!grepl('Total', `Age group`))
  } else {
    df <- df %>%
      filter(grepl('Total', `Age group`))
    df$`Age group` <- NULL
  }
  
  # sex
  if(sex){
    df <- df %>%
      filter(!grepl('Total', Sex))
  } else {
    df <- df %>%
      filter(grepl('Total', Sex))
    df$Sex <- NULL
  }
  
  # pob
  if(pob){
    df <- df %>%
      filter(!grepl('Total', `Place of Birth`))
  } else {
    df <- df %>%
      filter(grepl('Total', `Place of Birth`))
    df$`Place of Birth` <- NULL
  }
  
  # vm
  if(vm){
    df <- df %>%
      filter(!grepl('Total', `Visible minority`))
  } else {
    df <- df %>%
      filter(grepl('Total',  `Visible minority`))
    df$`Visible minority` <- NULL
  }
  
  # ai
  if(ai){
    df <- df %>%
      filter(!grepl('Total', `Aboriginal identity`))
  } else {
    df <- df %>%
      filter(grepl('Total',  `Aboriginal identity`))
    df$`Aboriginal identity` <- NULL
  }
  
  # geo_code
  if(geo_code){
    df <- df %>%
      filter(geo_code != '3500')
  } else {
    df <- df %>%
      filter(geo_code == '3500')
    df$geo_code <- NULL
    df$Geography  <- NULL
  }
  
  # filter for year
  df <- df %>%
    filter(year %in% years)
  # if(length(years) == 1){
  #   df$year <- NULL
  # }
  
  # make percentages 
  if(percent %in% c('Percentage', 'Both')) {
    if(is.null(sc)) {
      warning("Choose a sub category to generate percentages")
    } else {
      if(!sc %in% c('income', 'total population count')){
        denom_column <- which(grepl('Total', names(df)))
        denom <- df[, denom_column]
        names(denom) <- 'x'
        denom <- denom$x
        
        # identify indices of numerator columns
        ni <- (denom_column + 1):ncol(df)
        
        # Only convert to percentage those columns which don't already
        # have the words "ratio", "rate" or "%" in them
        ni_names <- names(df)[ni]
        already_percentage <- 
          ni_names[
            grepl('ratio', ni_names) |
              grepl(' rate', ni_names) |
              grepl('%', ni_names, fixed = TRUE)]
        already_percentage <- which(names(df) %in% already_percentage)
        ni <- ni[!ni %in% already_percentage]
        
        # make temp data frame 
        col_names <- names(df)
        
        df <- as.data.frame(df)
        
        if(percent == 'Percentage'){
          for(j in ni) {
              df[,j] <- (df[, j]/denom)*100
          }
        } else if(percent == 'Both'){
          for(j in ni) {
              n <- df[,j]
              p <- (df[, j]/denom)*100
              df[,j] <- paste0(prettyNum(n, big.mark = ','), ' (',
                               round(p, 2), '%)')
          }
        }
        
        colnames(df) <- col_names
      }
    }
  }
  total_columns <- grepl('Total', names(df))
  if(length(which(total_columns)) == 1){
    names(df)[total_columns] <- 'Total' 
  }
  return(df)
}

# Define function for plotting between 1 and 3 variables
plotter <- function(df, variable = NULL, show_labels = TRUE){
  # Skim down the columns to only keep the grouper ones and the plotting variable
  df <- df[,names(df) %in% c(head_vector, variable)]
  no_year <- FALSE
  if(length(variable) != 1){
    too_many_variables <- TRUE
  } else {
    too_many_variables <- FALSE
  }
  
  all_nas <- FALSE
  if(length(variable) == 1){
    which_var <- which(names(df) == variable)
    val <- df[, which_var]
    if(all(is.na(val))){
      all_nas <- TRUE
    }
  }
  if(all_nas){
    the_title <- paste0('There is no data available for ', variable, ' for the year(s) selected.')
    return(ggplot() +
             theme_databrew() +
             labs(title = the_title))
  } else {

    one_year <- FALSE
    if ('year' %in% names(df)) {
      df$year <- as.factor(df$year)
      if(length(unique(df$year)) == 1){
        one_year <- TRUE
      }
    } 
    if(nrow(df) == 0){
      no_year <- TRUE
    }
    df <- df[,names(df) != 'geo_code']
    total_column <- which(grepl('Total', names(df)))[1]
    df <- df[,-total_column]
    # Identify the column to be plotted
    plot_column <- ncol(df)#  total_column + 1
    original_y <- names(df)[plot_column]
    names(df)[plot_column] <- 'y'
    # Only keep necessary columns
    df <- df[,1:plot_column]
    if(
      (!one_year & ncol(df) > 4) |
      (one_year & ncol(df) > 5) | 
      no_year | 
      too_many_variables){
      st <- ''
      if(no_year){
        st <- 'You must select at least one year'
      } else {
        st <- 'Variables can be Age group, Sex, Place of Birth, Visible minority, Geography and Year(s)'
      }
      if(too_many_variables){
        the_title <- 'Pick only one variable in the upper right to view a plot'
      } else {
        the_title <- 'Pick between 0 and 3 sub-groups to view a plot'
      }
      
      return(ggplot() +
               theme_databrew() +
               labs(title = the_title,
                    subtitle = st))
    } else {
      if(one_year & ncol(df) > 2){
        df$year <- NULL
        plot_column <- plot_column - 1
      }
      original_var_names <- names(df)[1:(plot_column - 1)]
      # if('year' %in% original_var_names){
      #   if(length(unique(df$year)) == 1){
      #     df <- df[,names(df) != 'year']
      #     original_var_names <- original_var_names[original_var_names != 'year']
      #   }
      # }
      names(df)[1:(plot_column - 1)] <- paste0('var', 1:(plot_column - 1))
      # Define how many plot variables there are
      plot_variables <- plot_column - 1
      # Make a plot 
      g <- ggplot(data = df,
                  aes(x = var1,
                      y = y)) +
        geom_bar(stat = 'identity',
                 fill = 'darkorange',
                 alpha = 0.6) 
      if(show_labels){
        g <- g +
          geom_text(aes(label = round(y, digits = 2)), alpha = 0.4,
                    position = position_dodge(width = 1), vjust = -0.5)
      }
      if(plot_variables == 2){
        cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(df$var2)))
        if(length(cols) == 2){
          cols <- c('darkorange', 'lightblue')
        }
        g <- ggplot(data = df,
                    aes(x = var1,
                        y = y, 
                        group = var2,
                        fill = var2)) +
          geom_bar(stat = 'identity', position = 'dodge')  +
          scale_fill_manual(name = '',
                            values = cols) 
        if(show_labels){
          g <- g +
            geom_text(aes(label = round(y, digits = 2)), alpha = 0.4,
                      position = position_dodge(width = 1), vjust = -0.5)
        }
      }
      if(plot_variables == 3){
        cols <- colorRampPalette(brewer.pal(n = 9, 'Spectral'))(length(unique(df$var3)))
        if(length(cols) == 2){
          cols <- c('darkorange', 'lightblue')
        }
        g <- ggplot(data = df,
                    aes(x = var1,
                        y = y, 
                        group = var3,
                        fill = var3)) +
          geom_bar(stat = 'identity', position = 'dodge') +
          scale_fill_manual(name = '',
                            values = cols) +
          facet_wrap(~var2)
        if(show_labels){
          g <- g +
            geom_text(aes(label = round(y, digits = 2)), alpha = 0.4,
                      position = position_dodge(width = 1), vjust = -0.5)
        }
      }
    }
    g <- g +
      theme_databrew() +
      theme(axis.text.x = element_text(angle = 45,
                                       hjust = 1)) 
    title <- paste0(original_y, ' by ', original_var_names[1])
    if(plot_variables == 2){
      title <- paste0(title, ' and ', original_var_names[2])
    }
    if(plot_variables == 3) {
      title <- paste0(title, ', ', original_var_names[2], ', and ', original_var_names[3])
    }
    
    g <- g +
      labs(x = '',
           title = title, 
           y = '')
    if('Geography' %in% original_var_names){
      which_geo <- which(grepl('eograph', original_var_names))
      geo_var <- paste0('var', which_geo)
      lots <- length(unique(df[,geo_var])) > 10
      if(lots){
        if(plot_variables == 3){
          g <- 
            g + theme(axis.text.x = element_text(size = 6))
        } else {
          g <- 
            g + theme(axis.text.x = element_text(size = 8))
        }
        
      }
    }
    return(g)
  }
}



