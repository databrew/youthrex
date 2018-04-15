
# temp <- survey[[1]]
# str(temp)
##########
# Source databrew package files
##########
db_files <- dir('R')
for (i in 1:length(db_files)){
  source(paste0('R/', db_files[i]))
}

# Source helper functions
source('functions.R')


##########
# Get Canadian shapefile
##########
# https://drive.google.com/file/d/1vew1zjUFJwR6sQJY0OxaczcddW_fJ1fH/view

if('map_dat.RData' %in% dir('data/geo')){
  load('data/geo/map_dat.RData')
} else {
  # Get a map of canada
  can2 <- raster::getData(name = 'GADM', country = 'CAN', level = 2)
  # Subset to just Toronto
  ont2 <- can2[can2@data$NAME_1 == 'Ontario',]
  # Create a more simplified version of ont2 for faster mapping
  ont2 <- rmapshaper::ms_simplify(ont2, keep_shapes = TRUE)  # Create a crazy-ass looking version
  ont_crazy <- thinnedSpatialPoly(SP = ont2,
                                  minarea = 0,
                                  tolerance = 5,
                                  topologyPreserve = TRUE)
  # Save for later
  save(ont2, ont_crazy,
       file = 'data/geo/map_dat.RData')
}



# Get a fortified version of ont
ont_fortified <- broom::tidy(ont2)
ont_fortified <- ont_fortified %>% left_join(ont2@data %>%
                                               mutate(OBJECTID = as.character(OBJECTID)) %>%
                                               dplyr::select(OBJECTID, CCA_2) %>%
                                               dplyr::rename(id = OBJECTID,
                                                             geography = CCA_2)) %>%
  mutate(geography = as.character(geography))


##########
# This function will be used in the get_data function to clean columns and make long
##########

get_census_data <- function() {
  # first get vector of data set names to loop through later
  data_names <- list.files('data/census_data')
  # get 2001, 2011, and 2016 dictionaries (2006 is the base reference)
  dict_folder <- list.files('dictionaries/census_variables_dict')
  census_vars_dict <- list()
  # loop through and load all variable dictionaries 
  for(i in 1:length(dict_folder)){
    dict_name <- dict_folder[i]
    temp_dict <-  read_csv(paste0('dictionaries/census_variables_dict/', dict_name))
    temp_dict$X1 <-NULL
    # temp_dict$year <- gsub('.csv', '', gsub('temp_', '', dict_name))
    census_vars_dict[[i]] <- temp_dict
  }

  # cread empty list to store data
  data_list <- list()
  total_list <- list()
  # get data type
  sub_names <- data_names[grepl('census', data_names)]
  # function that loops through each name in for census data and read into a list
  for (i in 1:length( sub_names)) {
    name <- sub_names[i]
    # treat 2016 data seperately - 4 data sets in one folder
    if(grepl('2016', name)){
      # list to store results
      data_2016_list <- list()
      data_2016_names <- list.files('data/census_data/2016_census')
      # loop by length -1 because we don't want to read in the income data until later on
      for(j in 1:(length(data_2016_names) - 1)){
        data_name <- data_2016_names[j]
        temp_data <- read_csv(paste0('data/census_data/2016_census/', data_name))
        
        Encoding(temp_data$Geography) <- "latin1"
        
        data_2016_list[[j]] <- temp_data
      }
      
      # combine data
      temp_data <- do.call(rbind, data_2016_list)
      
      # remove duplicates from these columns
      df_dups <- temp_data[c('Geography', 'Age Groups', 'Sex (3)', 'Place of Birth', 'Visible minori')]
      temp_data <- temp_data[!duplicated(df_dups),]
      
      # read in census_2016_5.csv which has income data and is indentical to the four other 2016 datasets once they are aggregated, 
      # allowing for an easy join.
      temp_data_income <- read_csv(paste0('data/census_data/2016_census/census_2016_5.csv'))
      
      # recode income datasets vismin column name to match the others and change encoding
      names(temp_data_income)[5] <- "Visible minori" 
      Encoding(temp_data_income$Geography) <- "latin1"
      
      # join to other 2016 data
      temp_data <- inner_join(temp_data, temp_data_income)
      
      # clean geography variabe so it matches the variable in the dictrionay
      temp_data$Geography <- unlist(lapply(strsplit(as.character(temp_data$Geography), '0', fixed = TRUE),
                         function(x){x[1]}))    
      
      temp_data$Geography <- gsub('[[:digit:]]+', '', temp_data$Geography)
      
      temp_data <- temp_data %>% filter(!grepl('(Part)|Unorganized|IRI|TP|MU|NO|S-|CV|VL| T |CY| C |2| M |)', Geography))
      temp_data$Geography <- trimws(temp_data$Geography)
      
      # clean geography up 
      # get last word
      # temp_data$Geography <- unlist(lapply(lapply(strsplit(temp_data$Geography, ' '), function(x){
      #   x[1:(length(x)-1)]
      # }), function(x){
      #   paste0(x, collapse = ' ')
      # }))

      # add year
      year <- as.numeric(substr(name, 1, 4))
      temp_data$year <- year
      temp_data$geo_code <- NA
      
    } else {
      temp_data <- read_csv(paste0('data/census_data/', name))
      
      # Declare that the encoding is all screwed up for this file
      Encoding(temp_data$Geography) <- "latin1"
      
      # Address the weirdness with "New Credit (Part)"
      temp_data$Geography <- gsub('(Part) ', '', temp_data$Geography, fixed = TRUE)
      # Keep only the first part of the name (what is with the %?)
      temp_data$Geography <- paste0(unlist(lapply(strsplit(temp_data$Geography, ')', fixed = TRUE),
                                                  function(x){x[1]})), ')')
      
      # give Ontario four digit number to subset by.
      temp_data$Geography <- ifelse(grepl('Ontario', temp_data$Geography), 'Ontario', temp_data$Geography)
      
      
      #subset to Ontarios and 4 digit geo codes
      geo_codes <- unlist(lapply(strsplit(temp_data$Geography,
                                          '(', fixed = TRUE),
                                 function(x){
                                   gsub(')', '', x[2], fixed = TRUE)}))
      
      # (those wih NA for the geo_code are all ontario) - give it 3500 so we can subset
      # entirely by 4 digit geo_code
      temp_data$geo_code <- geo_codes
      temp_data$geo_code[is.na(temp_data$geo_code)] <- '3500'
      
      # keep only rows that have 4 number
      temp_data <- temp_data[nchar(temp_data$geo_code) == 4,]
      
      # add year
      year <- as.numeric(substr(name, 1, 4))
      temp_data$year <- year
      
      # get 2011 geography as the map for the 2016 geography
      if(year == 2011){
        geo_unique_2011  <- unique(temp_data$Geography)
      }
      
    }
    
    # 2006 names were the reference year, so no need to get data dictionary for that year
    if(year != 2006){
      # get the index of the list to access the dictionary list generated before
      list_number <- which(grepl(as.character(year), dict_folder))
      census_variables <- as.data.frame(census_vars_dict[[list_number]])
      
      # # now overwite temp_data with names from variable list
      # colnames(temp_data)[!colnames(temp_data) == census_variables$old]
      # 
      # stop if these two are not equal: the uncleaned column name and the old variables 
      stopifnot(colnames(temp_data) == census_variables$old)
      colnames(temp_data) <- census_variables$new
      
      if(year == 2016){
        # get unique geogarphy 
        geo_unique_2016 <- unique(temp_data$Geography)
        # fuzzy matrix
        fuzzy_geo <- stringdistmatrix(a = geo_unique_2011,
                                      b = geo_unique_2016)
        x <- apply(fuzzy_geo, 1, function(x){
          # get the index of the best match(es)
          #best_match <- which.min(x)
          the_min <- min(x)
          best_match <- which(x  == the_min)
          # extract the best match from geo_unique_2011
          best_names <- geo_unique_2016[best_match]
          # paste together the best names
          best_names <- paste0(best_names, collapse = ';')
        })
        # matching
        fuzzy_dict <- data_frame(name_2011 = geo_unique_2011,
                                 name_2016 = x)
        # add 2011 old
        fuzzy_dict$old_2011 <- geo_unique_2011
        
        # keep only the 2016 name and the original 2011 names
        fuzzy_dict$name_2011 <- NULL
        
        # change name of variable you want to join (2016) into "Geography"
        colnames(fuzzy_dict)[1] <- 'Geography'
        temp_data <- inner_join(fuzzy_dict, temp_data, by = 'Geography')
        
        # make old_2011 variable the new geography variable and remove geography
        temp_data$Geography <- NULL
        names(temp_data)[names(temp_data) == 'old_2011'] <- 'Geography'
        
        # now get geo code from old_2011
        # give Ontario four digit number to subset by.
        temp_data$Geography <- ifelse(grepl('Ontario', temp_data$Geography), 'Ontario', temp_data$Geography)
        
        #subset to Ontarios and 4 digit geo codes
        geo_codes <- unlist(lapply(strsplit(temp_data$Geography,
                                            '(', fixed = TRUE),
                                   function(x){
                                     gsub(')', '', x[2], fixed = TRUE)}))
        
        # (those wih NA for the geo_code are all ontario) - give it 3500 so we can subset
        # entirely by 4 digit geo_code
        temp_data$geo_code <- geo_codes
        temp_data$geo_code[is.na(temp_data$geo_code)] <- '3500'
        
      } 
      
      data_list[[i]] <- temp_data
    } else {
      data_list[[i]] <- temp_data
    }
    print(i)
  }
  
  census <- bind_rows(data_list)
  
  # clean column names
  names(census)[2:3] <- c('Age group', 'Sex')
  names(census)[5] <- c('Visible minority')
  
  # remove Total - 15 and up from age group
  census  <- census[!grepl('15 to 24 years', census$`Age group`),]
  # clean sex
  census$Sex <- ifelse(grepl('fem', tolower(census$Sex)), 'Female', 
                       ifelse(grepl('Male', census$Sex), 'Male', 
                              ifelse(grepl('Total', census$Sex), 'Total',NA)))
  # clean place of birth
  census$`Place of Birth` <- ifelse(grepl('Total', census$`Place of Birth`), 'Total - Place of birth',
                                    ifelse(grepl(' in',census$`Place of Birth`), 'Born in Canada',
                                           ifelse(grepl('out', census$`Place of Birth`), 'Born outside of Canada',
                                                  NA)))
  
  # fix multiple vismin
  census$`Visible minority` <- gsub('minorities', 'minority', census$`Visible minority`)
  census$`Visible minority` <- ifelse(census$`Visible minority` == "Total visible minority population",
                                      'All visible minorities',
                                      census$`Visible minority`)
  
  census$`Visible minority` <- ifelse(grepl('Total', census$`Visible minority`), 'Total - Population by visible minority', census$`Visible minority`)
  

  # fix geography
  geo_dictionary <- census %>% 
    dplyr::select(geo_code, Geography) %>% 
    filter(!duplicated(geo_code))
  
  # join dictionary to census
  census <- census %>% 
    dplyr::select(-Geography) %>% 
    left_join(geo_dictionary, by = 'geo_code')
  
  # remove duplicates 
  census <- census %>%
    distinct(geo_code, year, `Age group`, Sex, `Place of Birth`, `Visible minority`, .keep_all = TRUE)
  
  census <- census[, unique(c('Geography', 'geo_code', 'year', 'Age group', 'Sex', 'Place of Birth', names(census)))]
  
  # # Fix column names to make sure that denominators are correctly called "Total"
  # names(census)[names(census) == 'Population 15 years and over by Place of Residence 5 years ago'] <-
  #   paste0('Total - ', 'Population 15 years and over by Place of Residence 5 years ago')
  names(census)[names(census) == 'Population 15 years and over by labour force activity'] <-
    paste0('Total - ', 'Population 15 years and over by labour force activity')
  # names(census)[names(census) == 'Population 15 and over by hours of unpaid housework'] <- 
  #   paste0('Total - ', 'Population 15 and over by hours of unpaid housework')
  # names(census)[names(census) == 'Population 15 and over by hours of unpaid childcare'] <- 
  #   paste0('Total - ', 'Population 15 and over by hours of unpaid childcare')
  # names(census)[names(census) == 'Population 15 and over by hours of unpaid care to seniors'] <-
  #   paste0('Total - ', 'Population 15 and over by hours of unpaid care to seniors')
  census$`Population - concept not applicable` <- NULL
  census$`Population for the low income status variable` <- NULL
  census$`Standard error of average household income $` <- NULL
  
  # before removing totoal 15 and over, get the population for each census track
  census_pop <- census[ ,c('Geography', 'geo_code','year', 'Sex', 'Age group', 'Visible minority', 'Place of Birth',
                           'Total - Population 15 years and over by Place of Residence 5 years ago')]
  
  # # get totals
  # census_pop <- census_pop %>% filter(grepl('Total',`Age group`)) %>%
  #   filter(grepl('Total',`Sex`)) %>% filter(grepl('Total',`Place of Birth`)) %>%
  #   filter(grepl('Total',`Visible minority`))
  # 
  # # keep only non demo
  # census_pop <- census_pop[ c('Geography', 'geo_code','year',
  #                             'Total - Population 15 years and over by Place of Residence 5 years ago')]
  # colnames(census_pop)[4] <- 'Total population'
  # 
  # saveRDS(census_pop, 'data/census_pop.rda')
  
  # Create a new total youth only for ages 15 to 29
  census <- census %>% filter(`Age group` != 'Total - 15 years and over')
  
  
  
  # This excludes age group AND percentage variables to get the grouped sum 
  total_rows_all <- census %>% dplyr::select(-c(`Age group`, `Average household income before tax $`, contains('%'))) %>% 
    group_by(Geography, geo_code, year, Sex, `Place of Birth`, `Visible minority`) %>% summarise_all(.funs = sum) %>% 
    mutate(`Age group` = 'Total - 15 to 29 years')
  
  # this excluds age group and keeps only percentage columns and gets the mean
  total_rows_percent <- census %>% dplyr::select(-`Age group`) %>% 
    dplyr::select(c(Geography, geo_code, year, Sex, `Place of Birth`, `Visible minority`, `Average household income before tax $`,contains('%'))) %>%
    group_by(Geography, geo_code, year, Sex, `Place of Birth`, `Visible minority`) %>% summarise_all(.funs = mean) %>% 
    mutate(`Age group` = 'Total - 15 to 29 years')
  
  # column bind both datasets
  total_rows <- bind_cols(total_rows_all, total_rows_percent)
  
  # remove extra cols froom bind
  total_rows$Geography1 <- total_rows$geo_code1 <- total_rows$year1 <- total_rows$Sex1 <- total_rows$`Place of Birth1` <- 
    total_rows$`Visible minority1` <- total_rows$`Age group1` <- NULL
  
  # add back to census
  census <- bind_rows(census,
                      total_rows)
  
  # recode the only variable that has two commas 
  census$Geography <- ifelse(grepl('Dundas', census$Geography) ,
                             'Stormont Dundas and Glengarry, UC (3501)', 
                             census$Geography)
  
  # Clean up geography - only remove geo codes and keep everything else
  census$Geography <- unlist(lapply(strsplit(census$Geography, '(', fixed = TRUE), function(x){x[1]}))
  # now remove everything after comma
  census$Geography <- unlist(lapply(strsplit(census$Geography, ',', fixed = TRUE), function(x){x[1]}))
  # remove trailling and leading spaces 
  census$Geography <- trimws(census$Geography, 'both')
  
  
  # Add a population column
  census <- census %>%
    dplyr::mutate(Population = `Total - School attendance`) %>%
    dplyr::mutate(`Total - Population` = Population) 
  
  # Add an aboriginal identity column
  new_rows <- 
    census %>%
    filter(`Visible minority` %in% c('Aboriginal identity',
                                     'Non-Aboriginal identity')) %>%
    dplyr::rename(`Aboriginal identity` = `Visible minority`) %>%
    mutate(`Visible minority` = "Total - Population by visible minority_standin") 
  
  
  ordered_columns <- unique(c('Geography', 'geo_code',
                              'year', 'Age group',
                              'Sex', 'Place of Birth',
                              'Visible minority',
                              'Aboriginal identity',
                              names(new_rows)))
  
  new_rows <- new_rows[,ordered_columns]
  old_rows <- census %>%
    filter(!`Visible minority` %in% c('Aboriginal identity',
                                      'Non-Aboriginal identity')) %>%
    mutate(`Aboriginal identity` = 'Total - Population by aboriginal identity')
  old_rows <- old_rows[,ordered_columns]
  census <- bind_rows(new_rows, old_rows)
  
  # Remove the word "identity" from the `Aboriginal identity` column
  census <- census %>%
    mutate(`Aboriginal identity` = 
             gsub(' identity', '', `Aboriginal identity`))
  
  # # Change "All others" in vm to "white"
  census <-
    census %>%
    mutate(`Visible minority` = ifelse(`Visible minority` == 'Not a visible minority', 'All others', `Visible minority`))
  
 
 
  return(census)
}

# # Generate dictionary
# write_csv(data_frame(variable = names(census),
#                      category = NA,
#                      sub_category = NA),
#           'dictionaries/census_dictionary.csv')

# read in dictionary 
census_dict <- read_csv('dictionaries/census_dictionary.csv')

if('census.feather' %in% dir('data')){
  census <- read_feather('data/census.feather')
} else {
  census <- get_census_data()
  # and then save data to to "data" folder for faster retrieval in subsequent runs
  # save(census, file = 'data/census.RData')
  write_feather(census, 'data/census.feather')
}

# define input cateogry choices
category_choices <- sort(unique(census_dict$category))
category_choices <- category_choices[!category_choices %in% c('demographic', 'geo_code', 'year')]
names(category_choices) <- Hmisc::capitalize(category_choices)

head_vector <- c('Geography', 'geo_code', 'year', 'Age group', 'Sex', 'Place of Birth','Visible minority', 'Aboriginal identity', 'Total')

# Eliminate everywhere references to 15 and over
names(census) <- gsub(' 15 and over', '', names(census))
names(census) <- gsub(' 15 years and over', '', names(census))
census_dict$variable <- names(census)

census_pop <- readRDS('data/census_pop.rda')

# recode the only variable that has two commas 
census_pop$Geography <- ifelse(grepl('Dundas', census_pop$Geography) ,
                           'Stormont Dundas and Glengarry, UC (3501)', 
                           census_pop$Geography)

# Clean up geography - only remove geo codes and keep everything else
census_pop$Geography <- unlist(lapply(strsplit(census_pop$Geography, '(', fixed = TRUE), function(x){x[1]}))
# now remove everything after comma
census_pop$Geography <- unlist(lapply(strsplit(census_pop$Geography, ',', fixed = TRUE), function(x){x[1]}))
# remove trailling and leading spaces 
census_pop$Geography <- trimws(census_pop$Geography, 'both')

# Pre-make a leaflet object
leafy <- leaf_basic(shp = ont2, tile = 'OpenStreetMap', palette = 'Oranges')
